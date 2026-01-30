# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(magrittr)
library(tidyverse)
library(purrr)
library(data.table)
library(tidyverse)
library(svglite)
library(VennDiagram)
library(grid)
library(gridExtra)
library(ggtext)

# Specify paths ----------------------------------------------------------------
print('Specify paths')

# NOTE:
# This file is used to specify paths and is in the .gitignore to keep your information secret.
# A file called specify_paths_example.R is provided for you to fill in.
# Please remove "_example" from the file name and add your specific file paths before running this script.

source("analysis/specify_paths.R")

# Make post-release directory --------------------------------------------------
print('Make post-release directory')

dir.create("output/post_release/", recursive = TRUE, showWarnings = FALSE)
output_folder <- "output/post_release"

# Create output folder for plots
plot_dir <- file.path(output_folder, "/forest_plots")
dir.create(plot_dir, showWarnings = FALSE)

# Add plot labels ---------------------------------------------------------
print("Add plot labels")

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

# Define group order for plotting
group_order <- c(
    "List size",
    "Practice region",
    "Age",
    "Sex",
    "Ethnicity",
    "Deprivation",
    "Rurality",
    "Smoking Status",
    "Obesity",
    "Care home residence",
    "Monthly consultation"
)

# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main
plot_irr <- function(regression, outcome_name) {
    # Load data --------------------------------------------------------------------
    print("Load model output")

    df <- readr::read_csv(
        "output/post_release/plot_model_output.csv",
        show_col_types = FALSE
    )

    # Filter data ------------------------------------------------------------------
    print("Filter data")

    df <- df %>%
        filter(
            outcome == outcome_name,
            model_type == regression,
            term == "exp_prop",
            model %in% c("mdl_crude", "mdl_age_sex")
        ) %>%
        select(
            cohort,
            exposure,
            outcome,
            model,
            irr,
            lci,
            uci,
            n_obs_midpoint6
        ) %>%
        mutate(
            # model aesthetics
            model = factor(
                model,
                levels = c("mdl_crude", "mdl_age_sex"),
                labels = c("Crude", "Age–sex adjusted")
            ),
            cohort = factor(
                cohort,
                levels = c(
                    "precovid",
                    "postcovid1",
                    "postcovid2",
                    "postcovid3"
                )
            ),
            exposure = factor(exposure) # will control y-axis order later
        )

    # --- Join EXPOSURE labels ---
    exposure_labels <- labels %>%
        filter(!str_detect(term, "apc|ec")) %>%
        select(term, exposure_label = label, group, ref)

    df <- df %>%
        left_join(
            exposure_labels,
            by = c("exposure" = "term")
        )

    # --- Join OUTCOME label ---
    outcome_label <- labels %>%
        filter(term == outcome_name) %>%
        pull(label)

    # --- Join COHORT labels ---
    cohort_labels <- labels %>%
        filter(
            term %in% c("precovid", "postcovid1", "postcovid2", "postcovid3")
        ) %>%
        select(term, label)

    df <- df %>%
        left_join(
            cohort_labels,
            by = c("cohort" = "term")
        ) %>%
        rename(cohort_label = label)

    # --- Factor setup ---
    df <- df %>%
        mutate(
            cohort_label = factor(
                cohort_label,
                levels = cohort_labels$label
            )
        ) %>%
        mutate(
            group = factor(
                group,
                levels = group_order
            ),
            ref_order = if_else(is.na(ref), Inf, ref)
        ) %>%
        arrange(group, ref_order)

    df <- df %>%
        mutate(
            exposure_label = factor(
                exposure_label,
                levels = unique(exposure_label)
            )
        )
    df <- df %>%
        mutate(
            exposure_label_full = if_else(
                !is.na(ref),
                paste0(group, ": ", exposure_label),
                exposure_label
            )
        )

    df <- df %>%
        mutate(
            exposure_label_full = factor(
                exposure_label_full,
                levels = unique(exposure_label_full)
            )
        )

    # Make forest plot -----------------------------------------------------------
    print("Make forest plot")

    title_text <- paste0(
        "General practice characteristics and ",
        "**",
        tolower(outcome_label),
        "**"
    )

    is_acsc <- str_detect(outcome_name, "acsc")

    x_limits <- if (is_acsc) {
        c(0.8, 1.15)
    } else {
        c(0.9, 1.15)
    }

    x_breaks <- if (is_acsc) {
        c(0.8, 0.9, 1.0, 1.1)
    } else {
        c(0.9, 1.0, 1.1)
    }

    caption_text <- str_wrap(
        paste0(
            "Points show incidence rate ratios (IRRs) with 95% confidence intervals. ",
            "Estimates from random-intercept ",
            ifelse(regression == "negbin", "negative binomial", "Poisson"),
            " regression models."
        ),
        width = 500
    )

    p <- ggplot(
        df,
        aes(
            x = irr,
            y = exposure_label_full,
            colour = cohort_label,
            linetype = model,
            group = interaction(cohort_label, model)
        )
    ) +
        geom_vline(
            xintercept = 1,
            colour = "grey60",
            linetype = "dashed",
            linewidth = 0.6
        ) +
        geom_errorbarh(
            aes(xmin = lci, xmax = uci),
            position = position_dodge(width = 0.7),
            height = 0.2,
            linewidth = 0.7
        ) +
        geom_point(
            position = position_dodge(width = 0.7),
            size = 1.6
        ) +
        scale_x_log10(
            breaks = x_breaks,
            labels = scales::number_format(accuracy = 0.01)
        ) +
        coord_cartesian(xlim = x_limits) +
        labs(
            title = title_text,
            x = "Incidence rate ratio (IRR)",
            y = NULL,
            colour = "",
            linetype = "Model",
            caption = caption_text
        ) +
        theme_bw() +
        theme(
            plot.title = ggtext::element_markdown(
                hjust = 0,
                size = 12,
                margin = margin(b = 8),
                face = "plain"
            ),
            plot.caption = element_text(
                size = 8,
                hjust = 0,
                colour = "grey30",
                lineheight = 1.2,
                margin = margin(t = 8)
            ),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 9),
            plot.margin = margin(t = 14, r = 10, b = 14, l = 10),
            legend.box = "vertical",
            legend.key.width = unit(12, "pt"),
            legend.key.height = unit(10, "pt"),
            legend.spacing.x = unit(4, "pt"),
            legend.spacing.y = unit(2, "pt"),
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 9)
        ) +
        guides(
            colour = guide_legend(
                title = "",
                nrow = 2,
                byrow = TRUE,
                override.aes = list(size = 2)
            ),
            linetype = guide_legend(
                title = "Model",
                override.aes = list(size = 0.8)
            )
        )
    ggsave(
        filename = file.path(
            plot_dir,
            paste0("forest_", outcome_name, "_", regression, ".png")
        ),
        plot = p,
        width = 10,
        height = 9
    )
}

# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main

plot_irr("negbin", "apc_main")
plot_irr("poisson", "apc_main")
plot_irr("negbin", "ec_main")
plot_irr("poisson", "ec_main")
plot_irr("negbin", "apc_unpl_main")
plot_irr("poisson", "apc_unpl_main")

plot_irr("negbin", "apc_acsc_any_main")
plot_irr("poisson", "apc_acsc_any_main")
plot_irr("negbin", "ec_acsc_any_main")
plot_irr("poisson", "ec_acsc_any_main")
