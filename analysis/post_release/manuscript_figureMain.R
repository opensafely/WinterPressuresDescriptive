# Load libraries ---------------------------------------------------------------
print("Load libraries")

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
print("Specify paths")

# NOTE:
# This file is used to specify paths and is in the .gitignore to keep your information secret.
# A file called specify_paths_example.R is provided for you to fill in.
# Please remove "_example" from the file name and add your specific file paths before running this script.

source("analysis/specify_paths.R")

# Make post-release directory --------------------------------------------------
print("Make post-release directory")

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
plot_irr <- function(regression, sub_group, outcome_name) {
    # Load data --------------------------------------------------------------------
    print("Load model output")

    df <- readr::read_csv(
        "output/post_release/plot_model_output.csv",
        show_col_types = FALSE
    )

    df <- df %>% mutate(
        outcome = str_remove(outcome, paste0("_", analysis))
    )

    # Filter data ------------------------------------------------------------------
    print("Filter data")

    df <- df %>%
        filter(
            analysis == sub_group,
            outcome == outcome_name,
            model_type == regression,
            term == "exp_prop",
            model %in% c("mdl_crude", "mdl_age_sex")
        ) %>%
        select(
            cohort,
            analysis,
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

    # --- Join analysis labels ---
    analysis_label <- labels %>%
        filter(
            term %in% c(sub_group)
        ) %>%
        pull(label)

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
        "**",
        " in ",
        "**",
        tolower(analysis_label),
        "**"
    )

    is_acsc <- str_detect(outcome_name, "acsc")

    x_limits <- if (is_acsc) {
        c(0.7, 1.4)
    } else {
        c(0.8, 1.3)
    }

    x_breaks <- if (is_acsc) {
        c(0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4)
    } else {
        c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3)
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
            alpha = model,
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
            aes(xmin = lci, xmax = uci, alpha = model),
            position = position_dodge(width = 0.5),
            height = 0.2,
            linewidth = 0.7
        ) +
        geom_point(
            position = position_dodge(width = 0.5),
            size = 2.5
        ) +
        # --- Scales ---
        scale_alpha_manual(
            values = c("Crude" = 0.35, "Age-sex adjusted" = 1),
            name = "Model"
        ) +
        scale_size_manual(
            values = c("Crude" = 1.6, "Age-sex adjusted" = 2.2),
            name = "Model"
        ) +
        scale_x_log10(
            limits = x_limits,
            breaks = x_breaks,
            labels = scales::number_format(accuracy = 0.01)
        ) +
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
                margin = margin(b = 8)
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
                nrow = 1,
                byrow = TRUE,
                override.aes = list(size = 2, alpha = 1)
            ),
            alpha = guide_legend(
                title = "Model",
                override.aes = list(colour = "black")
            ),
            size = "none" # hide duplicate legend
        )
    ggsave(
        filename = file.path(
            plot_dir,
            paste0("forest_", sub_group, "_", regression, "_", outcome_name, ".png")
        ),
        plot = p,
        width = 10,
        height = 9
    )
}

# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main

plot_irr("negbin", "main", "apc")
plot_irr("poisson", "main", "apc")
plot_irr("negbin", "main", "ec")
plot_irr("poisson", "main", "ec")
plot_irr("negbin", "main", "apc_unpl")
plot_irr("poisson", "main", "apc_unpl")

plot_irr("negbin", "main", "apc_acsc_any")
plot_irr("poisson", "main", "apc_acsc_any")
plot_irr("negbin", "main", "ec_acsc_any")
plot_irr("poisson", "main", "ec_acsc_any")

plot_irr("negbin", "sub_asth", "apc")
plot_irr("poisson", "sub_asth", "apc")
plot_irr("negbin", "sub_asth", "ec")
plot_irr("poisson", "sub_asth", "ec")
plot_irr("negbin", "sub_asth", "apc_unpl")
plot_irr("poisson", "sub_asth", "apc_unpl")

plot_irr("negbin", "sub_asth", "apc_acsc_any")
plot_irr("poisson", "sub_asth", "apc_acsc_any")
plot_irr("negbin", "sub_asth", "ec_acsc_any")
plot_irr("poisson", "sub_asth", "ec_acsc_any")

plot_irr("negbin", "sub_copd", "apc")
plot_irr("poisson", "sub_copd", "apc")
plot_irr("negbin", "sub_copd", "ec")
plot_irr("poisson", "sub_copd", "ec")
plot_irr("negbin", "sub_copd", "apc_unpl")
plot_irr("poisson", "sub_copd", "apc_unpl")

plot_irr("negbin", "sub_copd", "apc_acsc_any")
plot_irr("poisson", "sub_copd", "apc_acsc_any")
plot_irr("negbin", "sub_copd", "ec_acsc_any")
plot_irr("poisson", "sub_copd", "ec_acsc_any")

plot_irr("negbin", "sub_diab", "apc")
plot_irr("poisson", "sub_diab", "apc")
plot_irr("negbin", "sub_diab", "ec")
plot_irr("poisson", "sub_diab", "ec")
plot_irr("negbin", "sub_diab", "apc_unpl")
plot_irr("poisson", "sub_diab", "apc_unpl")

plot_irr("negbin", "sub_diab", "apc_acsc_any")
plot_irr("poisson", "sub_diab", "apc_acsc_any")
plot_irr("negbin", "sub_diab", "ec_acsc_any")
plot_irr("poisson", "sub_diab", "ec_acsc_any")

plot_irr("negbin", "sub_htn", "apc")
plot_irr("poisson", "sub_htn", "apc")
plot_irr("negbin", "sub_htn", "ec")
plot_irr("poisson", "sub_htn", "ec")
plot_irr("negbin", "sub_htn", "apc_unpl")
plot_irr("poisson", "sub_htn", "apc_unpl")

plot_irr("negbin", "sub_htn", "apc_acsc_any")
plot_irr("poisson", "sub_htn", "apc_acsc_any")
plot_irr("negbin", "sub_htn", "ec_acsc_any")
plot_irr("poisson", "sub_htn", "ec_acsc_any")

plot_irr("negbin", "sub_sevmh", "apc")
plot_irr("poisson", "sub_sevmh", "apc")
plot_irr("negbin", "sub_sevmh", "ec")
plot_irr("poisson", "sub_sevmh", "ec")
plot_irr("negbin", "sub_sevmh", "apc_unpl")
plot_irr("poisson", "sub_sevmh", "apc_unpl")

plot_irr("negbin", "sub_sevmh", "apc_acsc_any")
plot_irr("poisson", "sub_sevmh", "apc_acsc_any")
plot_irr("negbin", "sub_sevmh", "ec_acsc_any")
plot_irr("poisson", "sub_sevmh", "ec_acsc_any")
