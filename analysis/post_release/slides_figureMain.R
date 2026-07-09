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
library(patchwork)

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
    "Practice region",
    "List size",
    "Monthly consultation",
    "Age",
    "Sex",
    "Ethnicity",
    "Deprivation",
    "Rurality",
    "Smoking Status",
    "Obesity",
    "Care home residence"
)

practice_groups <- c(
    "Practice region",
    "List size",
    "Monthly consultation"
)

case_mix_groups <- setdiff(
    group_order,
    practice_groups
)

# regression <- "negbin"
# sub_group <- "main"
# outcome_names <- c("apc", "ec")
# cohorts <- c("precovid", "postcovid1", "postcovid2", "postcovid3")
# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main
plot_irr <- function(regression, sub_group, outcome_names, cohorts, practice_char) {
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
            cohort %in% cohorts,
            analysis == sub_group,
            outcome %in% outcome_names,
            model_type == regression,
            grepl("^exp_prop(_|$)", term),
            model %in% c("mdl_age_sex")
        ) %>%
        mutate(
            exposure = if_else(
                term == "exp_prop", exposure, term
            )
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
            n_obs_midpoint6,
            mad
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
        ) %>%
        filter(!is.na(exposure_label))

    # --- Join OUTCOME label ---
    outcome_labels <- labels %>%
        select(term, outcome_label = label, outcome_group = group)

    df <- df %>%
        left_join(
            outcome_labels,
            by = c("outcome" = "term")
        )
    outcome_group <- unique(df$outcome_group)[1]

    # --- Join COHORT labels ---
    cohort_labels <- labels %>%
        filter(term %in% cohorts) %>%
        select(term, label)

    df <- df %>%
        left_join(
            cohort_labels,
            by = c("cohort" = "term")
        ) %>%
        rename(cohort_label = label)

    cohort_suffix <- if (identical(
        cohorts,
        c("precovid", "postcovid1", "postcovid2", "postcovid3")
    )) {
        "all"
    } else if (identical(
        cohorts,
        c("precovid", "postcovid3")
    )) {
        "prepost3"
    } else {
        paste(gsub("postcovid", "post", cohorts), collapse = "_")
    }

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

    table_df <- df %>%
        select(
            cohort,
            cohort_label,
            exposure_label,
            group,
            ref,
            mad
        ) %>%
        distinct() %>%
        arrange(cohort_label, group, ref) %>%
        select(
            cohort_label,
            exposure_label,
            mad
        )
    table_df_wide <- table_df %>%
        pivot_wider(
            names_from = cohort_label,
            values_from = mad
        )

    practice_header <- table_df_wide[1, ]
    practice_header[, ] <- NA
    practice_header$exposure_label <- "Practice"

    casemix_header <- table_df_wide[1, ]
    casemix_header[, ] <- NA
    casemix_header$exposure_label <- "Patient case-mix (% of patients in practice)"

    table_side <- bind_rows(
        practice_header,
        table_df_wide[1:11, ],
        casemix_header,
        table_df_wide[12:nrow(table_df_wide), ]
    )

    # set a flag for the top header row to be bolded in the table
    table_side <- table_side %>%
        mutate(
            is_header = if_else(
                exposure_label %in% c("Practice", "Patient case-mix (% of patients in practice)"),
                TRUE,
                FALSE
            )
        )

    cohort_cols <- names(table_side)[
        !(names(table_side) %in% c("exposure_label", "is_header"))
    ]

    n_cohorts <- length(cohort_cols)

    if (n_cohorts == 4) {
        widths <- c(0.65, 0.35)
    } else {
        widths <- c(0.70, 0.30)
    }

    row_levels <- rev(table_side$exposure_label)

    table_side <- table_side %>%
        mutate(
            exposure_label = factor(
                exposure_label,
                levels = row_levels
            )
        )

    # Calculate median MAD for each exposure across cohorts and create a new label for the exposure label that includes the median MAD value in parentheses, unless the row is a header or the median MAD is NaN

    table_side <- table_side %>%
        rowwise() %>%
        mutate(
            mad_median = median(
                c_across(all_of(cohort_cols)),
                na.rm = TRUE
            ),
            exposure_label_full = case_when(
                is_header ~ exposure_label,
                is.na(mad_median) ~ exposure_label,
                mad_median <= 100 ~ sprintf(
                    "%s (%.1f%%)",
                    exposure_label,
                    mad_median
                ),
                TRUE ~ sprintf(
                    "%s (%.0f)",
                    exposure_label,
                    mad_median
                )
            )
        ) %>%
        ungroup()

    # Set the y-axis labels for the forest plot to be the exposure_label_full values, with names corresponding to the exposure_label values
    y_labels <- table_side$exposure_label_full
    names(y_labels) <- table_side$exposure_label

    # Set the y-axis labels for the forest plot to be bold for the header rows
    y_labels[y_labels == "Practice"] <-
        "<b>Practice</b>"

    y_labels[y_labels == "Patient case-mix (% of patients in practice)"] <-
        "<b>Patient case-mix (% of patients in practice)</b>"

    # Add dummy rows to ensure that the table and forest plot have the same number of rows
    dummy_rows <- df %>%
        distinct(
            cohort,
            analysis,
            outcome,
            model,
            outcome_label,
            outcome_group,
            cohort_label
        ) %>%
        slice(rep(1:n(), each = 2)) %>%
        mutate(
            exposure = NA_character_,
            exposure_label = rep(
                c(
                    "Practice",
                    "Patient case-mix (% of patients in practice)"
                ),
                times = n() / 2
            ),
            group = NA_character_,
            ref = NA_real_,
            ref_order = NA_real_,
            irr = NA_real_,
            lci = NA_real_,
            uci = NA_real_,
            n_obs_midpoint6 = NA_real_,
            mad = NA_real_
        )

    df <- bind_rows(df, dummy_rows)

    # set the order of the exposure_label factor to match the order in the table
    df <- df %>%
        mutate(
            exposure_label = factor(
                exposure_label,
                levels = row_levels
            )
        )
    
    # Select the characteristics to be included in the forest plot
    practice_char <- match.arg(
        practice_char,
        c("all", "practice", "case_mix")
    )

    if (practice_char == "practice") {
        df <- df %>%
            filter(group %in% practice_groups)
    } else if (practice_char == "case_mix") {
        df <- df %>%
            filter(group %in% case_mix_groups)
    }

    # Make forest plot -----------------------------------------------------------
    print("Make forest plot")

    title_text <- paste0(
        "General practice characteristics and ",
        "**",
        tolower(outcome_group),
        "**",
        " in ",
        "**",
        tolower(analysis_label),
        "**"
    )

    is_acsc <- any(str_detect(outcome_names, "acsc"))

    x_limits <- if (is_acsc) {
        c(0.9, 1.3)
    } else {
        c(0.9, 1.3)
    }

    x_breaks <- if (is_acsc) {
        c(0.9, 1.0, 1.1, 1.2, 1.3)
    } else {
        c(0.9, 1.0, 1.1, 1.2, 1.3)
    }

    # Clip confidence intervals to plotting range
    df <- df %>%
        mutate(
            lci_plot = pmax(lci, x_limits[1]),
            uci_plot = pmin(uci, x_limits[2])
        )

    caption_text <- str_wrap(
        paste0(
            "Points show incidence rate ratios (IRRs) with 95% confidence intervals. ",
            "Estimates from random-intercept ",
            ifelse(regression == "negbin", "negative binomial", "Poisson"),
            " regression models.",
            "\n\n",
            " MAD: median absolute deviation. The median MAD across cohorts is shown in parentheses for each continuous characteristic, representing a one-unit increase in the standardised exposure."
        ),
        width = 250
    )

    p <- ggplot(
        df,
        aes(
            x = irr,
            y = exposure_label,
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
            aes(xmin = lci_plot, xmax = uci_plot, alpha = model),
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
        scale_colour_manual(
            values = c(
                "Pre-COVID19" = "#F8766D",
                "2022/23" = "#7CAE00",
                "2023/24" = "#00BFC4",
                "2024/25" = "#C77CFF"
            ),
            drop = FALSE
        ) +
        scale_y_discrete(
            labels = y_labels,
            drop = FALSE
        ) +
        scale_size_manual(
            values = c("Crude" = 1.6, "Age-sex adjusted" = 2.2),
            name = "Model"
        ) +
        scale_x_log10(
            breaks = x_breaks,
            labels = scales::number_format(accuracy = 0.01)
        ) +
        coord_cartesian(
            xlim = x_limits
        ) +
        facet_grid(
            ~outcome_label
        ) +
        labs(
            title = title_text,
            x = "Incidence rate ratio (IRR)",
            y = "Characteristics (median MAD across cohorts)",
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
            axis.text.y = ggtext::element_markdown(size = 9),
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
            paste0("forest_", sub_group, "_", regression, "_", paste(outcome_names, collapse = "_"), "_", cohort_suffix, ".png")
        ),
        plot = p,
        width = 16,
        height = 9,
        dpi = 300
    )
}

plot_irr("negbin", "main", c("apc","apc_unpl", "apc_acsc_any", "apc_unpl_acsc_any"), c("precovid", "postcovid3"))
# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main

# Hospital use
plot_irr("negbin", "main", c("apc", "ec"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))
plot_irr("poisson", "main", c("apc", "ec"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))

plot_irr("negbin", "main", c("apc", "ec"), c("precovid", "postcovid3"))
plot_irr("poisson", "main", c("apc", "ec"), c("precovid", "postcovid3"))

plot_irr("negbin", "main", c("apc","apc_unpl", "apc_acsc_any", "apc_unpl_acsc_any"), c("precovid", "postcovid3"))
plot_irr("poisson", "main", c("apc_unpl", "apc_plan"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))

plot_irr("negbin", "main", c("apc_unpl", "apc_plan"), c("precovid", "postcovid3"))
plot_irr("poisson", "main", c("apc_unpl", "apc_plan"), c("precovid", "postcovid3"))

# ACSC-related hospital use
plot_irr("negbin", "main", c("apc_acsc_any", "ec_acsc_any"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))
plot_irr("poisson", "main", c("apc_acsc_any", "ec_acsc_any"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))

plot_irr("negbin", "main", c("apc_acsc_any", "ec_acsc_any"), c("precovid", "postcovid3"))
plot_irr("poisson", "main", c("apc_acsc_any", "ec_acsc_any"), c("precovid", "postcovid3"))

plot_irr("negbin", "main", c("apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))
plot_irr("poisson", "main", c("apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid1", "postcovid2", "postcovid3"))

plot_irr("negbin", "main", c("apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"))
plot_irr("poisson", "main", c("apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"))

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
