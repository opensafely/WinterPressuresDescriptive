# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(forestploter)
library(dplyr)
library(tidyr)
library(stringr)
library(grid)

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

nested_groups <- c(
    "Practice region",
    "Age",
    "Sex",
    "Ethnicity",
    "Deprivation",
    "Smoking Status"
)

# regression <- "negbin"
# sub_group <- "main"
# outcome_names <- c("apc", "apc_unpl")
# cohorts <- c("precovid", "postcovid3")
# practice_char <- "all"
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
            ),
            irr_display = sprintf("%.2f (%.2f–%.2f)", irr, lci, uci)
        ) %>%
        select(
            cohort,
            analysis,
            exposure,
            outcome,
            irr,
            lci,
            uci,
            IRR = irr_display,
            n_obs_midpoint6,
            MAD = mad
        ) %>%
        mutate(
            cohort = factor(
                cohort,
                levels = c(
                    "precovid",
                    "postcovid1",
                    "postcovid2",
                    "postcovid3"
                )
            )
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
        arrange(group, ref_order, cohort_label)

    # Create the Characteristics column for the forest plot
    df <- df %>%
        mutate(
            characteristics = if_else(
                group %in% practice_groups,
                "Practice characteristics",
                "Patient case-mix"
            )
        )

    forest_table <- df %>%
        select(
            characteristics,
            group,
            sub_characteristics = exposure_label,
            cohort_label,
            MAD,
            outcome,
            irr,
            lci,
            uci,
            IRR
        ) %>%
        pivot_wider(
            names_from = outcome,
            values_from = c(
                irr,
                lci,
                uci,
                IRR
            )
        )

    # Select the characteristics to be included in the forest plot
    practice_char <- match.arg(
        practice_char,
        c("all", "practice", "case_mix")
    )

    if (practice_char == "practice") {
        forest_table <- forest_table %>%
            filter(group %in% practice_groups)
    } else if (practice_char == "case_mix") {
        forest_table <- forest_table %>%
            filter(group %in% case_mix_groups)
    }

    value_types <- c(
        "IRR",
        "irr",
        "lci",
        "uci"
    )

    value_cols <- unlist(
        lapply(outcome_names, function(outcome) {
            paste0(value_types, "_", outcome)
        })
    )

    forest_table <- forest_table %>%
        select(
            characteristics,
            group,
            sub_characteristics,
            cohort_label,
            MAD,
            all_of(value_cols)
        )

    # Forest estimates
    est <- lapply(outcome_names, \(x) forest_table[[paste0("irr_", x)]])
    lower <- lapply(outcome_names, \(x) forest_table[[paste0("lci_", x)]])
    upper <- lapply(outcome_names, \(x) forest_table[[paste0("uci_", x)]])

    # Header for the forest plot
    outcome_headers <- labels %>%
        filter(term %in% outcome_names) %>%
        arrange(match(term, outcome_names))


    # Build display table for the forest plot
    forest_table <- forest_table %>%
        group_by(characteristics, group, sub_characteristics) %>%
        mutate(
            Levels = case_when(
                row_number() == 1 &
                    group %in% nested_groups ~
                    paste0("\u00A0\u00A0", sub_characteristics),
                row_number() == 1 ~
                    sub_characteristics,
                TRUE ~
                    ""
            )
        ) %>%
        ungroup()

    # Blank repeated characteristics, group and sub_characteristics for the forest plot
    forest_table <- forest_table %>%
        group_by(characteristics, group) %>%
        mutate(
            Characteristic = if_else(
                row_number() == 1,
                as.character(group),
                ""
            )
        ) %>%
        ungroup()

    forest_table <- forest_table %>%
        group_by(characteristics) %>%
        mutate(
            Characteristics = if_else(
                row_number() == 1,
                as.character(characteristics),
                ""
            ),
            Cohort = cohort_label
        ) %>%
        ungroup()

    # Add placeholder columns for forest
    plot_cols <- paste0("forest", seq_along(outcome_names))

    forest_table[plot_cols] <- strrep(" ", 40)

    # Reorder columns for the forest plot
    fixed_cols <- c(
        "Levels",
        "Cohort",
        "MAD"
    )

    display_cols <- unlist(
        lapply(seq_along(outcome_names), function(i) {
            c(
                plot_cols[i],
                paste0("IRR_", outcome_names[i])
            )
        })
    )

    forest_table <- forest_table %>%
        select(
            all_of(fixed_cols),
            all_of(display_cols)
        )

    ci_column <- seq(
        from = length(fixed_cols) + 1,
        by = 2,
        length.out = length(outcome_names)
    )

    height <- max(2400, nrow(forest_table) * 45)

    p <- forest(
        data = forest_table,
        est = est,
        lower = lower,
        upper = upper,
        ci_column = ci_column,
        ref_line = 1,
        x_trans = "log",
        xlim = c(0.9, 1.2),
        ticks_at = c(
            0.9,
            1,
            1.1,
            1.2
        ),
        theme = forest_theme(
            base_size = 7,

            # Confidence intervals
            ci_pch = 15,
            ci_col = "#1F78B4",
            ci_fill = "#1F78B4",
            ci_lwd = 1.2,
            ci_cex = 0.25,
            ci_Theight = 0,

            # Reference line
            refline_col = "grey60",
            refline_lwd = 1,

            # X-axis
            xaxis_gp = grid::gpar(
                fontsize = 7
            ),

            # Table text
            core = list(
                fg_params = list(
                    hjust = 0,
                    x = 0.02,
                    fontsize = 7
                ),
                padding = unit(c(2.5, 3), "mm")
            ),

            # Header
            colhead = list(
                fg_params = list(
                    fontface = "bold",
                    fontsize = 8,
                    hjust = 0,
                    x = 0.02
                ),
                padding = unit(c(3, 3), "mm")
            ),

            # Footnote
            footnote_gp = grid::gpar(
                fontsize = 7
            )
        )
    )
    # Save forest plot
    png(
        filename = file.path(
            plot_dir,
            paste0(
                "vforest-",
                sub_group, "-",
                regression, "-",
                practice_char, "-",
                outcome_group, "-",
                cohort_suffix,
                ".png"
            )
        ),
        width = 4000,
        height = height,
        res = 300
    )

    grid::grid.newpage()
    grid::grid.draw(p)

    dev.off()
}

plot_irr(
    regression = "negbin",
    sub_group = "main",
    outcome_names = c("ec", "ec_acsc_any"),
    cohorts = c("postcovid3"),
    practice_char = "all"
)

plot_irr(
  regression = "negbin",
  sub_group = "main",
  outcome_names = c("apc", "apc_unpl", "apc_acsc_any", "apc_unpl_acsc_any"),
  cohorts = c("precovid","postcovid3"),
  practice_char = "all"
)
