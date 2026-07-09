# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(forestploter)
library(dplyr)
library(tidyr)
library(stringr)

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

regression <- "negbin"
sub_group <- "main"
outcome_names <- c("apc", "apc_unpl")
cohorts <- c("precovid", "postcovid3")
practice_char = TRUE
# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main
plot_irr <- function(regression, sub_group, outcome_names, cohorts, practice_char = TRUE) {
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
            irr_display,
            n_obs_midpoint6,
            mad
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
    outcome_group <- unique(df$outcome_group)

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
    
    forest_table <- df %>%
    select(
        group,
        exposure_label,
        cohort_label,
        mad,
        outcome,
        irr,
        lci,
        uci,
        irr_display
    ) %>%
    pivot_wider(
        names_from = outcome,
        values_from = c(
            irr,
            lci,
            uci,
            irr_display
        )
    )
    
    # Select the characteristics to be included in the forest plot
    if (practice_char) {
        forest_table <- forest_table %>%
        filter(group %in% practice_groups)
    } else {

        forest_table <- forest_table %>%
            filter(group %in% case_mix_groups)
    }

    value_types <- c(
        "irr_display",
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
        group,
        exposure_label,
        cohort_label,
        mad,
        all_of(value_cols)
    )

    # Forest estimates
    est <- lapply(outcome_names, \(x) forest_table[[paste0("irr_", x)]])
    lower <- lapply(outcome_names, \(x) forest_table[[paste0("lci_", x)]])
    upper <- lapply(outcome_names, \(x) forest_table[[paste0("uci_", x)]])

    #Header for the forest plot
    outcome_headers <- labels %>%
    filter(term %in% outcome_names) %>%
    arrange(match(term, outcome_names))


    #Build display table for the forest plot
    forest_table <- forest_table %>%
        group_by(group, exposure_label) %>%
        mutate(
            Characteristics = case_when(
                row_number() == 1 &
                    group %in% nested_groups ~
                    paste0("    ", exposure_label),

                row_number() == 1 ~
                    exposure_label,

                TRUE ~
                    ""
            )
        ) %>%
        ungroup()
    
    # Blank repeated group and exposure labels for the forest plot
    forest_table <- forest_table %>%
    group_by(group, exposure_label) %>%
    mutate(
        exposure_label = if_else(
            row_number() == 1,
            exposure_label,
            ""
        )
    ) %>%
    ungroup()

    forest_table <- forest_table %>%
    group_by(group) %>%
    mutate(
        group = if_else(
            row_number() == 1,
            as.character(group),
            ""
        )
    ) %>%
    ungroup()





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