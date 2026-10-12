# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(tidyverse)
library(grid)


# Specify paths ----------------------------------------------------------------
print("Specify paths")

source("analysis/specify_paths.R")


# Specify plotting options -----------------------------------------------------
print("Specify plotting options")

cohort_1 <- "precovid"
cohort_2 <- "postcovid3" # Change to NULL for a single-cohort figure.

# Patient-group order must match the figure legend.
subgroups_to_plot <- c(
    "main",
    "sub_asth",
    "sub_copd",
    "sub_diab",
    "sub_htn",
    "sub_sevmh"
)

# Figure legend:
# (A) All people
# (B) Patients living with asthma
# (C) Patients living with COPD
# (D) Patients living with diabetes
# (E) Patients living with hypertension
# (F) Patients living with severe mental illness

outcomes_all_cause <- c(
    "apc",
    "apc_unpl",
    "apc_plan",
    "ec"
)

outcomes_acsc <- c(
    "apc_acsc_any",
    "apc_unpl_acsc_any",
    "apc_plan_acsc_any",
    "ec_acsc_any"
)

perpeople <- 1000

# Distance between y-axis labels and horizontal grid lines.
y_break_interval <- 5

output_dir <- "output/post_release/iqr_plots_outcomes"

dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
)

selected_cohorts <- c(cohort_1, cohort_2)

if (length(selected_cohorts) < 1L ||
    length(selected_cohorts) > 2L ||
    anyNA(selected_cohorts) ||
    anyDuplicated(selected_cohorts)) {
    stop("Choose one cohort, or two different cohorts.")
}

cohort_colours <- c(
    precovid = "#F8766D",
    postcovid1 = "#7CAE00",
    postcovid2 = "#00BFC4",
    postcovid3 = "#C77CFF"
)

if (!all(selected_cohorts %in% names(cohort_colours))) {
    stop("Add the selected cohort(s) to cohort_colours.")
}


# Read input -------------------------------------------------------------------
print("Read input")

input <- readr::read_csv(
    file.path(graphs, "input_trajectory_outcomes.csv"),
    show_col_types = FALSE
)

required <- c(
    "cohort", "outcome", "week_number", "q1", "median", "q3"
)

missing_columns <- setdiff(required, names(input))

if (length(missing_columns)) {
    stop(
        "Missing input column(s): ",
        paste(missing_columns, collapse = ", "),
        ". The summary input needs q1, median and q3 of the ",
        "practice-level weekly rates."
    )
}


# Read labels ------------------------------------------------------------------
print("Read labels")

labels <- readr::read_csv(
    "lib/labels.csv",
    show_col_types = FALSE
)

outcome_labels <- labels %>%
    transmute(
        outcome = term,
        outcome_label = label
    ) %>%
    distinct(outcome, .keep_all = TRUE)

cohort_labels <- labels %>%
    filter(term %in% names(cohort_colours)) %>%
    transmute(
        cohort = term,
        cohort_label = label
    ) %>%
    distinct(cohort, .keep_all = TRUE)


# Prepare plotting data --------------------------------------------------------
print("Prepare plotting data")

trajectory <- input %>%
    mutate(
        subgroup = coalesce(
            str_extract(outcome, "(main|sub_[a-z]+)$"),
            "main"
        ),
        outcome = str_remove(
            outcome,
            "_(main|sub_[a-z]+)$"
        )
    ) %>%
    left_join(outcome_labels, by = "outcome") %>%
    left_join(cohort_labels, by = "cohort") %>%
    mutate(
        outcome_label = coalesce(outcome_label, outcome),
        cohort_label = coalesce(cohort_label, cohort),
        week_number = as.integer(week_number),
        plot_date = as.Date("2000-10-01") +
            7L * (week_number - 1L),
        across(c(q1, median, q3), ~ .x * perpeople)
    )


# Create plotting function -----------------------------------------------------
print("Create plotting function")

create_and_save_iqr_plot <- function(
  trajectory,
  subgroups,
  cohorts,
  outcomes,
  figure_name,
  plots_dir
) {
    plot_data <- trajectory %>%
        filter(
            subgroup %in% subgroups,
            cohort %in% cohorts,
            outcome %in% outcomes
        )

    if (!nrow(plot_data)) {
        stop("No matching data for ", figure_name, ".")
    }

    # Check expected combinations, excluding pre-COVID emergency care.
    expected_panels <- tidyr::expand_grid(
        subgroup = subgroups,
        cohort = cohorts,
        outcome = outcomes
    ) %>%
        filter(
            !(cohort == "precovid" &
                outcome %in% c("ec", "ec_acsc_any"))
        )

    missing_panels <- expected_panels %>%
        anti_join(
            plot_data %>%
                distinct(subgroup, cohort, outcome),
            by = c("subgroup", "cohort", "outcome")
        )

    if (nrow(missing_panels)) {
        warning(
            "Missing subgroup/cohort/outcome combinations in ",
            figure_name, ":\n",
            paste(
                paste(
                    missing_panels$subgroup,
                    missing_panels$cohort,
                    missing_panels$outcome,
                    sep = " / "
                ),
                collapse = "\n"
            )
        )
    }

    # Retain the EC column but omit pre-COVID EC rates.
    plot_data <- plot_data %>%
        filter(
            !(cohort == "precovid" &
                outcome %in% c("ec", "ec_acsc_any"))
        )

    if (!nrow(plot_data)) {
        stop(
            "No data remain after omitting pre-COVID emergency care rates."
        )
    }

    available_cohorts <- cohorts[
        cohorts %in% plot_data$cohort
    ]

    if (!all(cohorts %in% available_cohorts)) {
        stop(
            "No matching data for each selected cohort in ",
            figure_name, "."
        )
    }

    if (anyNA(
        plot_data[, c("week_number", "q1", "median", "q3")]
    )) {
        stop(
            "Missing week or quartile values in ",
            figure_name, "."
        )
    }

    if (any(
        !is.finite(plot_data$q1) |
            !is.finite(plot_data$median) |
            !is.finite(plot_data$q3)
    )) {
        stop(
            "Non-finite quartile values in ",
            figure_name, "."
        )
    }

    if (any(
        plot_data$q1 > plot_data$median |
            plot_data$median > plot_data$q3 |
            plot_data$q1 < 0
    )) {
        stop(
            "Expected nonnegative q1 <= median <= q3 in ",
            figure_name, "."
        )
    }

    duplicate_weeks <- plot_data %>%
        count(subgroup, cohort, outcome, week_number) %>%
        filter(n > 1L)

    if (nrow(duplicate_weeks)) {
        stop(
            "More than one row per subgroup, cohort, outcome and week in ",
            figure_name, "."
        )
    }

    # Outcome labels from labels.csv, with raw names as fallback.
    outcome_names <- setNames(outcomes, outcomes)

    available_outcome_labels <- trajectory %>%
        filter(outcome %in% outcomes) %>%
        distinct(outcome, outcome_label) %>%
        deframe()

    outcome_names[names(available_outcome_labels)] <-
        available_outcome_labels

    is_acsc <- all(outcomes %in% outcomes_acsc)

    if (is_acsc) {
        # Match each ACSC outcome to its all-cause label.
        main_outcome_names <- setNames(
            outcomes_all_cause,
            outcomes_all_cause
        )

        main_labels <- outcome_labels %>%
            filter(outcome %in% outcomes_all_cause) %>%
            mutate(outcome_label = coalesce(outcome_label, outcome)) %>%
            deframe()

        main_outcome_names[names(main_labels)] <- main_labels

        outcome_names <- setNames(
            unname(main_outcome_names[outcomes_all_cause]),
            outcomes_acsc
        )
    }

    # Letter labels for patient-group rows.
    subgroup_names <- c(
        main = "(A)",
        sub_asth = "(B)",
        sub_copd = "(C)",
        sub_diab = "(D)",
        sub_htn = "(E)",
        sub_sevmh = "(F)"
    )

    if (!all(subgroups %in% names(subgroup_names))) {
        stop("Add a panel label for each selected subgroup.")
    }

    cohort_names <- trajectory %>%
        distinct(cohort, cohort_label) %>%
        deframe()

    plot_data <- plot_data %>%
        mutate(
            subgroup = factor(
                subgroup,
                levels = subgroups
            ),
            outcome = factor(
                outcome,
                levels = outcomes
            ),
            cohort = factor(
                cohort,
                levels = cohorts
            )
        ) %>%
        arrange(subgroup, outcome, cohort, week_number)

    # Shared y-axis within each figure, covering all IQR ribbons.
    y_upper <- max(
        12,
        2 * ceiling(max(plot_data$q3) / 2)
    )

    x_start <- as.Date("2000-10-01")
    x_end <- max(plot_data$plot_date)

    p <- ggplot(
        plot_data,
        aes(x = plot_date, group = cohort)
    ) +
        geom_ribbon(
            aes(
                ymin = q1,
                ymax = q3,
                fill = cohort
            ),
            alpha = 0.35,
            colour = NA
        ) +
        geom_line(
            aes(y = median, colour = cohort),
            linewidth = 1
        ) +
        scale_fill_manual(
            name = NULL,
            values = cohort_colours,
            breaks = available_cohorts,
            labels = unname(
                cohort_names[available_cohorts]
            )
        ) +
        scale_colour_manual(
            name = NULL,
            values = cohort_colours,
            breaks = available_cohorts,
            labels = unname(
                cohort_names[available_cohorts]
            )
        ) +
        scale_x_date(
            breaks = seq(
                x_start,
                x_end,
                by = "4 weeks"
            ),
            labels = function(x) {
                paste(
                    format(x, "%b"),
                    as.integer(format(x, "%d"))
                )
            },
            limits = c(x_start, x_end),
            expand = expansion(mult = c(0.01, 0.01))
        ) +
        scale_y_continuous(
            breaks = seq(
                0,
                y_upper,
                by = y_break_interval
            ),
            expand = expansion(mult = c(0, 0.02))
        ) +
        coord_cartesian(
            ylim = c(0, y_upper)
        ) +
        labs(
            title = if (is_acsc) "Hospital use for any ambulatory care sensitive conditions" else NULL,
            x = "Weeks after 1 October",
            y = "Weekly rate per 1,000 registered patients in each group"
        ) +
        facet_grid(
            rows = vars(subgroup),
            cols = vars(outcome),
            labeller = labeller(
                subgroup = as_labeller(subgroup_names),
                outcome = as_labeller(outcome_names)
            ),
            scales = "fixed",
            switch = "y",
            drop = FALSE
        ) +
        theme_bw(base_size = 18) +
        theme(
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(
                colour = "grey88",
                linewidth = 0.3
            ),

            # Extra space between patient-group rows.
            panel.spacing.y = unit(3, "lines"),
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(
                size = 25,
                face = "bold",
                hjust = 0,
                margin = margin(t = 5, b = 20)
            ),

            # Horizontal (A)–(F) labels on the left.
            strip.text.y.left = element_text(
                size = 25,
                face = "bold",
                angle = 0,
                hjust = 0,
                vjust = 1,
                margin = margin(l = 8, r = 20)
            ),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(
                size = 25,
                margin = margin(t = 20)
            ),
            axis.title.y = element_text(
                size = 25,
                margin = margin(r = 20)
            ),
            axis.text.x = element_text(
                size = 15,
                angle = 0,
                hjust = 0
            ),
            axis.text.y = element_text(size = 20),
            legend.position = "bottom",
            legend.text = element_text(
                size = 20
            ),
            legend.key.width = unit(1.2, "cm"),
            legend.key.height = unit(0.7, "cm"),
            plot.title = element_text(
                size = 25,
                face = "bold",
                hjust = 0,
                margin = margin(b = 15)
            ),
            plot.title.position = "panel"
        )

    safe_name <- function(x) {
        x %>%
            str_replace_all("[^A-Za-z0-9]+", "_") %>%
            str_remove("_$")
    }

    filename <- file.path(
        plots_dir,
        paste0(
            "iqr_chart_",
            safe_name(figure_name),
            "_",
            paste(safe_name(cohorts), collapse = "_"),
            ".png"
        )
    )

    ggsave(
        filename = filename,
        plot = p,
        width = 20,
        height = 2.5 * length(subgroups) + 2,
        dpi = 400
    )

    message("Saved ", filename)
    invisible(p)
}


# Figure 1: all-cause hospital use ----------------------------------------------
print("Create combined all-cause figure")

p_all_cause <- create_and_save_iqr_plot(
    trajectory = trajectory,
    subgroups = subgroups_to_plot,
    cohorts = selected_cohorts,
    outcomes = outcomes_all_cause,
    figure_name = "all_cause_combined",
    plots_dir = output_dir
)


# Figure 2: ACSC-related hospital use -------------------------------------------
print("Create combined ACSC figure")

p_acsc <- create_and_save_iqr_plot(
    trajectory = trajectory,
    subgroups = subgroups_to_plot,
    cohorts = selected_cohorts,
    outcomes = outcomes_acsc,
    figure_name = "acsc_combined",
    plots_dir = output_dir
)
