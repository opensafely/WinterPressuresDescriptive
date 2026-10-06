# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(tidyverse)
library(purrr)
library(data.table)
library(tidyverse)
library(grid)
library(glue)

# Specify paths ----------------------------------------------------------------
print("Specify paths")
source("analysis/specify_paths.R")

# Choose one cohort or overlay two in each outcome facet.
cohort_1 <- "precovid"
cohort_2 <- "postcovid3" # Change to NULL for a single-cohort figure.

# NULL makes a separate figure for every available subgroup; use "main" for one.
subgroups_to_plot <- NULL

outcomes_to_show <- c(
    "apc", "apc_unpl", "apc_plan", "ec",
    "apc_acsc_any", "apc_unpl_acsc_any",
    "apc_plan_acsc_any", "ec_acsc_any"
)

perpeople <- 1000
output_dir <- "output/post_release/iqr_plots_outcomes"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

selected_cohorts <- c(cohort_1, cohort_2)
if (length(selected_cohorts) < 1L ||
    length(selected_cohorts) > 2L ||
    anyNA(selected_cohorts) ||
    anyDuplicated(selected_cohorts)) {
    stop("Choose one cohort, or two different cohorts.")
}

# Match the cohort colours in the forest plot's scale_colour_manual().
cohort_colours <- c(
    precovid = "#F8766D", # Pre-COVID19
    postcovid1 = "#7CAE00", # 2022/23
    postcovid2 = "#00BFC4", # 2023/24
    postcovid3 = "#C77CFF" # 2024/25
)
if (!all(selected_cohorts %in% names(cohort_colours))) {
    stop("Add the selected cohort(s) to cohort_colours.")
}

input <- readr::read_csv(
    file.path(graphs, "input_trajectory_outcomes.csv"),
    show_col_types = FALSE
)
required <- c("cohort", "outcome", "week_number", "q1", "median", "q3")
missing <- setdiff(required, names(input))
if (length(missing)) {
    stop(
        "Missing input column(s): ", paste(missing, collapse = ", "),
        ". The summary input needs q1, median and q3 of the ",
        "practice-level weekly rates."
    )
}

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

outcome_labels <- labels %>%
    transmute(outcome = term, outcome_label = label) %>%
    distinct(outcome, .keep_all = TRUE)

cohort_labels <- labels %>%
    filter(term %in% names(cohort_colours)) %>%
    transmute(cohort = term, cohort_label = label) %>%
    distinct(cohort, .keep_all = TRUE)

subgroup_labels <- labels %>%
    filter(term == "main" | str_detect(term, "^sub_[a-z]+$")) %>%
    transmute(subgroup = term, subgroup_label = label) %>%
    distinct(subgroup, .keep_all = TRUE)

trajectory <- input %>%
    mutate(
        subgroup = coalesce(
            str_extract(outcome, "(main|sub_[a-z]+)$"),
            "main"
        ),
        outcome = str_remove(outcome, "_(main|sub_[a-z]+)$")
    ) %>%
    left_join(outcome_labels, by = "outcome") %>%
    left_join(cohort_labels, by = "cohort") %>%
    left_join(subgroup_labels, by = "subgroup") %>%
    mutate(
        outcome_label = coalesce(outcome_label, outcome),
        cohort_label = coalesce(cohort_label, cohort),
        subgroup_label = coalesce(subgroup_label, subgroup),
        week_number = as.integer(week_number),
        plot_date = as.Date("2000-10-01") + 7L * (week_number - 1L),
        across(c(q1, median, q3), ~ .x * perpeople)
    )

create_and_save_iqr_plot <- function(
  trajectory,
  subgroup_name,
  cohorts,
  outcomes,
  plots_dir
) {
    plot_data <- trajectory %>%
        filter(
            subgroup == subgroup_name,
            cohort %in% cohorts,
            outcome %in% outcomes
        )

    if (!nrow(plot_data)) {
        stop("No matching data for subgroup ", subgroup_name, ".")
    }
    if (!all(outcomes %in% plot_data$outcome)) {
        warning(
            "Some outcomes are absent for subgroup ", subgroup_name, ": ",
            paste(setdiff(outcomes, plot_data$outcome), collapse = ", ")
        )
    }

    # Emergency care data in the pre-COVID period are less reliable.
    # Retain the EC facets, but show post-COVID cohorts only in those facets.
    plot_data <- plot_data %>%
        filter(!(cohort == "precovid" & outcome %in% c("ec", "ec_acsc_any")))
    if (!nrow(plot_data)) {
        stop("No data remain after omitting pre-COVID emergency care rates.")
    }
    available_cohorts <- cohorts[cohorts %in% plot_data$cohort]
    expected_cohorts <- if (all(outcomes %in% c("ec", "ec_acsc_any"))) {
        setdiff(cohorts, "precovid")
    } else {
        cohorts
    }
    if (!all(expected_cohorts %in% available_cohorts)) {
        stop("No matching data for each selected cohort in ", subgroup_name, ".")
    }

    if (anyNA(plot_data[, c("week_number", "q1", "median", "q3")])) {
        stop("Missing week or quartile values in selected data: ", subgroup_name)
    }
    if (any(plot_data$q1 > plot_data$median | plot_data$median > plot_data$q3 |
        plot_data$q1 < 0)) {
        stop("Expected nonnegative q1 <= median <= q3 for ", subgroup_name)
    }

    duplicate_weeks <- plot_data %>%
        count(cohort, outcome, week_number) %>%
        filter(n > 1L)
    if (nrow(duplicate_weeks)) {
        stop("More than one row per cohort, outcome and week in ", subgroup_name)
    }

    outcome_order <- plot_data %>%
        distinct(outcome, outcome_label) %>%
        mutate(outcome = factor(outcome, levels = outcomes)) %>%
        arrange(outcome) %>%
        pull(outcome_label)

    cohort_names <- plot_data %>%
        distinct(cohort, cohort_label) %>%
        deframe()
    plot_data <- plot_data %>%
        mutate(
            cohort = factor(cohort, levels = cohorts),
            outcome_label = factor(outcome_label, levels = outcome_order)
        ) %>%
        arrange(outcome_label, cohort, week_number)

    # One shared y-axis for all outcomes, including all visible IQR ribbons.
    y_upper <- max(12, 2 * ceiling(max(plot_data$q3) / 2))
    x_start <- as.Date("2000-10-01")
    x_end <- max(plot_data$plot_date)
    ncol <- if (length(outcomes) == 8L) 4L else if (length(outcomes) == 6L) 3L else 2L

    p <- ggplot(plot_data, aes(x = plot_date, group = cohort)) +
        geom_ribbon(
            aes(ymin = q1, ymax = q3, fill = cohort),
            alpha = 0.35,
            colour = NA
        ) +
        geom_line(aes(y = median, colour = cohort), linewidth = 1) +
        scale_fill_manual(
            name = NULL,
            values = cohort_colours,
            breaks = available_cohorts,
            labels = unname(cohort_names[available_cohorts])
        ) +
        scale_colour_manual(
            name = NULL,
            values = cohort_colours,
            breaks = available_cohorts,
            labels = unname(cohort_names[available_cohorts])
        ) +
        scale_x_date(
            breaks = seq(x_start, x_end, by = "4 weeks"),
            labels = function(x) paste(format(x, "%b"), as.integer(format(x, "%d"))),
            limits = c(x_start, x_end),
            expand = expansion(mult = c(0.01, 0.01))
        ) +
        scale_y_continuous(
            breaks = seq(0, y_upper, by = 2),
            expand = expansion(mult = c(0, 0.02))
        ) +
        coord_cartesian(ylim = c(0, y_upper)) +
        labs(
            x = "Weeks after 1 October",
            y = "Weekly rate per 1,000 registered patients"
        ) +
        facet_wrap(
            vars(outcome_label),
            ncol = ncol,
            scales = "fixed",
            axes = "margins"
        ) +
        theme_bw(base_size = 18) +
        theme(
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
            strip.background = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = element_text(size = 20, face = "bold", hjust = 0),
            axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 20)),
            axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 10)),
            axis.text.x = element_text(size = 15, angle = 0, hjust = 0),
            axis.text.y = element_text(size = 15),
            legend.position = "bottom"
        )

    safe_name <- function(x) {
        x %>%
            str_replace_all("[^A-Za-z0-9]+", "_") %>%
            str_remove("_$")
    }
    filename <- file.path(
        plots_dir,
        paste0(
            "iqr_chart_", safe_name(subgroup_name), "_",
            paste(safe_name(cohorts), collapse = "_"), ".png"
        )
    )
    ggsave(filename, p, width = 20, height = 12, dpi = 400)
    message("Saved ", filename)
    invisible(p)
}

if (is.null(subgroups_to_plot)) {
    subgroups_to_plot <- unique(trajectory$subgroup)
}

for (subgroup_value in subgroups_to_plot) {
    create_and_save_iqr_plot(
        trajectory = trajectory,
        subgroup_name = subgroup_value,
        cohorts = selected_cohorts,
        outcomes = outcomes_to_show,
        plots_dir = output_dir
    )
}
