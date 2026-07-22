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

# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(
    paste0(graphs, "input_trajectory_outcomes.csv"),
    show_col_types = FALSE
)

# Plot settings ---------------------------------------------------------------

perpeople <- 1000

# Create output folder
plot_dir <- file.path(output_folder, "decile_plots_outcomes")

dir.create(
    plot_dir,
    recursive = TRUE,
    showWarnings = FALSE
)

# Add plot labels -------------------------------------------------------------

print("Add plot labels")

labels <- readr::read_csv(
    "lib/labels.csv",
    show_col_types = FALSE
)

# Outcome labels
outcome_labels <- labels %>%
    select(
        outcome = term,
        outcome_label = label,
        ref
    )

# Cohort labels
cohort_labels <- labels %>%
    filter(
        term %in% c(
            "precovid",
            "postcovid1",
            "postcovid2",
            "postcovid3"
        )
    ) %>%
    select(
        cohort = term,
        cohort_label = label
    )

desired_cohort_order <- c(
    "Pre-COVID19",
    "2022/23",
    "2023/24",
    "2024/25"
)

# Subgroup labels
subgroup_labels <- labels %>%
    filter(
        term %in% c(
            "main",
            "sub_[a-z]+"
        )
    ) %>%
    select(
        subgroup = term,
        subgroup_label = label
    )

# Prepare decile data ---------------------------------------------------------

print("Prepare decile data")

practice_deciles <- df %>%
    mutate(
        # Extract subgroup if it appears at the end of the outcome name
        subgroup = str_extract(
            outcome,
            "(main|sub_[a-z]+)$"
        ),
        outcome = str_remove(
            outcome,
            "_(main|sub_[a-z]+)$"
        )
    ) %>%
    left_join(
        outcome_labels,
        by = "outcome"
    ) %>%
    left_join(
        cohort_labels,
        by = "cohort"
    ) %>%
    left_join(
        subgroup_labels,
        by = "subgroup"
    ) %>%
    mutate(
        # Use the original names when no label is available
        outcome_label = coalesce(
            outcome_label,
            outcome
        ),
        cohort_label = coalesce(
            cohort_label,
            cohort
        ),
        subgroup_label = coalesce(
            subgroup_label,
            subgroup
        ),
        cohort_label = factor(
            cohort_label,
            levels = desired_cohort_order
        )
    ) %>%
    # Retain D1, D3, D5, D7 and D9
    # pivot_longer(
    #     cols = c(
    #         p10,
    #         p30,
    #         p50,
    #         p70,
    #         p90
    #     ),
    #     names_to = "decile",
    #     values_to = "value"
    # ) %>%
    # mutate(
    #     # Convert the proportions to rates per 1,000
    #     value = value * perpeople,
    #     decile = recode(
    #         decile,
    #         "p10" = "d1",
    #         "p30" = "d3",
    #         "p50" = "d5",
    #         "p70" = "d7",
    #         "p90" = "d9"
    #     ),
    #     decile = factor(
    #         decile,
    #         levels = c(
    #             "d1",
    #             "d3",
    #             "d5",
    #             "d7",
    #             "d9"
    #         ),
    #         labels = c(
    #             "D1",
    #             "D3",
    #             "D5",
    #             "D7",
    #             "D9"
    #         )
    #     ),
    #     week_number = as.integer(week_number)
    # )
    pivot_longer(
        cols = c(
            p10,
            p20,
            p30,
            p40,
            p50,
            p60,
            p70,
            p80,
            p90
        ),
        names_to = "decile",
        values_to = "value"
    ) %>%
    mutate(
        # Convert proportions to rates per 1,000
        value = value * perpeople,
        decile = recode(
            decile,
            "p10" = "D1",
            "p20" = "D2",
            "p30" = "D3",
            "p40" = "D4",
            "p50" = "D5",
            "p60" = "D6",
            "p70" = "D7",
            "p80" = "D8",
            "p90" = "D9"
        ),
        decile = factor(
            decile,
            levels = paste0("D", 1:9)
        ),
        week_number = as.integer(week_number),
        plot_date = as.Date("2000-10-01") +
            lubridate::weeks(week_number - 1)
    ) %>%
    select(
        cohort,
        cohort_label,
        outcome,
        outcome_label,
        ref,
        subgroup,
        subgroup_label,
        week_number,
        plot_date,
        decile,
        value
    ) %>%
    arrange(
        cohort_label,
        outcome_label,
        subgroup_label,
        decile,
        week_number
    )

# Create and save decile chart ------------------------------------------------

create_and_save_decile_plot <- function(
  deciles_df,
  cohort_name,
  subgroup_name,
  outcomes_to_show,
  plots_dir,
  y_var = "value",
  x_var = "plot_date",
  ncol = 2
) {
    print(head(deciles_df))

    plot_data <- deciles_df %>%
        filter(
            subgroup == subgroup_name,
            outcome %in% outcomes_to_show
        ) %>%
        mutate(
            outcome = factor(
                outcome,
                levels = outcomes_to_show
            )
        )

    # Obtain outcome labels in the manually specified order
    outcome_order <- plot_data %>%
        distinct(
            outcome,
            outcome_label
        ) %>%
        arrange(outcome) %>%
        pull(outcome_label)

    plot_data <- plot_data %>%
        mutate(
            outcome_label = factor(
                outcome_label,
                levels = outcome_order
            )
        )

    # Obtain the subgroup label for the title
    subgroup_title <- plot_data %>%
        distinct(subgroup_label) %>%
        pull(subgroup_label) %>%
        first()

    if (is.na(subgroup_title) || length(subgroup_title) == 0) {
        subgroup_title <- subgroup_name
    }

    # Obtain cohort labels for the title
    cohort_title <- plot_data %>%
        distinct(cohort_label) %>%
        pull(cohort_label) %>%
        first()

    if (is.na(cohort_title) || length(cohort_title) == 0) {
        cohort_title <- cohort_name
    }

    # Create the plot
    p <- ggplot(
        plot_data,
        aes(
            x = !!sym(x_var),
            y = !!sym(y_var),
            group = decile
        )
    ) +
        # Dashed lines for D1, D3, D7 and D9
        geom_line(
            data = function(d) {
                d %>% filter(decile != "D5")
            },
            colour = "grey50",
            linetype = "dashed",
            linewidth = 0.6
        ) +

        # Solid red line for D5 (median)
        geom_line(
            data = function(d) {
                d %>% filter(decile == "D5")
            },
            colour = "red",
            linetype = "solid",
            linewidth = 0.9
        ) +
        # Fortnightly date labels beginning on 1 October
        scale_x_date(
            breaks = seq(
                from = as.Date("2000-10-01"),
                to = as.Date("2001-02-11"),
                by = "4 weeks"
            ),
            labels = function(x) {
                paste(
                    format(x, "%b"),
                    as.integer(format(x, "%d"))
                )
            },
            limits = c(
                as.Date("2000-10-01"),
                as.Date("2001-02-11")
            ),
            expand = expansion(mult = c(0.01, 0.01))
        ) +
        # Same y-axis for every panel
        scale_y_continuous(
            breaks = seq(0, 12, by = 2),
            limits = c(0, 12),
            expand = expansion(mult = c(0, 0.02))
        ) +
        labs(
            title = glue(
                "Decile charts for weekly hospital use: ",
                "{cohort_title}, {subgroup_title}"
            ),
            x = "Weeks after 1st October",
            y = "Weekly rate per 1,000 registered patients"
        ) +
        facet_wrap(
            vars(outcome_label),
            ncol = ncol,
            scales = "fixed",
            axes = "all_x",
            axis.labels = "all_x"
        ) +
        theme_bw(base_size = 18) +
        theme(
            # Overall title
            plot.title = element_text(
                size = 22,
                face = "bold"
            ),

            # Outcome/facet titles
            strip.text = element_text(
                size = 20,
                face = "bold"
            ),

            # Axis titles
            axis.title.x = element_text(
                size = 18,
                face = "bold",
                margin = margin(t = 10)
            ),
            axis.title.y = element_text(
                size = 18,
                face = "bold",
                margin = margin(r = 10)
            ),

            # Axis tick labels
            axis.text.x = element_text(
                size = 15,
                angle = 45,
                hjust = 1
            ),
            axis.text.y = element_text(
                size = 15
            )
        )

    # Make values safe for use in the filename
    cohort_filename <- cohort_name %>%
        as.character() %>%
        str_replace_all("[^A-Za-z0-9]+", "_") %>%
        str_remove("_$")

    subgroup_filename <- subgroup_name %>%
        as.character() %>%
        str_replace_all("[^A-Za-z0-9]+", "_") %>%
        str_remove("_$")

    filename <- file.path(
        plots_dir,
        glue(
            "decile_chart_",
            "{subgroup_filename}_",
            "{cohort_filename}_",
            "{y_var}.png"
        )
    )

    ggsave(
        filename = filename,
        plot = p,
        width = 20,
        height = 12,
        dpi = 400
    )

    invisible(p)
}

# Create one figure for each subgroup and cohort -------------------------------

# outcomes_to_show <- c(
#     "apc",
#     "apc_unpl",
#     "apc_plan",
#     "ec",
#     "apc_acsc_any",
#     "apc_unpl_acsc_any",
#     "apc_plan_acsc_any",
#     "ec_acsc_any"
# )

outcomes_to_show <- c(
    "apc",
    "apc_unpl",
    "apc_plan",
    "ec",
    "apc_acsc_any",
    "apc_unpl_acsc_any",
    "apc_plan_acsc_any",
    "ec_acsc_any"
)

ncol <- if (length(outcomes_to_show) == 8) {
    4
} else if (length(outcomes_to_show) == 6) {
    3
} else {
    2
}

for (subgroup_value in unique(practice_deciles$subgroup)) {
    if (is.na(subgroup_value)) {
        next
    }

    subgroup_data <- practice_deciles %>%
        filter(
            .data$subgroup == subgroup_value
        )

    for (cohort_value in unique(subgroup_data$cohort)) {
        if (is.na(cohort_value)) {
            next
        }

        print(
            glue(
                "Creating decile chart for subgroup ",
                "{subgroup_value} and cohort {cohort_value}"
            )
        )

        filtered_deciles <- subgroup_data %>%
            filter(
                .data$cohort == cohort_value
            )

        create_and_save_decile_plot(
            deciles_df = filtered_deciles,
            cohort_name = as.character(cohort_value),
            subgroup_name = subgroup_value,
            outcomes_to_show = outcomes_to_show,
            plots_dir = plot_dir,
            y_var = "value",
            x_var = "plot_date",
            ncol = ncol
        )
    }
}

print("DECILE CHARTS GENERATED")
