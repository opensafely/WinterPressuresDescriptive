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

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)
# Load data --------------------------------------------------------------------
print("Load model output")

df_plot <- readr::read_csv(
    "output/post_release/table1_plot_data.csv",
    show_col_types = FALSE
)

# Select cohorts for the figure
cohorts_to_plot <- c("precovid", "postcovid1", "postcovid2", "postcovid3")

df_plot <- df_plot %>%
    filter(cohort %in% cohorts_to_plot) %>%
    mutate(
        cohort = factor(
            cohort,
            levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
            labels = c(
                "Pre COVID-19",
                "2022/23",
                "2023/24",
                "2024/25"
            )
        ),
        group = factor(
            group,
            levels = c(
                "List size",
                "Age",
                "Sex",
                "Ethnicity",
                "Deprivation",
                "Smoking Status",
                "Obesity",
                "Care home residence",
                "Monthly consultation"
            )
        )
    ) %>%
    mutate(
        category_label = forcats::fct_relevel(
            category_label,
            unique(labels$label[order(labels$ref)])
        )
    )

cohort_colours <- c(
    "Pre COVID-19" = "#F8766D",
    "2022/23" = "#7CAE00",
    "2023/24" = "#00BFC4",
    "2024/25" = "#C77CFF"
)

# Get list of unique groups
groups <- df_plot %>%
    filter(!is.na(group)) %>%
    pull(group) %>%
    unique()

# Create output folder for plots
plot_dir <- file.path(output_folder, "/table1_plots")
dir.create(plot_dir, showWarnings = FALSE)

# Categories to exclude from selected plots ----------------------------------
# Values must match the text in category_label
categories_to_ignore <- c(
    "Sex" = "Missing sex"
)

walk(groups, function(g) {
    # Identify categories to exclude for this group
    ignored_categories <- unname(
        categories_to_ignore[
            names(categories_to_ignore) == as.character(g)
        ]
    )

    plot_data <- df_plot %>%
        filter(
            group == g,
            !(
                as.character(category_label) %in%
                    ignored_categories
            )
        ) %>%
        droplevels()

    if (nrow(plot_data) == 0) {
        return(NULL)
    }
    # Y-axis label
    y_label <- case_when(
        g == "List size" ~ "Registered patients per practice",
        g == "Monthly consultation" ~ "Consultations per 1,000 patients",
        TRUE ~ "Percentage (%)"
    )

    # Plot title
    title <- case_when(
        g == "List size" ~
            paste(
                "Practice",
                paste0("**", tolower(g), "**"),
                "across practices by cohort"
            ),
        g == "Monthly consultation" ~
            paste(
                "Practice",
                paste0("**", tolower(g), "**"),
                "rate per 1,000 registered patients across practices by cohort"
            ),
        TRUE ~
            paste(
                "Proportion of",
                paste0("**", tolower(g), "**"),
                "by cohort"
            )
    )
    # X-axis text formatting
    x_text <- if (g == "Monthly consultation") {
        element_text(
            size = 9
            # angle = 0,
            # hjust = 1,
            # vjust = 1
        )
    } else {
        element_text(size = 9)
    }

    pd <- position_dodge(width = 0.7)

    width <- if (g == "Obesity" | g == "Sex" | g == "List size") {
        6
    } else if (g == "Care home residence") {
        5
    } else {
        13
    }

    p <- ggplot(
        plot_data,
        aes(
            x = category_label,
            colour = cohort,
            fill = cohort,
            group = cohort
        )
    ) +

        # Whiskers: 10th–90th percentiles
        geom_errorbar(
            aes(
                ymin = p10,
                ymax = p90
            ),
            position = pd,
            width = 0.15,
            linewidth = 0.6,
            show.legend = FALSE
        ) +

        # Box: IQR; centre line: median
        geom_crossbar(
            aes(
                ymin = q1,
                y = median,
                ymax = q3
            ),
            position = pd,
            width = 0.50,
            colour = "black",
            linewidth = 0.5,
            alpha = 1
        ) +

        # Use the same cohort colours as the model plots
        scale_colour_manual(
            values = cohort_colours,
            drop = TRUE
        ) +
        scale_fill_manual(
            values = cohort_colours,
            name = "Cohort",
            drop = TRUE
        ) +
        labs(
            title = title,
            x = NULL,
            y = y_label,
            caption = paste(
                "Boxes show the median and interquartile range;",
                "whiskers show the 10th-90th percentiles."
            )
        ) +
        theme_classic() +
        theme(
            plot.title = ggtext::element_markdown(
                size = 12,
                hjust = 0,
                lineheight = 1.1,
                margin = margin(b = 8)
            ),
            panel.grid.major.y = element_line(
                colour = "grey90",
                linewidth = 0.4
            ),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = x_text,
            axis.title.y = element_text(size = 10),
            plot.caption = element_text(
                size = 8,
                hjust = 0,
                colour = "grey30",
                lineheight = 1.2,
                margin = margin(t = 6)
            ),
            legend.position = "bottom",
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 9)
        ) +

        # Show only the filled-box legend
        guides(
            colour = "none",
            fill = guide_legend(
                title = "Cohort",
                nrow = 1,
                byrow = TRUE,
                override.aes = list(alpha = 0.7)
            )
        )

    ggsave(
        filename = file.path(
            plot_dir,
            paste0("table1_", g, "_median_iqr.png")
        ),
        plot = p,
        width = width,
        height = 4
    )
})
