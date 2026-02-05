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

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)
# Load data --------------------------------------------------------------------
print("Load model output")

df_plot <- readr::read_csv(
    "output/post_release/table1_plot_data.csv",
    show_col_types = FALSE
)

df_plot <- df_plot %>%
    mutate(
        cohort = factor(
            cohort,
            levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
            labels = c(
                "Pre-COVID",
                "Post-lockdown I",
                "Post-lockdown II",
                "Post-lockdown III"
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
                "Rurality",
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


# Get list of unique groups
groups <- df_plot %>%
    filter(!is.na(group)) %>%
    pull(group) %>%
    unique()

# Create output folder for plots
plot_dir <- file.path(output_folder, "/table1_plots")
dir.create(plot_dir, showWarnings = FALSE)

walk(groups, function(g) {
    plot_data <- df_plot %>%
        filter(group == g)

    if (nrow(plot_data) == 0) {
        return(NULL)
    }
    # Y-axis label
    y_label <- case_when(
        g == "List size" ~ "Number of registered patients, median(IQR)",
        g == "Monthly consultation" ~ "Consultations per 1,000 patients, median(IQR)",
        TRUE ~ "Proportion (%), median(IQR)"
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
                "Practice composition of",
                paste0("**", tolower(g), "**"),
                "by cohort"
            )
    )

    p <- ggplot(
        plot_data,
        aes(
            x = category_label,
            y = median,
            fill = cohort
        )
    ) +
        geom_col(
            position = position_dodge(width = 0.7),
            width = 0.6,
            colour = "grey30"
        ) +
        geom_errorbar(
            aes(ymin = q1, ymax = q3),
            position = position_dodge(width = 0.7),
            width = 0.2,
            linewidth = 0.6
        ) +
        labs(
            title = title,
            x = NULL,
            y = y_label,
            fill = "Cohort"
        ) +
        theme_bw() +
        theme(
            plot.title = element_text(size = 12),
            axis.text.x = element_text(size = 9),
            axis.title.y = element_text(size = 10),
            legend.position = "bottom",
            panel.grid.minor = element_blank()
        )

    ggsave(
        filename = file.path(
            plot_dir,
            paste0("table1_", g, "_median_iqr.png")
        ),
        plot = p,
        width = 14,
        height = 4
    )
})
