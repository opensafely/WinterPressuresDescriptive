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
  "output/post_release/table2_raw.csv",
  show_col_types = FALSE
) 

sub_groups<- unique(df$group)

perpeople_cumu <- 1000
perpeople_mean <- 1000

# Filter outcomes to whole population; all practices; unplanned apc and/or ec due to all causes or any acsc conditions
plot_table2 <- function(sub_group) {
  df_g <- df %>%
    filter(
      group == sub_group,
      strata == "Overall",
      acsc_condition %in% c("-", "any")
    ) %>%
    mutate(
      outcome_name = str_remove(outcome_name, "_mp6.*")
    ) %>%
    select(
      -group,
      -source,
      -apc_plan_status,
      -acsc,
      -acsc_condition,
      -midpoint6,
      -stat,
      -strata,
      -prop_zero
    )

  # Add plot labels ---------------------------------------------------------
  print("Add plot labels")

  labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

  analysis_label <- labels %>%
    filter(
      term %in% c(sub_group)
    ) %>%
    pull(label)

  df_g <- merge(
    df_g,
    labels,
    by.x = "outcome_name",
    by.y = "term",
    all.x = TRUE
  )
  df_g <- dplyr::rename(df_g, "outcome_label" = "label")

  # Make outcome_label respect order in ref
  df_g <- df_g %>%
    mutate(
      outcome_label = forcats::fct_relevel(
        outcome_label,
        unique(labels$label[order(labels$ref)])
      )
    )

  # --- Cohort labels (add this new section) ---

  # Define desired order explicitly
  desired_order <- c(
    "Pre-COVID19 (2018-10-01)",
    "Post-lockdown I (2022-10-01)",
    "Post-lockdown II (2023-10-01)",
    "Post-lockdown III (2024-10-01)"
  )

  # Join with labels.csv for pretty names
  df_g <- df_g %>%
    left_join(
      labels %>%
        select(term, label) %>%
        filter(
          term %in%
            c("precovid", "postcovid1", "postcovid2", "postcovid3")
        ),
      by = c("cohort" = "term")
    ) %>%
    mutate(
      cohort_label = if_else(!is.na(label), label, cohort),
      cohort_label = factor(cohort_label, levels = desired_order)
    )

  # Plot Proportion Deciles OVERALL ------------------------------------------------
  print("Plot Proportion Deciles OVERALL")

  df_long <- df_g %>%
    pivot_longer(
      cols = starts_with("p"),
      names_to = "decile",
      values_to = "value"
    ) %>%
    mutate(
      value = case_when(
        str_detect(group, "cumulative") ~ value * perpeople_cumu,
        str_detect(group, "average") ~ value * perpeople_mean,
        TRUE ~ value
      ),
      decile = factor(
        decile,
        levels = c(
          "p10",
          "p20",
          "p30",
          "p40",
          "p50",
          "p60",
          "p70",
          "p80",
          "p90"
        ),
        labels = c(
          "P10",
          "P20",
          "P30",
          "P40",
          "P50",
          "P60",
          "P70",
          "P80",
          "P90"
        )
      )
    )

  # Get list of unique groups
  groups <- df_long %>%
    filter(!is.na(group)) %>%
    pull(group) %>%
    unique()

  # Create output folder for plots
  plot_dir <- file.path(output_folder, "/decile_plots")
  dir.create(plot_dir, showWarnings = FALSE)

  # Loop over groups and save plots
  walk(groups, function(g) {
    # Data for this group
    plot_data <- df_long %>% filter(group == g)

    # Data for horizontal overall lines (one per category × cohort)
    #   lines_df <- df_prop %>%
    #     filter(group == g) %>%
    #     distinct(category_label, cohort_label, prop_overall_midpoint6) %>%
    #     mutate(prop_overall = prop_overall_midpoint6 * 100) # convert to %

    #   # Compute intersection points (P50 vertical line vs horizontal line)
    #   intersection_df <- lines_df %>%
    #     mutate(decile = "P50") # same x-position as the vertical line

    # Y-axis label
    y_label <- case_when(
      str_detect(
        g,
        "cumulative"
      ) ~
        "Cumulative rate per 1,000 registered patients",
      str_detect(g, "average") ~ "Weekly rate per 1,000 registered patients",
      TRUE ~ "Rate"
    )

    # Plot title
    title_text <- paste(
      "Deciles of",
      paste0("**", g, "**"),
      "in ",
      paste0("**", tolower(analysis_label), "**"),
      "during flu and winter months"
    )

    # Create plot
    p <- ggplot(
      plot_data,
      aes(x = decile, y = value, color = cohort_label, group = cohort_label)
    ) +
      # Solid lines = decile trends (practice-level)
      geom_line(linewidth = 0.8, alpha = 0.6) +
      geom_point(size = 1, alpha = 0.6) +
      geom_vline(xintercept = "P50", colour = "grey80", linewidth = 0.6) +

      # Dashed lines = overall proportions
      # geom_hline(
      #   data = lines_df,
      #   aes(
      #     yintercept = prop_overall,
      #     color = cohort_label,
      #     linetype = "Population average"
      #   ),
      #   linewidth = 0.8,
      #   alpha = 0.8,
      #   show.legend = TRUE
      # ) +

      # Triangles = intersection points
      # geom_point(
      #   data = intersection_df,
      #   aes(x = decile, y = prop_overall, color = cohort_label),
      #   shape = 17, # triangle
      #   size = 2, # smaller marker
      #   alpha = 0.9,
      #   show.legend = FALSE
      # ) +

      facet_wrap(~outcome_label) +
      scale_x_discrete(
        labels = c("P10", "", "P30", "", "P50", "", "P70", "", "P90")
      ) +
      labs(
        title = title_text,
        y = y_label,
        x = "Percentile",
        colour = "Cohorts", # legend title for color
        linetype = "" # legend title for linetype
      ) +
      # scale_linetype_manual(
      #   name = "",
      #   values = c("Population average" = "33")
      # ) +
      theme_bw() +
      theme(
        plot.title = element_markdown(),
        axis.text.x = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "vertical"
      )

    # Save each plot
    ggsave(
      filename = file.path(
        plot_dir,
        paste0("deciles_", sub_group, "_", g, ".png")
      ),
      plot = p,
      width = 10,
      height = 6
    )
  })
}

for (sub_group in sub_groups) {
  plot_table2(sub_group)
}
