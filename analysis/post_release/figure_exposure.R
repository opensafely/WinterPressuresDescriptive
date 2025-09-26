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

# Load data --------------------------------------------------------------------
print("Load model output")

# List all CSV files matching the pattern
file_list <- list.files(
  path = table1,
  pattern = "^table1-.*-midpoint6\\.csv$",
  full.names = TRUE
)

# read, add cohort column, and combine
df <- file_list %>%
  lapply(function(f) {
    df <- read_csv(f)
    cohort <- str_match(basename(f), "table1-cohort_(.*)-midpoint6")[, 2]
    df %>% mutate(cohort = cohort)
  }) %>%
  bind_rows()

readr::write_csv(df, paste0(output_folder, "/table1.csv"), na = "-")


# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(
  "output/post_release/table1.csv",
  show_col_types = FALSE
)

df <- df %>%
  mutate(across(ends_with("_midpoint6"), as.numeric))

# Add plot labels ---------------------------------------------------------
print("Add plot labels")

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

df <- merge(
  df,
  labels,
  by.x = "category",
  by.y = "term",
  all.x = TRUE
)
df <- dplyr::rename(df, "category_label" = "label")

# Make category_label respect order in ref
df <- df %>%
  mutate(
    category_label = forcats::fct_relevel(
      category_label,
      unique(labels$label[order(labels$ref)])
    )
  )

# Plot Proportion
df_prop <- df %>%
  filter(type == "prop", strata == "Overall")

df_long <- df_prop %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "decile",
    values_to = "value"
  ) %>%
  mutate(
    value = value * 100, # scale to percent
    decile = factor(
      decile,
      levels = c(
        "p10_midpoint6",
        "p20_midpoint6",
        "p30_midpoint6",
        "p40_midpoint6",
        "p50_midpoint6",
        "p60_midpoint6",
        "p70_midpoint6",
        "p80_midpoint6",
        "p90_midpoint6"
      ),
      labels = c("P10", "P20", "P30", "P40", "P50", "P60", "P70", "P80", "P90")
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
  p <- ggplot(
    df_long %>% filter(group == g),
    aes(x = decile, y = value, color = cohort, group = cohort)
  ) +
    geom_line(size = 0.8, alpha = 0.6) +
    geom_point(size = 1, alpha = 0.6) +
    geom_vline(xintercept = "P50", colour = "grey80", size = 0.6) +
    facet_wrap(~category_label, scales = "free_y") +
    scale_x_discrete(
      labels = c("P10", "", "P30", "", "P50", "", "P70", "", "P90")
    ) +
    labs(
      title = paste(
        "Deciles of the proportion of",
        g,
        "groups across practices by cohort"
      ),
      y = "Proportion (%)",
      x = "Percentile"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 9),
      legend.position = "bottom"
    )

  # save each plot
  ggsave(
    filename = file.path(plot_dir, paste0("deciles_", g, ".png")),
    plot = p,
    width = 10,
    height = 6
  )
})


ggplot(
  df_long %>% filter(group == "Age"),
  aes(x = decile, y = value, color = cohort, group = cohort)
) +
  geom_line(size = 0.8, alpha = 0.6) +
  geom_point(size = 1, alpha = 0.8) +
  facet_wrap(~category_label, scales = "free_y") +
  scale_x_discrete(
    labels = c("P10", "", "P30", "", "P50", "", "P70", "", "P90")
  ) +
  labs(
    title = "Deciles of proportions by cohort",
    y = "Proportion",
    x = "Percentile"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 9),
    legend.position = "bottom"
  )

# Consultations – Deciles, Median, Q1, Q3 -------------------------------------

df_cons <- df %>%
  filter(type == "prop", strata == "Overall", group == "Consultations") %>%
  select(
    category,
    category_label,
    cohort,
    p10_midpoint6,
    p20_midpoint6,
    p30_midpoint6,
    p40_midpoint6,
    p50_midpoint6,
    p60_midpoint6,
    p70_midpoint6,
    p80_midpoint6,
    p90_midpoint6,
    q1_midpoint6,
    median_midpoint6,
    q3_midpoint6
  ) %>%
  # scale to per 1000 patients
  mutate(across(where(is.numeric), ~ .x * 1000))

# Reshape all deciles
df_deciles <- df_cons %>%
  pivot_longer(
    cols = starts_with("p"),
    names_to = "percentile",
    values_to = "value"
  ) %>%
  mutate(
    percentile = factor(
      percentile,
      levels = c(
        "p10_midpoint6",
        "p20_midpoint6",
        "p30_midpoint6",
        "p40_midpoint6",
        "p50_midpoint6",
        "p60_midpoint6",
        "p70_midpoint6",
        "p80_midpoint6",
        "p90_midpoint6"
      ),
      labels = c("P10", "P20", "P30", "P40", "P50", "P60", "P70", "P80", "P90")
    )
  )

# Reshape median, q1, q3
df_summary <- df_cons %>%
  pivot_longer(
    cols = c(q1_midpoint6, median_midpoint6, q3_midpoint6),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    stat = recode(
      stat,
      q1_midpoint6 = "Q1",
      median_midpoint6 = "Median",
      q3_midpoint6 = "Q3"
    )
  )

# Plot
p_cons <- ggplot() +
  # all deciles (thin dashed)
  geom_line(
    data = df_deciles,
    aes(
      x = category_label,
      y = value,
      group = interaction(cohort, percentile),
      color = cohort
    ),
    linetype = "dashed",
    size = 0.4,
    alpha = 0.5
  ) +
  # Q1 and Q3 (thinner solid)
  geom_line(
    data = df_summary %>% filter(stat %in% c("Q1", "Q3")),
    aes(
      x = category_label,
      y = value,
      group = interaction(cohort, stat),
      color = cohort
    ),
    size = 0.6,
    alpha = 0.8
  ) +
  # Median (thicker solid, highlighted with points)
  geom_line(
    data = df_summary %>% filter(stat == "Median"),
    aes(
      x = category_label,
      y = value,
      group = interaction(cohort, stat),
      color = cohort
    ),
    size = 1.2
  ) +
  geom_point(
    data = df_summary %>% filter(stat == "Median"),
    aes(
      x = category_label,
      y = value,
      group = interaction(cohort, stat),
      color = cohort
    ),
    size = 1
  ) +
  labs(
    title = "Consultations: Deciles with highlighted Median & IQR over time",
    x = "Month",
    y = "Consultations per 1,000 patients"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(plot_dir, "consultations_deciles_median_iqr.png"),
  plot = p_cons,
  width = 12,
  height = 6
)


p_cons <- ggplot(
  df_cons,
  aes(x = category_label, color = cohort, fill = cohort)
) +
  # P10–P90 ribbon
  geom_ribbon(
    aes(ymin = p10_midpoint6, ymax = p90_midpoint6, group = cohort),
    alpha = 0.15,
    linetype = 0
  ) +
  # Q1–Q3 ribbon
  geom_ribbon(
    aes(ymin = q1_midpoint6, ymax = q3_midpoint6, group = cohort),
    alpha = 0.3,
    linetype = 0
  ) +
  # Median line
  geom_line(aes(y = median_midpoint6, group = cohort), size = 1) +
  geom_point(aes(y = median_midpoint6, group = cohort), size = 1) +
  labs(
    title = "Consultations per 1,000 patients: Median, IQR and P10–P90 over time",
    x = "Month",
    y = "Consultations per 1,000 patients"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(plot_dir, "consultations_band.png"),
  plot = p_cons,
  width = 12,
  height = 6
)
