# Load libraries --------------------------------------------------------------
print('Load libraries')

library(tidyverse)
library(lubridate)
library(data.table)
library(jsonlite)
library(here)
library(fs)

# Define correlations output folder ---------------------------------------------------------
print("Creating output/correlations output folder")

output_dir <- "output/correlations/"
fs::dir_create(here::here(output_dir))

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify command arguments ----------------------------------------------------
print('Specify command arguments')

args <- commandArgs(trailingOnly = TRUE)
print(length(args))
if (length(args) == 0) {
    cohort <- "postcovid1"
} else {
    cohort <- args[[1]]
}

# Define input file path ---------------------------------------------------------
print('Define input file path')

file_path <- paste0("output/dataset_clean/input_", cohort, "_clean.csv")

# Load data ----------------------------------------------------------------------
print('Load data')

input <- read_csv(file_path)
message(paste0(
    "Dataset has been read successfully with N = ",
    nrow(input),
    " rows"
))

# Restrict columns for exposures (ratios) ------------------------------------------------
print('Restrict columns')

n_before <- ncol(input)

input <- input %>%
    select(
        practice_id,
        starts_with("index_date"),
        starts_with("exp_prop") # Exposures
    )

n_after <- ncol(input)
n_removed <- n_before - n_after

message("Number of variables before restriction: ", n_before)
message("Number of variables after restriction: ", n_after)
message("Number of variables removed: ", n_removed)


print("Define variable categories for correlation plots")

# Identify variable groups ------------------------------------------------
vars_cons <- grep("^exp_prop_cons_", names(input), value = TRUE)
vars_age <- grep(
    "^exp_prop_(under5y|[0-9]+_to_[0-9]+|age_85_plus|age_missing)$",
    names(input),
    value = TRUE
)
vars_eth <- grep("^exp_prop_eth_", names(input), value = TRUE)
vars_imd <- grep("^exp_prop_imd_", names(input), value = TRUE)
vars_urb <- grep("^exp_prop_(urb_|rura)", names(input), value = TRUE)
vars_smoke <- grep("^exp_prop_smoker_", names(input), value = TRUE)
vars_sex <- grep(
    "^exp_prop_(male|female|sex_missing)$",
    names(input),
    value = TRUE
)
vars_morb <- grep(
    "^exp_prop_(af|alcoholproblem|anxietydepression|asthma|cancer|chd|ckd|constipation|copd|ctd|dementia|diabetes|epilepsy|hearingloss|hf|hypt|ibs|osteoarthritis|psychosis|stroketia|obesity)",
    names(input),
    value = TRUE
)

# Combine categories with readable labels ---------------------------------
categories <- list(
    consultation = list(vars = vars_cons, label = "Consultation"),
    age = list(vars = vars_age, label = "Age"),
    ethnicity = list(vars = vars_eth, label = "Ethnicity"),
    imd = list(vars = vars_imd, label = "Index of Multiple Deprivation (IMD)"),
    rurality = list(vars = vars_urb, label = "Urban–Rural Classification"),
    smoking = list(vars = vars_smoke, label = "Smoking Status"),
    sex = list(vars = vars_sex, label = "Sex"),
    morbidity = list(
        vars = vars_morb,
        label = "Morbidity (Long-Term Conditions)"
    )
)

plot_corr_heatmap <- function(
    df,
    vars,
    category_name,
    category_label,
    output_dir
) {
    message("\n--- Processing category: ", category_label, " ---")

    # Drop missing or constant columns
    vars_nonconst <- vars[
        sapply(vars, function(v) var(df[[v]], na.rm = TRUE) > 0)
    ]
    if (length(vars_nonconst) < 2) {
        message(
            "Skipping ",
            category_label,
            ": fewer than 2 non-constant variables."
        )
        return(NULL)
    }

    # Compute correlation matrix
    corr_matrix <- cor(df[vars_nonconst], use = "pairwise.complete.obs")

    # Save correlation matrix as CSV
    csv_path <- file.path(
        output_dir,
        paste0("correlations_", category_name, "_", cohort, ".csv")
    )
    write.csv(corr_matrix, csv_path, row.names = TRUE)
    message("Saved correlation matrix: ", csv_path)

    # Melt for ggplot
    melted_corr <- corr_matrix %>%
        as.data.frame() %>%
        rownames_to_column("Var1") %>%
        pivot_longer(-Var1, names_to = "Var2", values_to = "value")

    # Generate readable heatmap
    p <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "grey90") +
        scale_fill_gradient2(
            low = "blue",
            mid = "white",
            high = "red",
            midpoint = 0,
            limits = c(-1, 1),
            name = "Correlation"
        ) +
        theme_minimal(base_size = 11) +
        theme(
            axis.text.x = element_text(
                angle = 70,
                vjust = 1,
                hjust = 1,
                size = 9
            ),
            axis.text.y = element_text(size = 9),
            plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
            panel.grid = element_blank()
        ) +
        labs(
            title = paste("Correlations of variables for", category_label),
            x = NULL,
            y = NULL
        )

    # Save plot
    plot_path <- file.path(
        output_dir,
        paste0("heatmap_", category_name, "_", cohort, ".png")
    )
    ggsave(plot_path, p, width = 9, height = 7, dpi = 300)
    message("Saved heatmap: ", plot_path)
}

# Generate correlation plots by category ------------------------------
print("Generate correlation heatmaps by category")

for (cat_name in names(categories)) {
    cat_vars <- categories[[cat_name]]$vars
    cat_label <- categories[[cat_name]]$label
    plot_corr_heatmap(
        input,
        cat_vars,
        cat_name,
        cat_label,
        here::here(output_dir)
    )
}

print("All category correlation plots generated successfully.")

# Pairwise scatter plot: September consultation rates vs mean (per 1,000 patients)
print(
    "Generate pairwise scatter plot for September consultation rates and the mean (per 1,000 patients)"
)

# Dynamically find the consultation variable ending in '09' (September)
cons_sep_var <- grep("^exp_prop_cons_\\d{6}$", names(input), value = TRUE)
cons_sep_var <- cons_sep_var[grepl("09$", cons_sep_var)] # pick those ending with 09
message("Identified September consultation variable: ", cons_sep_var)

# Check both variables exist
if (length(cons_sep_var) == 1 && "exp_prop_cons_mean" %in% names(input)) {
    # Compute per-1,000 patient rates
    input <- input %>%
        mutate(
            cons_sep_per1000 = .data[[cons_sep_var]] * 1000,
            cons_mean_per1000 = exp_prop_cons_mean * 1000
        )

    # Compute correlation coefficient
    corr_val <- cor(
        input$cons_sep_per1000,
        input$cons_mean_per1000,
        use = "pairwise.complete.obs"
    )

    # Create scatter plot
    p_scatter <- ggplot(
        input,
        aes(x = cons_sep_per1000, y = cons_mean_per1000)
    ) +
        geom_point(alpha = 0.7, color = "steelblue", size = 2) +
        geom_smooth(
            method = "lm",
            se = TRUE,
            color = "darkred",
            linewidth = 0.8
        ) +
        theme_minimal(base_size = 12) +
        labs(
            title = paste0(
                "Scatter plot of consultation rates in September and the mean \n",
                "(Pearson r = ",
                round(corr_val, 3),
                ")"
            ),
            x = "Consultation rate in September (per 1,000 patients)",
            y = "Mean consultation rate over 12 months (per 1,000 patients)"
        ) +
        theme(
            plot.title = element_text(
                face = "bold",
                size = 13,
                hjust = 0,
                vjust = 1,
                lineheight = 1.1
            ),
            plot.margin = margin(10, 15, 10, 15),
            panel.grid.minor = element_blank()
        )

    # Save scatter plot
    scatter_path <- file.path(
        here::here(output_dir),
        paste0("scatter_cons_sep_vs_mean_", cohort, ".png")
    )
    ggsave(scatter_path, p_scatter, width = 9, height = 6.5, dpi = 300)
    message("Saved scatter plot: ", scatter_path)
} else {
    warning(
        "exp_prop_cons_sep or exp_prop_cons_mean not found in dataset — skipping scatter plot."
    )
}

# Massive correlation heatmap across all exposure variables (ordered by category)
# ---------------------------------------------------------------------
print(
    "Generate massive correlation heatmap across all exposure variables (ordered by category)"
)

# Combine all variables from your defined categories (preserving order)
all_vars_ordered <- unlist(lapply(categories, function(x) x$vars))
all_vars_ordered <- all_vars_ordered[all_vars_ordered %in% names(input)]

# Drop columns with zero variance or all missing
all_vars_ordered <- all_vars_ordered[
    sapply(all_vars_ordered, function(v) var(input[[v]], na.rm = TRUE) > 0)
]

if (length(all_vars_ordered) > 1) {
    message("Number of exposure variables included: ", length(all_vars_ordered))

    # Compute correlation matrix
    corr_all <- cor(input[all_vars_ordered], use = "pairwise.complete.obs")

    # Save correlation matrix
    csv_path_all <- file.path(
        here::here(output_dir),
        paste0("correlations_all_variables_ordered_", cohort, ".csv")
    )
    write.csv(corr_all, csv_path_all, row.names = TRUE)
    message("Saved ordered full correlation matrix: ", csv_path_all)

    # Melt for ggplot
    melted_all <- corr_all %>%
        as.data.frame() %>%
        rownames_to_column("Var1") %>%
        pivot_longer(-Var1, names_to = "Var2", values_to = "value")

    # Make sure the variables appear in the intended category order
    melted_all$Var1 <- factor(melted_all$Var1, levels = all_vars_ordered)
    melted_all$Var2 <- factor(melted_all$Var2, levels = all_vars_ordered)

    # Generate big correlation heatmap
    p_all <- ggplot(melted_all, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "grey90") +
        scale_fill_gradient2(
            low = "blue",
            mid = "white",
            high = "red",
            midpoint = 0,
            limits = c(-1, 1),
            name = "Correlation"
        ) +
        theme_minimal(base_size = 10) +
        theme(
            axis.text.x = element_text(
                angle = 70,
                vjust = 1,
                hjust = 1,
                size = 6
            ),
            axis.text.y = element_text(size = 6),
            plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
            panel.grid = element_blank()
        ) +
        labs(
            title = paste(
                "Massive correlation heatmap across all exposure variables (ordered by category, N =",
                length(all_vars_ordered),
                ")"
            ),
            x = NULL,
            y = NULL
        )

    # Save big heatmap
    plot_path_all <- file.path(
        here::here(output_dir),
        paste0("heatmap_all_exposures_", cohort, ".png")
    )
    ggsave(plot_path_all, p_all, width = 13, height = 11, dpi = 300)
    message(
        "Saved massive correlation heatmap (ordered by category): ",
        plot_path_all
    )
} else {
    warning(
        "Fewer than 2 valid exposure variables available for massive correlation heatmap."
    )
}
