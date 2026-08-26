# Combine and format released VIF results by cohort

# Load libraries --------------------------------------------------------------
print("Load libraries")

library(tidyverse)
library(fs)

# Specify paths ---------------------------------------------------------------
print("Specify paths")

source("analysis/specify_paths.R")

# Make post-release directory -------------------------------------------------
print("Make post-release directory")

output_folder <- file.path(
    "output",
    "post_release"
)

dir_create(output_folder)

# Create output folder for correlation and VIF outputs
output_dir <- file.path(
    output_folder,
    "correlations"
)

dir_create(output_dir)

# Specify cohorts -------------------------------------------------------------
cohorts <- c(
    "precovid",
    "postcovid1",
    "postcovid2",
    "postcovid3"
)

# Specify exposure order ------------------------------------------------------
exposure_order <- c(
    "Region: East Midlands",
    "Region: London",
    "Region: North East",
    "Region: North West",
    "Region: South East",
    "Region: South West",
    "Region: West Midlands",
    "Region: Yorkshire and The Humber",
    "Rurality: Urban town",
    "Rurality: Rural",
    "list_size",
    "cons_mean",
    "age_0_4",
    "age_65_74",
    "age_75_79",
    "age_80",
    "sex_female",
    "ethnicity_white",
    "ethnicity_asian",
    "ethnicity_black",
    "ethnicity_mixed",
    "ethnicity_other",
    "imd_1_most",
    "imd_5_least",
    "smoking_never",
    "smoking_ever",
    "smoking_current",
    "obesity",
    "carehome"
)

# Read released VIF files ------------------------------------------------------
print("Read released VIF files")

read_released_vif <- function(cohort) {

    input_path <- file.path(
        correlations,
        cohort,
        paste0(
            "vif_mutually_adjusted_exposures_",
            cohort,
            ".csv"
        )
    )

    if (!file_exists(input_path)) {
        stop(
            "Released VIF file not found: ",
            input_path
        )
    }

    vif_data <- read_csv(
        input_path,
        show_col_types = FALSE
    )

    required_columns <- c(
        "exposure",
        "exposure_label",
        "r_squared",
        "vif",
        "n_practices_midpoint6"
    )

    missing_columns <- setdiff(
        required_columns,
        names(vif_data)
    )

    if (length(missing_columns) > 0) {
        stop(
            "Required columns missing from ",
            input_path,
            ": ",
            paste(missing_columns, collapse = ", ")
        )
    }

    vif_data |>
        mutate(
            cohort = cohort,
            .before = 1
        ) |>
        select(
            cohort,
            exposure,
            exposure_label,
            r_squared,
            vif,
            n_practices_midpoint6
        )
}

# Read and combine all four released CSV files
vif_raw <- map_dfr(
    cohorts,
    read_released_vif
)

# Check cohort-level practice counts ------------------------------------------
print("Check cohort-level practice counts")

n_practices <- vif_raw |>
    group_by(cohort) |>
    summarise(
        n_values = n_distinct(
            n_practices_midpoint6
        ),
        n_practices_midpoint6 = first(
            n_practices_midpoint6
        ),
        .groups = "drop"
    )

if (any(n_practices$n_values != 1)) {
    stop(
        paste0(
            "More than one n_practices_midpoint6 value ",
            "was found within a cohort."
        )
    )
}

n_practices <- n_practices |>
    select(
        cohort,
        n_practices_midpoint6
    )

# Keep only the requested VIF variables ---------------------------------------
vif_results <- vif_raw |>
    select(
        cohort,
        exposure,
        exposure_label,
        r_squared,
        vif
    )

# Check for duplicate exposure rows -------------------------------------------
duplicate_exposures <- vif_results |>
    count(
        cohort,
        exposure
    ) |>
    filter(n != 1)

if (nrow(duplicate_exposures) > 0) {
    stop(
        "Duplicate exposure rows were found within at least one cohort."
    )
}

# Check that all expected exposures are present -------------------------------
missing_exposures <- setdiff(
    exposure_order,
    unique(vif_results$exposure)
)

unexpected_exposures <- setdiff(
    unique(vif_results$exposure),
    exposure_order
)

if (length(missing_exposures) > 0) {
    stop(
        "Expected exposures missing from the released VIF files: ",
        paste(missing_exposures, collapse = ", ")
    )
}

if (length(unexpected_exposures) > 0) {
    stop(
        "Unexpected exposures found in the released VIF files: ",
        paste(unexpected_exposures, collapse = ", ")
    )
}

# Convert results to wide format ----------------------------------------------
print("Format VIF results")

vif_wide <- vif_results |>
    mutate(
        exposure = factor(
            exposure,
            levels = exposure_order
        ),
        r_squared = if_else(
            is.na(r_squared),
            "",
            sprintf("%.3f", r_squared)
        ),
        vif = if_else(
            is.na(vif),
            "",
            sprintf("%.2f", vif)
        )
    ) |>
    arrange(exposure) |>
    mutate(
        exposure = as.character(exposure)
    ) |>
    pivot_wider(
        names_from = cohort,
        values_from = c(
            r_squared,
            vif
        )
    )

# Order R² and VIF columns within each cohort
result_columns <- unlist(
    map(
        cohorts,
        function(cohort) {
            c(
                paste0("r_squared_", cohort),
                paste0("vif_", cohort)
            )
        }
    )
)

vif_wide <- vif_wide |>
    select(
        exposure,
        exposure_label,
        all_of(result_columns)
    )

# Create the cohort-level number-of-practices row -----------------------------
n_row <- n_practices |>
    mutate(
        r_squared = format(
            n_practices_midpoint6,
            big.mark = "",
            scientific = FALSE,
            trim = TRUE
        ),
        # CSV files cannot merge cells, so the corresponding VIF cell is blank
        vif = ""
    ) |>
    select(
        cohort,
        r_squared,
        vif
    ) |>
    pivot_wider(
        names_from = cohort,
        values_from = c(
            r_squared,
            vif
        )
    ) |>
    mutate(
        exposure = "n_practices_midpoint6",
        exposure_label = "Number of practices",
        .before = 1
    ) |>
    select(
        exposure,
        exposure_label,
        all_of(result_columns)
    )

# Combine the practice-count row with the VIF results -------------------------
table_data <- bind_rows(
    n_row,
    vif_wide
)

# Save combined results as CSV ------------------------------------------------
print("Save formatted VIF results")

output_path <- file.path(
    output_dir,
    "vif_mutually_adjusted_exposures_all_cohorts.csv"
)

write_csv(
    table_data,
    output_path,
    na = ""
)

message(
    "Formatted VIF results saved to: ",
    output_path
)