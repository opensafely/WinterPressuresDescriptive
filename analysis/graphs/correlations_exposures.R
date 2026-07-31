# Examine correlations between candidate exposures for the mutually adjusted model

# Load libraries --------------------------------------------------------------
print("Load libraries")

library(tidyverse)
library(here)
library(fs)

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")

# Specify command arguments ---------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    cohort <- "postcovid1"
} else {
    cohort <- args[[1]]
}

message("Cohort: ", cohort)

# Load active analyses ---------------------------------------------------------
print("Load active analyses")

active_analyses <- readr::read_rds("lib/active_analyses.rds")

# Specify paths and settings --------------------------------------------------
input_path <- here::here(
    "output",
    "dataset_clean",
    paste0("input_", cohort, "_clean.rds")
)

output_dir <- here::here("output", "correlations", cohort)
dir_create(output_dir)

# Pairs at or above this absolute correlation are written to a separate table.
high_correlation_cutoff <- 0.70

# Load data -------------------------------------------------------------------
print("Load data")

input <- read_rds(input_path)

# Define candidate exposures --------------------------------------------------
# Keep this list broad for the OpenSAFELY release. It includes variables that
# may not ultimately enter the mutually adjusted model, so model selection and
# narrower heatmaps can be completed after the correlation outputs are released.

candidate_exposures <- list(
    practice = c(
        "list_size",
        "practice_region",
        "practice_rurality",
        "cons_mean"
    ),
    age_and_sex = c(
        "age_0_4",
        "age_5_11",
        "age_12_17",
        "age_18_29",
        "age_30_44",
        "age_45_54",
        "age_55_64",
        "age_65_74",
        "age_75_79",
        "age_80",
        "age_missing",
        "sex_male",
        "sex_female",
        "sex_missing"
    ),
    ethnicity = c(
        "ethnicity_white",
        "ethnicity_mixed",
        "ethnicity_asian",
        "ethnicity_black",
        "ethnicity_other",
        "ethnicity_missing"
    ),
    deprivation = c(
        "imd_1_most",
        "imd_2",
        "imd_3",
        "imd_4",
        "imd_5_least",
        "imd_missing"
    ),
    # morbidity = c(
    #     "cms_af",
    #     "cms_alcohol",
    #     "cms_anxdep",
    #     "cms_asthma",
    #     "cms_cancer",
    #     "cms_chd",
    #     "cms_ckd",
    #     "cms_constip",
    #     "cms_copd",
    #     "cms_ctd",
    #     "cms_dem",
    #     "cms_diabetes",
    #     "cms_epilepsy",
    #     "cms_hl",
    #     "cms_hf",
    #     "cms_htn",
    #     "cms_ibs",
    #     "cms_oa",
    #     "cms_psych",
    #     "cms_stia"
    # ),
    other_health = c(
        "obesity",
        "carehome"
    ),
    smoking = c(
        "smoking_current",
        "smoking_ever",
        "smoking_never",
        "smoking_missing"
    )
    # vaccination = c(
    #     "vax_flu_y",
    #     "vax_pneum_y"
    # )
)

category_labels <- c(
    practice = "Practice characteristics",
    age_and_sex = "Age and sex",
    ethnicity = "Ethnicity",
    deprivation = "Index of Multiple Deprivation",
    # morbidity = "Long-term conditions",
    other_health = "Other health characteristics",
    smoking = "Smoking status",
    vaccination = "Vaccination"
)

exposure_names <- unique(unlist(candidate_exposures, use.names = FALSE))
missing_exposures <- setdiff(exposure_names, names(input))

if (length(missing_exposures) > 0) {
    warning(
        "The following candidate exposures were not found and will be omitted: ",
        paste(missing_exposures, collapse = ", ")
    )
}

exposure_names <- intersect(exposure_names, names(input))

if (length(exposure_names) < 2) {
    stop("Fewer than two candidate exposures were found in the input data.")
}

input <- input %>%
    select(practice_id, all_of(exposure_names)) %>%
    distinct(practice_id, .keep_all = TRUE)

# Define the reduced exposure set used in the mutually adjusted model.
# Edit this vector whenever exposures are added to or removed from that model.
# Categorical exposures are expanded to their k - 1 indicator columns below.
# Extract mutually adjusted exposures from active analyses --------------------

mutually_adjusted_exposure_string <- active_analyses %>%
    filter(
        analysis_type == "mutually_adjusted"
    ) %>%
    pull(exposure) %>%
    first()

mutually_adjusted_exposures <- mutually_adjusted_exposure_string %>%
    str_split(
        pattern = fixed(";")
    ) %>%
    unlist() %>%
    trimws() %>%
    unique()

non_candidate_exposures <- setdiff(
    mutually_adjusted_exposures,
    exposure_names
)

if (length(non_candidate_exposures) > 0) {
    stop(
        paste0(
            "Mutually adjusted exposures not found in candidate_exposures: ",
            paste(non_candidate_exposures, collapse = ", ")
        )
    )
}

# Convert one-column matrices (for example, variables created with scale()) to ordinary atomic vectors so they can be written safely to CSV later.
matrix_columns <- names(input)[map_lgl(input, is.matrix)]

if (length(matrix_columns) > 0) {
    invalid_matrix_columns <- matrix_columns[
        map_int(input[matrix_columns], ncol) != 1
    ]

    if (length(invalid_matrix_columns) > 0) {
        stop(
            "The following exposures contain matrices with more than one ",
            "column: ",
            paste(invalid_matrix_columns, collapse = ", ")
        )
    }

    input <- input %>%
        mutate(
            across(
                all_of(matrix_columns),
                ~ as.vector(.x[, 1])
            )
        )
}

list_columns <- names(input)[map_lgl(input, is.list)]

if (length(list_columns) > 0) {
    stop(
        "The following exposures are list columns and need to be converted ",
        "to ordinary vectors: ",
        paste(list_columns, collapse = ", ")
    )
}

# Prepare categorical exposures ----------------------------------------------
# Region and rurality are represented by k - 1 indicator variables, as they
# would be in the regression design matrix. The intended reference categories
# are used when their labels are present in the data.

if ("practice_region" %in% names(input)) {
    input <- input %>%
        mutate(practice_region = factor(practice_region))

    if ("East" %in% levels(input$practice_region)) {
        input <- input %>%
            mutate(practice_region = relevel(practice_region, ref = "East"))
    } else {
        warning(
            "'East' was not found as a practice_region level; ",
            "the first observed level will be used as the reference."
        )
    }
}

if ("practice_rurality" %in% names(input)) {
    input <- input %>%
        mutate(practice_rurality = factor(practice_rurality))

    if ("Urban conurbation" %in% levels(input$practice_rurality)) {
        input <- input %>%
            mutate(
                practice_rurality = relevel(
                    practice_rurality,
                    ref = "Urban conurbation"
                )
            )
    } else {
        warning(
            "'Urban conurbation' was not found as a practice_rurality level; ",
            "the first observed level will be used as the reference."
        )
    }
}

categorical_exposures <- intersect(
    c("practice_region", "practice_rurality"),
    names(input)
)

continuous_exposures <- setdiff(
    names(input),
    c("practice_id", categorical_exposures)
)

# Create k - 1 indicator columns without dropping rows with missing values.
# model.matrix() is not used here because it can omit incomplete rows before
# constructing its output, leading to a different row count from the main data.
create_indicator_columns <- function(x, prefix) {
    nonreference_levels <- levels(x)[-1]

    if (length(nonreference_levels) == 0) {
        return(tibble(.rows = length(x)))
    }

    map_dfc(
        nonreference_levels,
        function(category) {
            indicator_name <- paste0(prefix, category)
            indicator_value <- if_else(
                is.na(x),
                NA_real_,
                as.numeric(x == category)
            )

            tibble(!!indicator_name := indicator_value)
        }
    )
}

categorical_matrices <- list()

if ("practice_region" %in% categorical_exposures) {
    categorical_matrices$region <- create_indicator_columns(
        input$practice_region,
        "Region: "
    )
}

if ("practice_rurality" %in% categorical_exposures) {
    categorical_matrices$rurality <- create_indicator_columns(
        input$practice_rurality,
        "Rurality: "
    )
}

if (length(categorical_matrices) > 0) {
    categorical_matrix <- reduce(
        categorical_matrices,
        bind_cols,
        .init = tibble(.rows = nrow(input))
    )
} else {
    categorical_matrix <- tibble(.rows = nrow(input))
}

stopifnot(nrow(categorical_matrix) == nrow(input))

correlation_data <- bind_cols(
    input %>% select(all_of(continuous_exposures)),
    categorical_matrix
)

# Remove all-missing and constant variables -----------------------------------
valid_variable <- function(x) {
    observed <- x[!is.na(x)]
    length(observed) > 1 && n_distinct(observed) > 1
}

valid_variables <- names(correlation_data)[
    map_lgl(correlation_data, valid_variable)
]

removed_variables <- setdiff(names(correlation_data), valid_variables)

if (length(removed_variables) > 0) {
    warning(
        "All-missing or constant variables omitted: ",
        paste(removed_variables, collapse = ", ")
    )
}

correlation_data <- correlation_data %>%
    select(all_of(valid_variables))

if (ncol(correlation_data) < 2) {
    stop("Fewer than two non-constant exposure columns remain.")
}

# Define readable labels and plotting groups ----------------------------------
variable_labels <- c(
    list_size = "Practice list size",
    cons_mean = "Consultation rate",
    age_0_4 = "Age 0-4",
    age_5_11 = "Age 5-11",
    age_12_17 = "Age 12-17",
    age_18_29 = "Age 18-29",
    age_30_44 = "Age 30-44",
    age_45_54 = "Age 45-54",
    age_55_64 = "Age 55-64",
    age_65_74 = "Age 65-74",
    age_75_79 = "Age 75-79",
    age_80 = "Age 80+",
    age_missing = "Age missing",
    sex_male = "Male",
    sex_female = "Female",
    sex_missing = "Sex missing",
    ethnicity_white = "White ethnicity",
    ethnicity_mixed = "Mixed ethnicity",
    ethnicity_asian = "Asian ethnicity",
    ethnicity_black = "Black ethnicity",
    ethnicity_other = "Other ethnicity",
    ethnicity_missing = "Ethnicity missing",
    imd_1_most = "Most deprived",
    imd_2 = "IMD 2",
    imd_3 = "IMD 3",
    imd_4 = "IMD 4",
    imd_5_least = "Least deprived",
    imd_missing = "IMD missing",
    obesity = "Obesity",
    carehome = "Care home resident",
    smoking_current = "Current smoking",
    smoking_ever = "Former smoking",
    smoking_never = "Never smoking",
    smoking_missing = "Smoking missing",
    vax_flu_y = "Influenza vaccination",
    vax_pneum_y = "Pneumococcal vaccination"
)

display_label <- function(x) {
    case_when(
        x %in% names(variable_labels) ~ unname(variable_labels[x]),
        str_starts(x, "Region: ") ~ x,
        str_starts(x, "Rurality: ") ~ x,
        TRUE ~ str_replace_all(x, "_", " ")
    )
}

region_columns <- names(correlation_data)[
    str_starts(names(correlation_data), "Region: ")
]
rurality_columns <- names(correlation_data)[
    str_starts(names(correlation_data), "Rurality: ")
]

plot_groups <- list(
    practice = intersect(
        c("list_size", region_columns, rurality_columns, "cons_mean"),
        names(correlation_data)
    ),
    age_and_sex = intersect(
        candidate_exposures$age_and_sex,
        names(correlation_data)
    ),
    ethnicity = intersect(
        candidate_exposures$ethnicity,
        names(correlation_data)
    ),
    deprivation = intersect(
        candidate_exposures$deprivation,
        names(correlation_data)
    ),
    morbidity = intersect(
        candidate_exposures$morbidity,
        names(correlation_data)
    ),
    other_health = intersect(
        candidate_exposures$other_health,
        names(correlation_data)
    ),
    smoking = intersect(
        candidate_exposures$smoking,
        names(correlation_data)
    ),
    vaccination = intersect(
        candidate_exposures$vaccination,
        names(correlation_data)
    )
)

all_variables_ordered <- unique(
    unlist(plot_groups, use.names = FALSE)
)

# Expand the categorical exposures in the reduced model to the corresponding
# indicator columns created above.
mutually_adjusted_columns <- mutually_adjusted_exposures %>%
    map(function(exposure) {
        if (exposure == "practice_region") {
            region_columns
        } else if (exposure == "practice_rurality") {
            rurality_columns
        } else {
            exposure
        }
    }) %>%
    unlist(use.names = FALSE) %>%
    unique()

missing_mutually_adjusted_columns <- setdiff(
    mutually_adjusted_columns,
    names(correlation_data)
)

if (length(missing_mutually_adjusted_columns) > 0) {
    warning(
        "The following mutually adjusted model columns were not available ",
        "or were constant and will be omitted from the reduced heatmap: ",
        paste(missing_mutually_adjusted_columns, collapse = ", ")
    )
}

mutually_adjusted_columns <- intersect(
    mutually_adjusted_columns,
    names(correlation_data)
)

# Correlation and plotting functions ------------------------------------------
calculate_correlations <- function(df, variables) {
    cor(
        df[variables],
        use = "pairwise.complete.obs",
        method = "spearman"
    )
}

make_correlation_plot <- function(
  correlation_matrix,
  plot_title,
  text_size = 8
) {
    variable_order <- colnames(correlation_matrix)

    plot_data <- correlation_matrix %>%
        as.data.frame(check.names = FALSE) %>%
        rownames_to_column("variable_1") %>%
        pivot_longer(
            -variable_1,
            names_to = "variable_2",
            values_to = "correlation"
        ) %>%
        mutate(
            variable_1 = factor(variable_1, levels = rev(variable_order)),
            variable_2 = factor(variable_2, levels = variable_order)
        )

    axis_labels <- setNames(
        map_chr(variable_order, display_label),
        variable_order
    )

    ggplot(
        plot_data,
        aes(x = variable_2, y = variable_1, fill = correlation)
    ) +
        geom_tile(color = "white", linewidth = 0.15) +
        scale_fill_gradient2(
            low = "#2166AC",
            mid = "white",
            high = "#B2182B",
            midpoint = 0,
            limits = c(-1, 1),
            na.value = "grey85",
            name = "Spearman\ncorrelation"
        ) +
        scale_x_discrete(labels = axis_labels) +
        scale_y_discrete(labels = axis_labels) +
        coord_equal() +
        labs(
            title = plot_title,
            subtitle = paste0(
                "Region and rurality are represented by k−1 indicator ",
                "variables; reference categories are omitted"
            ),
            x = NULL,
            y = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
            axis.text.x = element_text(
                angle = 55,
                hjust = 1,
                vjust = 1,
                size = text_size
            ),
            axis.text.y = element_text(size = text_size),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(size = 9),
            legend.position = "right"
        )
}

# Generate domain-specific heatmaps -------------------------------------------
print("Generate domain-specific correlation heatmaps")

walk(names(plot_groups), function(group_name) {
    group_variables <- plot_groups[[group_name]]

    if (length(group_variables) < 2) {
        message(
            "Skipping ",
            group_name,
            ": fewer than two valid variables."
        )
        return(invisible(NULL))
    }

    group_correlations <- calculate_correlations(
        correlation_data,
        group_variables
    )

    write_csv(
        group_correlations %>%
            as.data.frame(check.names = FALSE) %>%
            rownames_to_column("exposure"),
        file.path(
            output_dir,
            paste0("correlations_", group_name, "_", cohort, ".csv")
        )
    )

    group_plot <- make_correlation_plot(
        group_correlations,
        paste0(
            category_labels[[group_name]],
            ": exposure correlations (",
            cohort,
            ")"
        ),
        text_size = 9
    )

    ggsave(
        file.path(
            output_dir,
            paste0("heatmap_", group_name, "_", cohort, ".png")
        ),
        group_plot,
        width = 9,
        height = 7,
        dpi = 300
    )
})

# Generate overall heatmap ----------------------------------------------------
print("Generate overall correlation heatmap")

overall_correlations <- calculate_correlations(
    correlation_data,
    all_variables_ordered
)

write_csv(
    overall_correlations %>%
        as.data.frame(check.names = FALSE) %>%
        rownames_to_column("exposure"),
    file.path(
        output_dir,
        paste0("correlations_all_exposures_", cohort, ".csv")
    )
)

overall_plot <- make_correlation_plot(
    overall_correlations,
    paste0(
        "Correlations between candidate mutually adjusted exposures (",
        cohort,
        ")"
    ),
    text_size = 6
)

ggsave(
    file.path(
        output_dir,
        paste0("heatmap_all_exposures_", cohort, ".png")
    ),
    overall_plot,
    width = 15,
    height = 13,
    dpi = 300
)

# Generate reduced mutually adjusted model heatmap ----------------------------
print("Generate mutually adjusted model correlation heatmap")

if (length(mutually_adjusted_columns) > 1) {
    mutually_adjusted_correlations <- calculate_correlations(
        correlation_data,
        mutually_adjusted_columns
    )

    write_csv(
        mutually_adjusted_correlations %>%
            as.data.frame(check.names = FALSE) %>%
            rownames_to_column("exposure"),
        file.path(
            output_dir,
            paste0(
                "correlations_mutually_adjusted_exposures_",
                cohort,
                ".csv"
            )
        )
    )

    mutually_adjusted_plot <- make_correlation_plot(
        mutually_adjusted_correlations,
        paste0(
            "Correlations between mutually adjusted model exposures (",
            cohort,
            ")"
        ),
        text_size = 7
    )

    ggsave(
        file.path(
            output_dir,
            paste0(
                "heatmap_mutually_adjusted_exposures_",
                cohort,
                ".png"
            )
        ),
        mutually_adjusted_plot,
        width = 13,
        height = 11,
        dpi = 300
    )
} else {
    warning(
        "Fewer than two mutually adjusted model columns were available; ",
        "the reduced heatmap was not generated."
    )
}

# Export unique highly correlated pairs ---------------------------------------
print("Export highly correlated exposure pairs")

high_correlation_pairs <- overall_correlations %>%
    as.data.frame(check.names = FALSE) %>%
    rownames_to_column("exposure_1") %>%
    pivot_longer(
        -exposure_1,
        names_to = "exposure_2",
        values_to = "correlation"
    ) %>%
    mutate(
        exposure_1_order = match(exposure_1, all_variables_ordered),
        exposure_2_order = match(exposure_2, all_variables_ordered)
    ) %>%
    filter(
        exposure_1_order < exposure_2_order,
        !is.na(correlation),
        abs(correlation) >= high_correlation_cutoff
    ) %>%
    transmute(
        exposure_1,
        exposure_1_label = map_chr(exposure_1, display_label),
        exposure_2,
        exposure_2_label = map_chr(exposure_2, display_label),
        correlation,
        absolute_correlation = abs(correlation)
    ) %>%
    arrange(desc(absolute_correlation))

write_csv(
    high_correlation_pairs,
    file.path(
        output_dir,
        paste0(
            "high_correlation_pairs_",
            str_replace(as.character(high_correlation_cutoff), "\\.", ""),
            "_",
            cohort,
            ".csv"
        )
    )
)

# Save pairwise sample sizes --------------------------------------------------
# This helps identify correlations based on different numbers of practices.

pairwise_n_matrix <- outer(
    all_variables_ordered,
    all_variables_ordered,
    Vectorize(
        function(variable_1, variable_2) {
            sum(
                complete.cases(
                    correlation_data[[variable_1]],
                    correlation_data[[variable_2]]
                )
            )
        }
    )
)

dimnames(pairwise_n_matrix) <- list(
    all_variables_ordered,
    all_variables_ordered
)

pairwise_n <- pairwise_n_matrix %>%
    as.data.frame(check.names = FALSE) %>%
    rownames_to_column("exposure")

pairwise_n_midpoint6_matrix <- matrix(
    roundmid_num(pairwise_n_matrix),
    nrow = nrow(pairwise_n_matrix),
    ncol = ncol(pairwise_n_matrix),
    dimnames = dimnames(pairwise_n_matrix)
)

pairwise_n_midpoint6 <- pairwise_n_midpoint6_matrix %>%
    as.data.frame(check.names = FALSE) %>%
    rownames_to_column("exposure") %>%
    rename_with(
        ~ paste0(.x, "_midpoint6"),
        -exposure
    )

write_csv(
    pairwise_n,
    file.path(
        output_dir,
        paste0("pairwise_n_all_exposures_", cohort, ".csv")
    )
)

write_csv(
    pairwise_n_midpoint6,
    file.path(
        output_dir,
        paste0("pairwise_n_all_exposures_", cohort, "-midpoint6.csv")
    )
)

# Save a long-format file for flexible post-release filtering and plotting ----
# This contains one row per unique exposure pair, its Spearman correlation and
# the number of practices contributing to that pair.

all_correlation_pairs <- overall_correlations %>%
    as.data.frame(check.names = FALSE) %>%
    rownames_to_column("exposure_1") %>%
    pivot_longer(
        -exposure_1,
        names_to = "exposure_2",
        values_to = "correlation"
    ) %>%
    mutate(
        exposure_1_order = match(exposure_1, all_variables_ordered),
        exposure_2_order = match(exposure_2, all_variables_ordered)
    ) %>%
    filter(exposure_1_order < exposure_2_order) %>%
    rowwise() %>%
    mutate(
        exposure_1_label = display_label(exposure_1),
        exposure_2_label = display_label(exposure_2),
        n_practices = map2_int(exposure_1, exposure_2, ~ as.integer(pairwise_n_matrix[.x, .y])),
        absolute_correlation = abs(correlation)
    ) %>%
    ungroup() %>%
    select(
        exposure_1,
        exposure_1_label,
        exposure_2,
        exposure_2_label,
        correlation,
        absolute_correlation,
        n_practices
    ) %>%
    arrange(desc(absolute_correlation))

write_csv(
    all_correlation_pairs,
    file.path(
        output_dir,
        paste0("correlation_pairs_all_exposures_", cohort, ".csv")
    )
)

message(
    "Correlation analysis completed. Outputs saved to: ",
    output_dir
)
