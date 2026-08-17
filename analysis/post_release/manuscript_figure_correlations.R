# Plot heatmaps from a released all-exposure correlation matrix

# Load libraries --------------------------------------------------------------
print("Load libraries")

library(tidyverse)
library(here)
library(fs)

# Specify paths ---------------------------------------------------------------
print("Specify paths")

source("analysis/specify_paths.R")

# Make post-release directory -------------------------------------------------
print("Make post-release directory")

dir.create("output/post_release", recursive = TRUE, showWarnings = FALSE)
output_folder <- "output/post_release"

# Create output folder for correlation plots.
plot_dir <- file.path(output_folder, "correlations")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Generate correlation heatmaps ----------------------------------------------
# Run this function manually for one released cohort at a time. The script does
# not need to be added to the OpenSAFELY project YAML.

generate_correlation_heatmaps <- function(cohort) {
    message("Cohort: ", cohort)

    # Specify paths and settings ----------------------------------------------
    input_path <- file.path(
        correlations,
        cohort,
        paste0("correlations_all_exposures_", cohort, ".csv")
    )

    output_dir <- plot_dir

    if (!file_exists(input_path)) {
        stop("Released correlation matrix not found: ", input_path)
    }

    dir_create(output_dir)

    # Load active analyses ---------------------------------------------------------
    # This keeps the reduced heatmap consistent with the exposures currently used
    # in the mutually adjusted model.
    print("Load active analyses")

    active_analyses_path <- here::here("lib", "active_analyses.rds")

    if (!file_exists(active_analyses_path)) {
        stop("Active analyses file not found: ", active_analyses_path)
    }

    active_analyses <- readr::read_rds(active_analyses_path)

    mutually_adjusted_exposure_strings <- active_analyses %>%
        filter(analysis_type == "mutually_adjusted") %>%
        pull(exposure) %>%
        unique()

    if (length(mutually_adjusted_exposure_strings) != 1) {
        stop(
            "Expected exactly one unique exposure specification for the ",
            "mutually adjusted analysis, but found ",
            length(mutually_adjusted_exposure_strings),
            "."
        )
    }

    mutually_adjusted_exposures <- mutually_adjusted_exposure_strings %>%
        str_split(pattern = fixed(";")) %>%
        unlist() %>%
        trimws() %>%
        discard(~ .x == "") %>%
        unique()

    # Read and validate the released correlation matrix ---------------------------
    print("Load released correlation matrix")

    correlation_csv <- readr::read_csv(
        input_path,
        show_col_types = FALSE,
        name_repair = "minimal"
    )

    if (ncol(correlation_csv) < 3) {
        stop("The released CSV must contain an exposure column and at least two exposure columns.")
    }

    # The generation script writes row names to the first column as `exposure`.
    # Renaming by position also allows the released file to be read if that first
    # column has been given a different name during disclosure control.
    names(correlation_csv)[1] <- "exposure"

    if (anyNA(correlation_csv$exposure) || any(correlation_csv$exposure == "")) {
        stop("The exposure column contains missing or empty names.")
    }

    if (anyDuplicated(correlation_csv$exposure)) {
        stop("The exposure column contains duplicate names.")
    }

    variable_order <- names(correlation_csv)[-1]

    if (anyDuplicated(variable_order)) {
        stop("The correlation-matrix columns contain duplicate exposure names.")
    }

    missing_rows <- setdiff(variable_order, correlation_csv$exposure)
    extra_rows <- setdiff(correlation_csv$exposure, variable_order)

    if (length(missing_rows) > 0 || length(extra_rows) > 0) {
        stop(
            "The released correlation matrix must have the same exposures in its ",
            "rows and columns. Missing rows: ",
            ifelse(length(missing_rows) == 0, "none", paste(missing_rows, collapse = ", ")),
            "; extra rows: ",
            ifelse(length(extra_rows) == 0, "none", paste(extra_rows, collapse = ", ")),
            "."
        )
    }

    non_numeric_columns <- variable_order[
        !map_lgl(correlation_csv[variable_order], is.numeric)
    ]

    if (length(non_numeric_columns) > 0) {
        stop(
            "The following correlation columns are not numeric: ",
            paste(non_numeric_columns, collapse = ", ")
        )
    }

    correlation_csv <- correlation_csv %>%
        arrange(match(exposure, variable_order)) %>%
        select(exposure, all_of(variable_order))

    correlation_matrix <- correlation_csv %>%
        select(-exposure) %>%
        as.matrix()

    rownames(correlation_matrix) <- correlation_csv$exposure

    if (any(abs(correlation_matrix) > 1 + 1e-8, na.rm = TRUE)) {
        stop("At least one correlation lies outside the expected range of -1 to 1.")
    }

    symmetry_difference <- abs(correlation_matrix - t(correlation_matrix))

    if (any(symmetry_difference > 1e-8, na.rm = TRUE)) {
        warning("The released correlation matrix is not exactly symmetric.")
    }

    # Define candidate exposure groups -------------------------------------------
    # These are kept in the same order as the script that generated the released
    # correlation matrices.
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
    )

    category_labels <- c(
        practice = "Practice characteristics",
        age_and_sex = "Age and sex",
        ethnicity = "Ethnicity",
        deprivation = "Index of Multiple Deprivation",
        other_health = "Other health characteristics",
        smoking = "Smoking status"
    )

    # Define readable labels ------------------------------------------------------
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
        smoking_missing = "Smoking missing"
    )

    display_label <- function(x) {
        case_when(
            x %in% names(variable_labels) ~ unname(variable_labels[x]),
            str_starts(x, "Region: ") ~ x,
            str_starts(x, "Rurality: ") ~ x,
            TRUE ~ str_replace_all(x, "_", " ")
        )
    }

    region_columns <- variable_order[str_starts(variable_order, "Region: ")]
    rurality_columns <- variable_order[str_starts(variable_order, "Rurality: ")]

    plot_groups <- list(
        practice = intersect(
            c("list_size", region_columns, rurality_columns, "cons_mean"),
            variable_order
        ),
        age_and_sex = intersect(candidate_exposures$age_and_sex, variable_order),
        ethnicity = intersect(candidate_exposures$ethnicity, variable_order),
        deprivation = intersect(candidate_exposures$deprivation, variable_order),
        other_health = intersect(candidate_exposures$other_health, variable_order),
        smoking = intersect(candidate_exposures$smoking, variable_order)
    )

    # Expand categorical exposures to the k - 1 indicator columns in the released
    # matrix, matching the design matrix used by the generation script.
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
        variable_order
    )

    if (length(missing_mutually_adjusted_columns) > 0) {
        warning(
            "The following mutually adjusted model columns were not present in ",
            "the released matrix and will be omitted: ",
            paste(missing_mutually_adjusted_columns, collapse = ", ")
        )
    }

    mutually_adjusted_columns <- intersect(
        mutually_adjusted_columns,
        variable_order
    )

    # Plotting functions ----------------------------------------------------------
    subset_correlation_matrix <- function(matrix, variables) {
        matrix[variables, variables, drop = FALSE]
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
                ": fewer than two variables are present in the released matrix."
            )
            return(invisible(NULL))
        }

        group_plot <- make_correlation_plot(
            subset_correlation_matrix(correlation_matrix, group_variables),
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

    overall_plot <- make_correlation_plot(
        correlation_matrix,
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
        mutually_adjusted_plot <- make_correlation_plot(
            subset_correlation_matrix(
                correlation_matrix,
                mutually_adjusted_columns
            ),
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

    message("Correlation heatmaps saved to: ", output_dir)

    invisible(output_dir)
}

# Run one cohort at a time after sourcing this script, for example:
generate_correlation_heatmaps("precovid")
# generate_correlation_heatmaps("postcovid1")
# generate_correlation_heatmaps("postcovid2")
# generate_correlation_heatmaps("postcovid3")