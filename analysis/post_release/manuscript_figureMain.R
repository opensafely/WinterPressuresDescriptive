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
library(patchwork)

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

# Create output folder for plots
plot_dir <- file.path(output_folder, "/forest_plots")
dir.create(plot_dir, showWarnings = FALSE)

# Add plot labels ---------------------------------------------------------
print("Add plot labels")

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

# Define group order for plotting
group_order <- c(
    "Practice region",
    "Practice rurality",
    "List size",
    "Monthly consultation",
    "Age",
    "Sex",
    "Ethnicity",
    "Deprivation",
    "Smoking Status",
    "Obesity",
    "Care home residence"
)

practice_groups <- c(
    "Practice region",
    "Practice rurality",
    "List size",
    "Monthly consultation"
)

case_mix_groups <- setdiff(
    group_order,
    practice_groups
)

# regression <- "negbin"
# sub_group <- "main"
# outcome_names <- c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any")
# cohorts <- c("precovid", "postcovid3")
# practice_char <- "practice"
# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main
plot_irr <- function(regression, sub_group, outcome_names, cohorts, practice_char = "all") {
    # Load data --------------------------------------------------------------------
    print("Load model output")

    df <- readr::read_csv(
        "output/post_release/plot_model_output.csv",
        show_col_types = FALSE
    )

    df <- df %>% mutate(
        outcome = str_remove(outcome, paste0("_", analysis))
    )

    # Filter data ------------------------------------------------------------------
    print("Filter data")

    df <- df %>%
        filter(
            cohort %in% cohorts,
            analysis == sub_group,
            outcome %in% outcome_names,
            model_type == regression,
            grepl("^exp_prop(_|$)", term),
            model %in% c("mdl_age_sex")
        ) %>%
        mutate(
            exposure = if_else(
                term == "exp_prop", exposure, term
            )
        ) %>%
        select(
            cohort,
            analysis,
            exposure,
            outcome,
            model,
            irr,
            lci,
            uci,
            n_obs_midpoint6,
            mad
        ) %>%
        mutate(
            # model aesthetics
            model = factor(
                model,
                levels = c("mdl_crude", "mdl_age_sex"),
                labels = c("Single-characteristic (crude)", "Single-characteristic (age-sex adjusted)")
            ),
            cohort = factor(
                cohort,
                levels = c(
                    "precovid",
                    "postcovid1",
                    "postcovid2",
                    "postcovid3"
                )
            ),
            exposure = factor(exposure) # will control y-axis order later
        )

    # --- Join EXPOSURE labels ---
    exposure_labels <- labels %>%
        filter(!str_detect(term, "apc|ec")) %>%
        select(term, exposure_label = label, group, ref)

    df <- df %>%
        left_join(
            exposure_labels,
            by = c("exposure" = "term")
        ) %>%
        filter(!is.na(exposure_label))

    # --- Join OUTCOME label ---
    outcome_labels <- labels %>%
        select(term, outcome_label = label, outcome_group = group, outcome_ref = ref)

    df <- df %>%
        left_join(
            outcome_labels,
            by = c("outcome" = "term")
        )
    outcome_levels <- df %>%
        distinct(
            outcome_group,
            outcome_label,
            outcome_ref
        ) %>%
        mutate(
            outcome_group = factor(
                outcome_group,
                levels = c(
                    "admitted patient care",
                    "acsc admitted patient care"
                )
            )
        ) %>%
        arrange(
            outcome_group,
            outcome_ref
        ) %>%
        pull(outcome_label)

    df <- df %>%
        mutate(
            outcome_label = factor(
                outcome_label,
                levels = outcome_levels
            )
        )
    outcome_group <- unique(df$outcome_group)[1]

    # --- Join COHORT labels ---
    cohort_labels <- labels %>%
        filter(term %in% cohorts) %>%
        select(term, label)

    df <- df %>%
        left_join(
            cohort_labels,
            by = c("cohort" = "term")
        ) %>%
        rename(cohort_label = label)

    cohort_suffix <- if (identical(
        cohorts,
        c("precovid", "postcovid1", "postcovid2", "postcovid3")
    )) {
        "all"
    } else if (identical(
        cohorts,
        c("precovid", "postcovid3")
    )) {
        "prepost3"
    } else {
        paste(gsub("postcovid", "post", cohorts), collapse = "_")
    }

    # --- Join analysis labels ---
    analysis_label <- labels %>%
        filter(
            term %in% c(sub_group)
        ) %>%
        pull(label)

    # --- Factor setup ---
    df <- df %>%
        mutate(
            cohort_label = factor(
                cohort_label,
                levels = cohort_labels$label
            )
        ) %>%
        mutate(
            group = factor(
                group,
                levels = group_order
            ),
            ref_order = if_else(is.na(ref), Inf, ref)
        ) %>%
        arrange(group, ref_order, outcome_ref)

    practice_char <- match.arg(
        practice_char,
        c("all", "practice", "case_mix")
    )

    # filter the df according to the practice_char argument
    is_ec <- all(str_detect(outcome_names, "^ec"))
    combined_all <- practice_char == "all"

    if (practice_char == "practice") {
        df <- df %>%
            filter(group %in% practice_groups)
    } else if (practice_char == "case_mix") {
        df <- df %>%
            filter(group %in% case_mix_groups)
    }

    table_df <- df %>%
        select(
            cohort,
            cohort_label,
            exposure_label,
            group,
            ref,
            mad
        ) %>%
        distinct() %>%
        arrange(cohort_label, group, ref) %>%
        select(
            cohort_label,
            exposure_label,
            mad
        )
    table_side <- table_df %>%
        pivot_wider(
            names_from = cohort_label,
            values_from = mad
        )

    cohort_cols <- names(table_side)[
        names(table_side) != "exposure_label"
    ]
    n_cohorts <- length(cohort_cols)

    if (n_cohorts == 4) {
        widths <- c(0.65, 0.35)
    } else {
        widths <- c(0.70, 0.30)
    }

    # Calculate median MAD and construct characteristic labels
    table_side <- table_side %>%
        rowwise() %>%
        mutate(
            mad_median = median(
                c_across(all_of(cohort_cols)),
                na.rm = TRUE
            ),
            exposure_label_full = case_when(
                is.na(mad_median) ~ as.character(exposure_label),
                mad_median <= 100 ~ sprintf(
                    "%s (per %.1f%%)",
                    exposure_label,
                    mad_median
                ),
                TRUE ~ sprintf(
                    "%s (per %.0f)",
                    exposure_label,
                    mad_median
                )
            )
        ) %>%
        ungroup() %>%
        mutate(
            exposure_label = as.character(exposure_label)
        )

    # Regions where TPP practices cover <50% of the regional population
    low_coverage_regions <- c(
        "South East",
        "London",
        "West Midlands",
        "North West",
        "North East"
    )

    table_side <- table_side %>%
        mutate(
            exposure_label_full = if_else(
                exposure_label %in% low_coverage_regions,
                paste0(exposure_label_full, "<sup><span style='font-size:10pt;'><b>*</b></span></sup>"),
                exposure_label_full
            )
        )

    # Groups that require a separate subtitle.
    # Single characteristics such as list size and obesity do not need
    # a subtitle because this would repeat the characteristic name.
    groups_with_subtitle <- c(
        "Practice region",
        "Practice rurality",
        "Age",
        "Sex",
        "Ethnicity",
        "Deprivation",
        "Smoking Status"
    )
    # Bold the characteristic name but not its "(per X)" information
    format_single_label <- function(x) {
        if_else(
            str_detect(x, fixed(" (per ")),
            str_replace(
                x,
                "^(.*?)( \\(per .+\\))$",
                "<b>\\1</b>\\2"
            ),
            paste0("<b>", x, "</b>")
        )
    }

    if (combined_all) {
        # Main sections
        practice_section <- "Practice characteristics"

        case_mix_section <- paste0(
            "Patient case-mix ",
            "(% of patients in practice with each characteristic)"
        )

        section_order <- c(
            practice_section,
            case_mix_section
        )

        # Create an identifier for each plotted characteristic
        df <- df %>%
            mutate(
                section = if_else(
                    group %in% practice_groups,
                    practice_section,
                    case_mix_section
                ),
                group_character = as.character(group),
                exposure_character = as.character(exposure_label),
                plot_row = paste(
                    "item",
                    group_character,
                    exposure_character,
                    sep = "|||"
                )
            )

        # One row per characteristic, in the required order
        item_rows <- df %>%
            distinct(
                section,
                group_character,
                exposure_character,
                plot_row,
                ref_order
            ) %>%
            left_join(
                table_side %>%
                    select(
                        exposure_character = exposure_label,
                        exposure_label_full
                    ),
                by = "exposure_character"
            ) %>%
            arrange(
                factor(section, levels = section_order),
                match(group_character, group_order),
                ref_order
            )

        # Construct the hierarchy:
        # section heading -> subgroup heading -> characteristics
        row_structure <- purrr::map_dfr(
            section_order,
            function(section_name) {
                groups_in_section <- group_order[
                    group_order %in%
                        item_rows$group_character[
                            item_rows$section == section_name
                        ]
                ]

                group_rows <- purrr::map_dfr(
                    groups_in_section,
                    function(group_name) {
                        rows_in_group <- item_rows %>%
                            filter(
                                section == section_name,
                                group_character == group_name
                            ) %>%
                            arrange(ref_order)

                        has_subtitle <- group_name %in%
                            groups_with_subtitle

                        characteristic_rows <- rows_in_group %>%
                            transmute(
                                plot_row,
                                y_label = if (has_subtitle) {
                                    paste0(
                                        "&nbsp;&nbsp;&nbsp;",
                                        exposure_label_full
                                    )
                                } else {
                                    format_single_label(exposure_label_full)
                                },
                                row_type = "characteristic"
                            )

                        if (has_subtitle) {
                            bind_rows(
                                tibble(
                                    plot_row = paste(
                                        "group",
                                        group_name,
                                        sep = "|||"
                                    ),
                                    y_label = paste0(
                                        "<b>",
                                        group_name,
                                        "</b>"
                                    ),
                                    row_type = "subgroup"
                                ),
                                characteristic_rows
                            )
                        } else {
                            characteristic_rows
                        }
                    }
                )

                # Give the case-mix heading two rows
                section_rows <- if (section_name == case_mix_section) {
                    tibble(
                        plot_row = c(
                            paste(
                                "section",
                                section_name,
                                "title",
                                sep = "|||"
                            ),
                            paste(
                                "section",
                                section_name,
                                "description",
                                sep = "|||"
                            )
                        ),
                        y_label = c(
                            paste0(
                                "<span style='color:#2F5597;font-size:11pt;'>",
                                "<b>Patient case-mix</b>",
                                "</span>"
                            ),
                            paste0(
                                "<span style='color:#2F5597;font-size:10pt;'>",
                                "(% of patients in practice with each characteristic)",
                                "</span>"
                            )
                        ),
                        row_type = "section"
                    )
                } else {
                    tibble(
                        plot_row = paste(
                            "section",
                            section_name,
                            "title",
                            sep = "|||"
                        ),
                        y_label = paste0(
                            "<span style='color:#2F5597;font-size:11pt;'>",
                            "<b>",
                            section_name,
                            "</b>",
                            "</span>"
                        ),
                        row_type = "section"
                    )
                }

                bind_rows(
                    section_rows,
                    group_rows
                )
            }
        )

        # ggplot displays the first factor level at the bottom
        row_levels <- rev(row_structure$plot_row)

        y_labels <- row_structure$y_label
        names(y_labels) <- row_structure$plot_row

        df <- df %>%
            mutate(
                plot_row = factor(
                    plot_row,
                    levels = row_levels
                )
            )

        # Data used to add pale bands to the two main section rows
        section_band_df <- row_structure %>%
            filter(row_type == "section") %>%
            transmute(
                plot_row = factor(
                    plot_row,
                    levels = row_levels
                )
            )
    } else {
        # Add subgroup headings to the separate practice and case-mix plots
        df <- df %>%
            mutate(
                group_character = as.character(group),
                exposure_character = as.character(exposure_label),
                plot_row = paste(
                    "item",
                    group_character,
                    exposure_character,
                    sep = "|||"
                )
            )

        # Obtain one row for each characteristic
        item_rows <- df %>%
            distinct(
                group_character,
                exposure_character,
                plot_row,
                ref_order
            ) %>%
            left_join(
                table_side %>%
                    select(
                        exposure_character = exposure_label,
                        exposure_label_full
                    ),
                by = "exposure_character"
            ) %>%
            arrange(
                match(group_character, group_order),
                ref_order
            )

        # Retain only groups present in the current figure
        groups_in_plot <- group_order[
            group_order %in% item_rows$group_character
        ]

        # Construct:
        # subgroup heading -> indented characteristics
        row_structure <- purrr::map_dfr(
            groups_in_plot,
            function(group_name) {
                has_subtitle <- group_name %in% groups_with_subtitle

                characteristic_rows <- item_rows %>%
                    filter(
                        group_character == group_name
                    ) %>%
                    arrange(ref_order) %>%
                    transmute(
                        plot_row,
                        y_label = if (has_subtitle) {
                            paste0(
                                "&nbsp;&nbsp;&nbsp;",
                                exposure_label_full
                            )
                        } else {
                            format_single_label(exposure_label_full)
                        },
                        row_type = "characteristic"
                    )

                group_display <- if_else(
                    group_name == "Smoking Status",
                    "Smoking status",
                    group_name
                )

                if (has_subtitle) {
                    bind_rows(
                        tibble(
                            plot_row = paste(
                                "group",
                                group_name,
                                sep = "|||"
                            ),
                            y_label = paste0(
                                "<span style='font-size:10pt;'>",
                                "<b>",
                                group_display,
                                "</b>",
                                "</span>"
                            ),
                            row_type = "subgroup"
                        ),
                        characteristic_rows
                    )
                } else {
                    characteristic_rows
                }
            }
        )

        # The first factor level appears at the bottom of a ggplot
        row_levels <- rev(row_structure$plot_row)

        y_labels <- row_structure$y_label
        names(y_labels) <- row_structure$plot_row

        df <- df %>%
            mutate(
                plot_row = factor(
                    plot_row,
                    levels = row_levels
                )
            )
    }

    # Make forest plot -----------------------------------------------------------
    print("Make forest plot")

    title_prefix <- if (practice_char == "all") {
        "General practice characteristics"
    } else if (practice_char == "practice") {
        "Practice characteristics"
    } else {
        "Patient case-mix (% of patients in practice with each characteristic)"
    }
    title_text <- paste0(
        title_prefix,
        " and ",
        "**",
        tolower(outcome_group),
        "**",
        " in ",
        "**",
        tolower(analysis_label),
        "**"
    )

    if (is_ec) {
        x_limits <- c(0.7, 1.8)
        x_breaks <- c(0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8)
    } else if (practice_char %in% c("all", "practice")) {
        # Wider range required because practice characteristics are included
        x_limits <- c(0.6, 1.5)
        x_breaks <- c(0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.3, 1.5)
    } else {
        x_limits <- c(0.9, 1.3)
        x_breaks <- c(0.9, 1.0, 1.1, 1.2, 1.3)
    }

    # Plot width and height

    if (combined_all) {
        ci_cap <- 0.2
        panel_width <- if (is_ec) 5 else 4
        plot_height <- if (is_ec) 11 else 12
    } else if (practice_char == "practice") {
        ci_cap <- 0.4
        panel_width <- 4.5
        plot_height <- 10
    } else {
        ci_cap <- 0.4
        panel_width <- 3.8
        plot_height <- 11
    }

    n_outcomes <- length(outcome_names)

    ncol <- if (combined_all) {
        n_outcomes
    } else if (n_outcomes == 2) {
        n_outcomes
    } else {
        ceiling(n_outcomes / 2)
    }

    plot_height <- if (n_outcomes < 3 && !is_ec) {
        6
    } else {
        plot_height
    }

    plot_width <- panel_width * ncol

    caption_width <- round(13 * plot_width)

    # Clip confidence intervals to plotting range
    df <- df %>%
        mutate(
            lci_plot = pmax(lci, x_limits[1]),
            uci_plot = pmin(uci, x_limits[2])
        )

    model_note <- paste0(
        "Points show incidence rate ratios (IRRs) with 95% confidence intervals. ",
        "Estimates from random-intercept ",
        ifelse(regression == "negbin", "negative binomial", "Poisson"),
        " regression models."
    )

    mad_note <- paste0(
        "Continuous characteristics were scaled by the cohort-specific median ",
        "absolute deviation (MAD). Values in parentheses show the median MAD ",
        "across the included cohorts and indicate the increase represented by each IRR."
    )

    region_note <- paste0(
        "* TPP practices cover less than 50% ",
        "of the total regional population."
    )

    caption_parts <- if (practice_char %in% c("all", "practice")) {
        c(model_note, region_note, mad_note)
    } else {
        c(model_note, mad_note)
    }

    caption_text <- paste(
        str_wrap(caption_parts, width = caption_width),
        collapse = "\n"
    )

    facet_spec <- if (combined_all) {
        facet_grid(
            cols = vars(outcome_label)
        )
    } else {
        facet_wrap(
            ~outcome_label,
            ncol = ncol
        )
    }

    y_title <- if (combined_all) {
        NULL
    } else if (practice_char == "all") {
        "Characteristics (median MAD across cohorts)"
    } else if (practice_char == "practice") {
        "Practice characteristics"
    } else {
        "Patient case-mix (median MAD across cohorts)"
    }

    p <- ggplot(
        df,
        aes(
            x = irr,
            y = plot_row,
            colour = cohort_label,
            alpha = model,
            group = interaction(cohort_label, model)
        )
    )

    if (combined_all) {
        p <- p +
            geom_tile(
                data = section_band_df,
                aes(
                    x = mean(x_limits),
                    y = plot_row
                ),
                inherit.aes = FALSE,
                width = diff(x_limits),
                height = 1.2,
                fill = "#EAF1F7",
                colour = NA
            )
    }

    p <- p +
        geom_vline(
            xintercept = 1,
            colour = "grey60",
            linetype = "dashed",
            linewidth = 0.6
        ) +
        geom_errorbar(
            aes(
                xmin = lci_plot,
                xmax = uci_plot
            ),
            orientation = "y",
            position = position_dodge(width = 0.5),
            width = ci_cap,
            linewidth = 0.55
        ) +
        geom_point(
            position = position_dodge(width = 0.5),
            size = 2.0
        ) +
        # --- Scales ---
        scale_alpha_manual(
            values = c("Single-characteristic (crude)" = 0.35, "Single-characteristic (age-sex adjusted)" = 1),
            name = "Model"
        ) +
        scale_colour_manual(
            name = "Cohort",
            values = c(
                "Pre-COVID19" = "#F8766D",
                "2022/23" = "#7CAE00",
                "2023/24" = "#00BFC4",
                "2024/25" = "#C77CFF"
            ),
            drop = FALSE
        ) +
        scale_y_discrete(
            labels = y_labels,
            drop = FALSE
        ) +
        scale_size_manual(
            values = c("Single-characteristic (crude)" = 1.6, "Single-characteristic (age-sex adjusted)" = 2.2),
            name = "Model"
        ) +
        scale_x_log10(
            breaks = x_breaks,
            labels = scales::number_format(accuracy = 0.01)
        ) +
        coord_cartesian(
            xlim = x_limits
        ) +
        facet_spec +
        labs(
            title = title_text,
            x = "Incidence rate ratio (IRR)",
            y = y_title,
            linetype = "Model",
            caption = caption_text
        ) +
        theme_bw() +
        theme(
            plot.title = ggtext::element_markdown(
                hjust = 0,
                size = 12,
                margin = margin(b = 8)
            ),
            strip.text.x = element_text(
                size = 12,
                face = "bold"
            ),
            plot.caption = element_text(
                size = 8,
                hjust = 0,
                colour = "grey30",
                lineheight = 1.2,
                margin = margin(t = 8)
            ),
            legend.position = "bottom",
            axis.text.y = ggtext::element_markdown(size = 9),
            plot.margin = margin(t = 14, r = 10, b = 14, l = 10),
            legend.box = "vertical",
            legend.key.width = unit(12, "pt"),
            legend.key.height = unit(10, "pt"),
            legend.spacing.x = unit(4, "pt"),
            legend.spacing.y = unit(2, "pt"),
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 9)
        ) +
        guides(
            colour = guide_legend(
                nrow = 1,
                byrow = TRUE,
                override.aes = list(size = 2, alpha = 1)
            ),
            alpha = guide_legend(
                title = "Model",
                override.aes = list(colour = "black")
            ),
            size = "none" # hide duplicate legend
        )

    ggsave(
        filename = file.path(
            plot_dir,
            paste0("forest-", sub_group, "-", regression, "-", paste(outcome_names, collapse = "_"), "-", practice_char, "-", cohort_suffix, ".png")
        ),
        plot = p,
        width = plot_width,
        height = plot_height,
        dpi = 300
    )
}
# Run main analyses
plot_irr("negbin", "main", c("ec", "ec_acsc_any"), c("postcovid3"), "all")
plot_irr("negbin", "main", c("apc", "apc_unpl", "apc_plan"), c("precovid", "postcovid3"), "all")
plot_irr("negbin", "main", c("apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "all")

plot_irr("negbin", "sub_asth", c("ec", "ec_acsc_any"), c("postcovid3"), "all")
plot_irr("negbin", "sub_asth", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "practice")
plot_irr("negbin", "sub_asth", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "case_mix")

plot_irr("negbin", "sub_copd", c("ec", "ec_acsc_any"), c("postcovid3"), "all")
plot_irr("negbin", "sub_copd", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "practice")
plot_irr("negbin", "sub_copd", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "case_mix")

plot_irr("negbin", "sub_htn", c("ec", "ec_acsc_any"), c("postcovid3"), "all")
plot_irr("negbin", "sub_htn", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "practice")
plot_irr("negbin", "sub_htn", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "case_mix")

plot_irr("negbin", "sub_diab", c("ec", "ec_acsc_any"), c("postcovid3"), "all")
plot_irr("negbin", "sub_diab", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "practice")
plot_irr("negbin", "sub_diab", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "case_mix")

plot_irr("negbin", "sub_sevmh", c("ec", "ec_acsc_any"), c("postcovid3"), "all")
plot_irr("negbin", "sub_sevmh", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "practice")
plot_irr("negbin", "sub_sevmh", c("apc", "apc_unpl", "apc_plan", "apc_acsc_any", "apc_unpl_acsc_any", "apc_plan_acsc_any"), c("precovid", "postcovid3"), "case_mix")



# Run all analyses for supplementary figures
# Analyses to plot
all_analyses <- c(
    "main",
    "sub_asth",
    "sub_copd",
    "sub_htn",
    "sub_diab",
    "sub_sevmh"
)

# Cohorts to include
ec_cohorts <- c(
    "postcovid1",
    "postcovid2",
    "postcovid3"
)

apc_cohorts <- c(
    "precovid",
    "postcovid1",
    "postcovid2",
    "postcovid3"
)

# Outcomes
ec_outcomes <- c(
    "ec",
    "ec_acsc_any"
)

apc_outcomes <- c(
    "apc",
    "apc_unpl",
    "apc_plan",
    "apc_acsc_any",
    "apc_unpl_acsc_any",
    "apc_plan_acsc_any"
)

apc_all_cause_outcomes <- c(
    "apc",
    "apc_unpl",
    "apc_plan"
)

apc_acsc_outcomes <- c(
    "apc_acsc_any",
    "apc_unpl_acsc_any",
    "apc_plan_acsc_any"
)

# Generate all plots
purrr::walk(
    all_analyses,
    function(current_analysis) {
        # EC: practice characteristics and case-mix together
        plot_irr(
            regression = "negbin",
            sub_group = current_analysis,
            outcome_names = ec_outcomes,
            cohorts = ec_cohorts,
            practice_char = "all"
        )

        # APC, all-cause outcomes: practice characteristics and case-mix together
        plot_irr(
            regression = "negbin",
            sub_group = current_analysis,
            outcome_names = apc_all_cause_outcomes,
            cohorts = apc_cohorts,
            practice_char = "all"
        )

        # APC, ACSC-related outcomes: practice characteristics and case-mix together
        plot_irr(
            regression = "negbin",
            sub_group = current_analysis,
            outcome_names = apc_acsc_outcomes,
            cohorts = apc_cohorts,
            practice_char = "all"
        )

        # APC, all: practice characteristics
        plot_irr(
            regression = "negbin",
            sub_group = current_analysis,
            outcome_names = apc_outcomes,
            cohorts = apc_cohorts,
            practice_char = "practice"
        )

        # APC, all: patient case-mix
        plot_irr(
            regression = "negbin",
            sub_group = current_analysis,
            outcome_names = apc_outcomes,
            cohorts = apc_cohorts,
            practice_char = "case_mix"
        )
    }
)
