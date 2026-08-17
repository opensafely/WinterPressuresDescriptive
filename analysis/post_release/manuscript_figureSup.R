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

# This file is used to specify paths and is in .gitignore to keep local
# information private. Copy specify_paths_example.R, remove "_example" from
# the filename, and add the required paths before running this script.
source("analysis/specify_paths.R")

# Make post-release directories ------------------------------------------------

print("Make post-release directories")

output_folder <- "output/post_release"
plot_dir <- file.path(output_folder, "forest_plots_mutually_adjusted")

dir.create(
    output_folder,
    recursive = TRUE,
    showWarnings = FALSE
)

dir.create(
    plot_dir,
    recursive = TRUE,
    showWarnings = FALSE
)

# Add plot labels --------------------------------------------------------------

print("Add plot labels")

labels <- readr::read_csv(
    "lib/labels.csv",
    show_col_types = FALSE
)

# Define the order of characteristic groups
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

# Plot mutually adjusted IRRs --------------------------------------------------

# regression: "negbin" or "poisson"
# sub_group: analysis name, e.g. "main"
# outcome_names: vector of outcome terms
# cohorts: vector of cohort terms
plot_irr_sup <- function(
  regression,
  sub_group,
  outcome_names,
  cohorts
) {
    # Load data ----------------------------------------------------------------

    print("Load model output")

    df <- readr::read_csv(
        "output/post_release/plot_model_output.csv",
        show_col_types = FALSE
    ) %>%
        mutate(
            outcome = str_remove(
                outcome,
                paste0("_", analysis)
            )
        )

    # Filter data --------------------------------------------------------------

    print("Filter data")

    df <- df %>%
        filter(
            cohort %in% cohorts,
            analysis == sub_group,
            outcome %in% outcome_names,
            model_type == regression,
            grepl("^exp(_|$)", term),
            model == "mdl_mut_adj"
        ) %>%
        mutate(
            # In a mutually adjusted model, exposure can contain a
            # semicolon-separated list. In that situation, term identifies
            # the individual coefficient represented by the row.
            exposure = if_else(
                str_detect(exposure, fixed(";")),
                term,
                exposure
            ),
            exposure = str_remove(
                exposure,
                "^exp_num_"
            )
        ) %>%
        select(
            cohort,
            analysis,
            exposure,
            outcome,
            model,
            term,
            irr,
            lci,
            uci,
            n_obs_midpoint6,
            mad
        ) %>%
        mutate(
            model = factor(
                model,
                levels = "mdl_mut_adj",
                labels = "Mutually adjusted"
            ),
            cohort = factor(
                cohort,
                levels = c(
                    "precovid",
                    "postcovid1",
                    "postcovid2",
                    "postcovid3"
                )
            )
        )

    if (nrow(df) == 0) {
        stop(
            "No mutually adjusted model estimates remained after filtering."
        )
    }

    # Join exposure labels -----------------------------------------------------

    exposure_labels <- labels %>%
        filter(!str_detect(term, "apc|ec")) %>%
        select(
            term,
            exposure_label = label,
            group,
            ref
        )

    df <- df %>%
        left_join(
            exposure_labels,
            by = c("exposure" = "term")
        )

    unmatched_exposures <- df %>%
        filter(is.na(exposure_label)) %>%
        distinct(exposure) %>%
        pull(exposure)

    if (length(unmatched_exposures) > 0) {
        warning(
            paste0(
                "The following exposures had no label and were omitted: ",
                paste(unmatched_exposures, collapse = ", ")
            )
        )
    }

    df <- df %>%
        filter(!is.na(exposure_label))

    # Join outcome labels ------------------------------------------------------

    outcome_labels <- labels %>%
        select(
            term,
            outcome_label = label,
            outcome_group = group,
            outcome_ref = ref
        )

    df <- df %>%
        left_join(
            outcome_labels,
            by = c("outcome" = "term")
        )

    # Retain the order supplied in outcome_names. For six APC outcomes, this
    # places the first three outcomes in row 1 and the last three in row 2.
    outcome_levels <- outcome_labels %>%
        filter(term %in% outcome_names) %>%
        mutate(
            outcome_order = match(term, outcome_names)
        ) %>%
        arrange(outcome_order) %>%
        pull(outcome_label) %>%
        unique()

    df <- df %>%
        mutate(
            outcome_label = factor(
                outcome_label,
                levels = outcome_levels
            )
        )

    # Join cohort labels -------------------------------------------------------

    cohort_labels <- labels %>%
        filter(term %in% cohorts) %>%
        mutate(
            cohort_order = match(term, cohorts)
        ) %>%
        arrange(cohort_order) %>%
        select(term, label)

    df <- df %>%
        left_join(
            cohort_labels,
            by = c("cohort" = "term")
        ) %>%
        rename(cohort_label = label) %>%
        mutate(
            cohort_label = factor(
                cohort_label,
                levels = cohort_labels$label
            )
        )

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
        paste(
            gsub("postcovid", "post", cohorts),
            collapse = "_"
        )
    }

    # Join analysis labels -----------------------------------------------------

    analysis_label <- labels %>%
        filter(term == sub_group) %>%
        pull(label) %>%
        first()

    # Set group and row order --------------------------------------------------

    df <- df %>%
        mutate(
            group = factor(
                group,
                levels = group_order
            ),
            ref_order = if_else(
                is.na(ref),
                Inf,
                ref
            )
        ) %>%
        arrange(
            group,
            ref_order,
            outcome_label
        )

    is_ec <- all(str_detect(outcome_names, "^ec"))

    # Calculate median MAD across included cohorts ----------------------------

    table_df <- df %>%
        select(
            cohort_label,
            exposure_label,
            group,
            ref,
            mad
        ) %>%
        distinct() %>%
        arrange(
            cohort_label,
            group,
            ref
        ) %>%
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

    cohort_cols <- intersect(
        as.character(cohort_labels$label),
        names(table_side)
    )

    table_side <- table_side %>%
        rowwise() %>%
        mutate(
            mad_median = median(
                c_across(all_of(cohort_cols)),
                na.rm = TRUE
            ),
            exposure_label_full = case_when(
                is.na(mad_median) ~
                    as.character(exposure_label),
                mad_median <= 100 ~
                    sprintf(
                        "%s (per %.1f%%)",
                        exposure_label,
                        mad_median
                    ),
                TRUE ~
                    sprintf(
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

    # Construct section and subgroup hierarchy --------------------------------

    practice_section <- "Practice characteristics"

    case_mix_section <- paste0(
        "Patient case-mix ",
        "(% of patients in practice with each characteristic)"
    )

    section_order <- c(
        practice_section,
        case_mix_section
    )

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
                    characteristic_rows <- item_rows %>%
                        filter(
                            section == section_name,
                            group_character == group_name
                        ) %>%
                        arrange(ref_order) %>%
                        transmute(
                            plot_row,
                            y_label = paste0(
                                "&nbsp;&nbsp;&nbsp;",
                                exposure_label_full
                            ),
                            row_type = "characteristic"
                        )

                    group_display <- if_else(
                        group_name == "Smoking Status",
                        "Smoking status",
                        group_name
                    )

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
                }
            )

            # The case-mix heading uses two rows so that its belt is taller.
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
                            "<span style='color:#2F5597;",
                            "font-size:11pt;'>",
                            "<b>Patient case-mix</b>",
                            "</span>"
                        ),
                        paste0(
                            "<span style='color:#2F5597;",
                            "font-size:10pt;'>",
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
                        "<span style='color:#2F5597;",
                        "font-size:11pt;'>",
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

    # ggplot places the first factor level at the bottom.
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

    section_band_df <- row_structure %>%
        filter(row_type == "section") %>%
        transmute(
            plot_row = factor(
                plot_row,
                levels = row_levels
            )
        )

    # Plot settings ------------------------------------------------------------

    print("Make forest plot")

    outcome_family <- if (is_ec) {
        "emergency care attendances"
    } else {
        "admitted patient care"
    }

    title_text <- paste0(
        "All practice-level characteristics and **",
        outcome_family,
        "** in **",
        tolower(analysis_label),
        "**"
    )

    if (is_ec) {
        x_limits <- c(0.7, 2.0)
        x_breaks <- c(
            0.7,
            0.8,
            0.9,
            1.0,
            1.2,
            1.4,
            1.6,
            1.8,
            2.0
        )
    } else {
        x_limits <- c(0.6, 1.5)
        x_breaks <- c(
            0.6,
            0.7,
            0.8,
            0.9,
            1.0,
            1.1,
            1.3,
            1.5
        )
    }

    ci_cap <- 0.3

    n_outcomes <- length(outcome_names)

    ncol <- if (is_ec || n_outcomes == 2) {
        n_outcomes
    } else {
        ceiling(n_outcomes / 2)
    }

    n_facet_rows <- ceiling(n_outcomes / ncol)

    if (is_ec) {
        panel_width <- 5
        plot_width <- panel_width * ncol
        plot_height <- 12
    } else {
        # APC: six outcomes arranged as three columns × two rows
        panel_width <- 5
        plot_width <- panel_width * ncol 
        plot_height <- 10 * n_facet_rows 
    }

    caption_width <- round(14 * plot_width)

    # Clip confidence intervals to the plotting range
    df <- df %>%
        mutate(
            lci_plot = pmax(lci, x_limits[1]),
            uci_plot = pmin(uci, x_limits[2])
        )

    caption_text <- str_wrap(
        paste0(
            "All displayed characteristics were included simultaneously ",
            "in each random-intercept ",
            ifelse(
                regression == "negbin",
                "negative binomial",
                "Poisson"
            ),
            " regression model. Points show incidence rate ratios (IRRs) ",
            "with 95% confidence intervals.",
            "\n\n",
            "Continuous characteristics were scaled by the cohort-specific ",
            "median absolute deviation (MAD). Values in parentheses show ",
            "the median MAD across the included cohorts."
        ),
        width = caption_width
    )

    facet_spec <- if (is_ec) {
        facet_grid(
            cols = vars(outcome_label)
        )
    } else {
        facet_wrap(
            ~outcome_label,
            ncol = ncol
        )
    }

    # Make forest plot ---------------------------------------------------------

    p <- ggplot(
        df,
        aes(
            x = irr,
            y = plot_row,
            colour = cohort_label,
            alpha = model,
            group = interaction(
                cohort_label,
                model
            )
        )
    ) +
        geom_tile(
            data = section_band_df,
            aes(
                x = mean(x_limits),
                y = plot_row
            ),
            inherit.aes = FALSE,
            width = diff(x_limits),
            height = 1,
            fill = "#EAF1F7",
            colour = NA
        ) +
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
            size = 2
        ) +
        scale_alpha_manual(
            name = "Model",
            values = c(
                "Mutually adjusted" = 1
            )
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
        scale_x_log10(
            breaks = x_breaks,
            labels = scales::number_format(
                accuracy = 0.01
            )
        ) +
        coord_cartesian(
            xlim = x_limits
        ) +
        facet_spec +
        labs(
            title = title_text,
            x = "Incidence rate ratio (IRR)",
            y = NULL,
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
            legend.box = "vertical",
            axis.text.y = ggtext::element_markdown(
                size = 9
            ),
            plot.margin = margin(
                t = 14,
                r = 10,
                b = 14,
                l = 10
            ),
            legend.key.width = unit(12, "pt"),
            legend.key.height = unit(10, "pt"),
            legend.spacing.x = unit(4, "pt"),
            legend.spacing.y = unit(2, "pt"),
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 9)
        ) +
        guides(
            colour = guide_legend(
                title = "Cohort",
                nrow = 1,
                byrow = TRUE,
                override.aes = list(
                    size = 2,
                    alpha = 1
                )
            ),
            alpha = guide_legend(
                title = "Model",
                override.aes = list(
                    colour = "black",
                    alpha = 1
                )
            )
        )

    # Save plot ----------------------------------------------------------------

    ggsave(
        filename = file.path(
            plot_dir,
            paste0(
                "forest-",
                sub_group,
                "-",
                regression,
                "-",
                paste(outcome_names, collapse = "_"),
                "-mutually-adjusted-",
                cohort_suffix,
                ".png"
            )
        ),
        plot = p,
        width = plot_width,
        height = plot_height,
        dpi = 300,
        limitsize = FALSE
    )
}

# Create mutually adjusted figures --------------------------------------------

# Six APC outcomes: three columns by two rows
plot_irr_sup(
    regression = "negbin",
    sub_group = "main",
    outcome_names = c(
        "apc",
        "apc_unpl",
        "apc_plan",
        "apc_acsc_any",
        "apc_unpl_acsc_any",
        "apc_plan_acsc_any"
    ),
    cohorts = c(
        "precovid",
        "postcovid3"
    )
)

# EC outcomes: first and most recent post-COVID-19 cohorts
plot_irr_sup(
    regression = "negbin",
    sub_group = "main",
    outcome_names = c(
        "ec",
        "ec_acsc_any"
    ),
    cohorts = c(
        "postcovid3"
    )
)

# Six APC outcomes: three columns by two rows
plot_irr_sup(
    regression = "negbin",
    sub_group = "main",
    outcome_names = c(
        "apc",
        "apc_unpl",
        "apc_plan",
        "apc_acsc_any",
        "apc_unpl_acsc_any",
        "apc_plan_acsc_any"
    ),
    cohorts = c(
        "precovid",
        "postcovid1",
        "postcovid2",
        "postcovid3"
    )
)

# EC outcomes: first and most recent post-COVID-19 cohorts
plot_irr_sup(
    regression = "negbin",
    sub_group = "main",
    outcome_names = c(
        "ec",
        "ec_acsc_any"
    ),
    cohorts = c(
        "postcovid1",
        "postcovid2",
        "postcovid3"
    )
)
