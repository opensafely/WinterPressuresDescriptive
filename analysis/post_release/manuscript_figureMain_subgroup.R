# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(tidyverse)
library(grid)
library(ggtext)
library(patchwork)


# Specify paths ----------------------------------------------------------------
print("Specify paths")

source("analysis/specify_paths.R")

plot_dir <- "output/post_release/forest_plots"

dir.create(
    plot_dir,
    recursive = TRUE,
    showWarnings = FALSE
)


# Read labels ------------------------------------------------------------------
print("Read labels")

labels <- readr::read_csv(
    "lib/labels.csv",
    show_col_types = FALSE
)


# Specify characteristic order -------------------------------------------------

group_order <- c(
    # "Practice region*",
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
    # "Practice region*",
    "Practice rurality",
    "List size",
    "Monthly consultation"
)

case_mix_groups <- setdiff(group_order, practice_groups)

groups_with_subtitle <- c(
    # "Practice region*",
    "Practice rurality",
    "Age",
    "Sex",
    "Ethnicity",
    "Deprivation",
    "Smoking Status"
)


# Specify patient groups -------------------------------------------------------

subgroups_to_compare <- c(
    "sub_asth",
    "sub_copd",
    "sub_diab",
    "sub_htn",
    "sub_sevmh"
)

subgroup_colours <- c(
    sub_asth = "#2864A0", # Deep blue
    sub_copd = "#C65D28", # Burnt orange
    sub_diab = "#238577", # Teal
    sub_htn = "#95639E", # Plum
    sub_sevmh = "#C99A18" # Golden yellow
)

subgroup_shapes <- c(
    sub_asth = 16, # Circle
    sub_copd = 17, # Triangle
    sub_diab = 15, # Square
    sub_htn = 18, # Diamond
    sub_sevmh = 8 # Star
)

subgroup_names <- c(
    sub_asth = "Asthma",
    sub_copd = "COPD",
    sub_diab = "Diabetes",
    sub_htn = "Hypertension",
    sub_sevmh = "Severe mental illness"
)


# Read model output ------------------------------------------------------------
print("Read model output")

model_output <- readr::read_csv(
    "output/post_release/plot_model_output.csv",
    col_types = readr::cols(
        error = readr::col_character(),
        irr = readr::col_double(),
        lci = readr::col_double(),
        uci = readr::col_double()
    ),
    show_col_types = FALSE
)


# Create plotting function -----------------------------------------------------

plot_irr <- function(
  regression,
  sub_group,
  outcome_names,
  cohorts,
  practice_char = "all",
  x_limits = NULL
) {
    practice_char <- match.arg(
        practice_char,
        c("all", "practice", "case_mix")
    )

    if (length(cohorts) != 1L || anyNA(cohorts)) {
        stop("Select exactly one cohort for subgroup comparisons.")
    }

    if (!length(sub_group) ||
        anyNA(sub_group) ||
        anyDuplicated(sub_group)) {
        stop("Select patient groups without missing or duplicate names.")
    }

    if (!all(sub_group %in% names(subgroup_names)) ||
        !all(sub_group %in% names(subgroup_colours)) ||
        !all(sub_group %in% names(subgroup_shapes))) {
        stop("Add labels, colours and shapes for all selected subgroups.")
    }

    if (cohorts == "precovid" &&
        any(str_detect(outcome_names, "^ec"))) {
        stop("Pre-COVID emergency care outcomes are not plotted.")
    }

    is_ec <- all(str_detect(outcome_names, "^ec"))


    # Filter model estimates ---------------------------------------------------

    df <- model_output %>%
        mutate(
            outcome = str_remove(
                outcome,
                paste0("_", analysis, "$")
            )
        ) %>%
        filter(
            cohort %in% cohorts,
            analysis %in% sub_group,
            outcome %in% outcome_names,
            model_type == regression,
            str_detect(term, "^exp_prop(_|$)"),
            model == "mdl_age_sex"
        ) %>%
        mutate(
            exposure = if_else(
                term == "exp_prop",
                exposure,
                term
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
            any_of("error")
        )

    if (!nrow(df)) {
        stop("No matching model estimates.")
    }


    # Join characteristic labels -----------------------------------------------

    exposure_labels <- labels %>%
        filter(group %in% group_order) %>%
        transmute(
            exposure = term,
            exposure_label = label,
            group,
            ref
        ) %>%
        distinct(exposure, .keep_all = TRUE)

    df <- df %>%
        left_join(exposure_labels, by = "exposure") %>%
        filter(!is.na(exposure_label))

    if (practice_char == "practice") {
        df <- df %>%
            filter(group %in% practice_groups)
    } else if (practice_char == "case_mix") {
        df <- df %>%
            filter(group %in% case_mix_groups)
    }

    if (!nrow(df)) {
        stop("No estimates remain after selecting labelled characteristics.")
    }


    # Check only estimates retained for plotting -------------------------------

    missing_groups <- setdiff(sub_group, unique(df$analysis))

    if (length(missing_groups)) {
        stop(
            "No matching estimates after filtering for: ",
            paste(missing_groups, collapse = ", ")
        )
    }

    duplicate_estimates <- df %>%
        count(analysis, exposure, outcome, model) %>%
        filter(n > 1L)

    if (nrow(duplicate_estimates)) {
        print(duplicate_estimates, n = Inf)

        stop(
            "Duplicate estimates for the same subgroup, ",
            "exposure, outcome and model."
        )
    }

    # Reference categories in this output have IRR = 1 and no CI.
    df <- df %>%
        mutate(
            is_reference = !is.na(irr) &
                irr == 1 &
                is.na(lci) &
                is.na(uci)
        )

    invalid_estimates <- df %>%
        filter(
            !is_reference &
                (
                    !is.finite(irr) |
                        !is.finite(lci) |
                        !is.finite(uci) |
                        lci <= 0 |
                        lci > irr |
                        irr > uci
                )
        )

    if (nrow(invalid_estimates)) {
        print(invalid_estimates, n = Inf, width = Inf)

        stop(
            nrow(invalid_estimates),
            " invalid non-reference estimate(s) remain after filtering."
        )
    }


    # Join outcome labels ------------------------------------------------------

    outcome_lookup <- labels %>%
        transmute(
            outcome = term,
            outcome_label = label
        ) %>%
        distinct(outcome, .keep_all = TRUE)

    outcome_names_labelled <- tibble(
        outcome = outcome_names
    ) %>%
        left_join(outcome_lookup, by = "outcome") %>%
        mutate(
            outcome_label = coalesce(outcome_label, outcome)
        )

    outcome_labeller <- outcome_names_labelled %>%
        deframe()


    # Cohort label -------------------------------------------------------------

    cohort_label <- labels %>%
        filter(term == cohorts) %>%
        pull(label)

    if (!length(cohort_label) || is.na(cohort_label[1])) {
        cohort_label <- cohorts
    } else {
        cohort_label <- cohort_label[1]
    }


    # Construct sections and characteristic rows --------------------------------

    practice_section <- "Practice characteristics"

    case_mix_section <- paste0(
        "Patient case-mix ",
        "(% of patients in the practice)"
    )

    section_order <- switch(practice_char,
        all = c(practice_section, case_mix_section),
        practice = practice_section,
        case_mix = case_mix_section
    )

    df <- df %>%
        mutate(
            analysis = factor(
                analysis,
                levels = sub_group
            ),
            outcome = factor(
                outcome,
                levels = outcome_names
            ),
            group = factor(
                group,
                levels = group_order
            ),
            ref_order = if_else(
                is.na(ref),
                Inf,
                as.numeric(ref)
            ),
            section = if_else(
                group %in% practice_groups,
                practice_section,
                case_mix_section
            ),
            group_character = as.character(group),
            plot_row = paste(
                "item",
                group_character,
                exposure,
                sep = "|||"
            )
        )

    item_rows <- df %>%
        distinct(
            section,
            group_character,
            exposure,
            exposure_label,
            plot_row,
            ref_order
        ) %>%
        arrange(
            match(section, section_order),
            match(group_character, group_order),
            ref_order,
            exposure_label
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

            purrr::map_dfr(
                groups_in_section,
                function(group_name) {
                    rows_in_group <- item_rows %>%
                        filter(
                            section == section_name,
                            group_character == group_name
                        )

                    has_subtitle <- group_name %in%
                        groups_with_subtitle

                    characteristic_rows <- rows_in_group %>%
                        transmute(
                            plot_row,
                            y_label = if (has_subtitle) {
                                paste0(
                                    "&nbsp;&nbsp;&nbsp;",
                                    exposure_label
                                )
                            } else {
                                paste0(
                                    "<b>",
                                    exposure_label,
                                    "</b>"
                                )
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
                                    "<b>",
                                    group_display,
                                    "</b>"
                                ),
                                row_type = "heading"
                            ),
                            characteristic_rows
                        )
                    } else {
                        characteristic_rows
                    }
                }
            ) %>%
                mutate(section = section_name)
        }
    )

    if (!all(section_order %in% row_structure$section)) {
        stop("No labelled characteristics for one or more selected sections.")
    }


    # Set x-axis range ---------------------------------------------------------

    # A shared range across all subgroups and outcomes in this figure.
    # Reference categories have missing CIs, so use na.rm = TRUE.
    if (is.null(x_limits)) {
        x_limits <- range(
            c(df$lci, df$uci, 1),
            na.rm = TRUE
        )

        x_limits <- c(
            x_limits[1] / 1.05,
            x_limits[2] * 1.05
        )
    }

    if (length(x_limits) != 2L ||
        any(!is.finite(x_limits)) ||
        x_limits[1] <= 0 ||
        x_limits[1] >= x_limits[2]) {
        stop("x_limits must contain two increasing positive values.")
    }

    if (any(
        df$lci < x_limits[1] |
            df$uci > x_limits[2],
        na.rm = TRUE
    )) {
        warning(
            "Some confidence intervals extend beyond x_limits. ",
            "Consider widening the range."
        )
    }

    x_breaks <- round(2^seq(-1, 1, 0.25), 1)

    # Fixed vertical offsets for patient groups --------------------------------

    # Keep subgroup positions consistent, even if an estimate is absent.
    subgroup_offsets <- setNames(
        if (length(sub_group) == 1L) {
            0
        } else {
            seq(
                0.32,
                -0.32,
                length.out = length(sub_group)
            )
        },
        sub_group
    )


    # Shared facet specification -----------------------------------------------

    facet_spec <- facet_grid(
        cols = vars(outcome),
        labeller = labeller(
            outcome = as_labeller(outcome_labeller)
        ),
        drop = FALSE
    )


    # Create each section ------------------------------------------------------

    make_section_plot <- function(section_name) {
        section_rows <- row_structure %>%
            filter(section == section_name) %>%
            mutate(
                y_position = rev(seq_len(n()))
            )

        section_df <- df %>%
            filter(section == section_name) %>%
            left_join(
                section_rows %>%
                    select(plot_row, y_position),
                by = "plot_row"
            ) %>%
            mutate(
                y_plot = y_position +
                    unname(
                        subgroup_offsets[as.character(analysis)]
                    )
            )

        is_last_section <- section_name ==
            tail(section_order, 1)

        separator_lines <- section_rows %>%
            filter(row_type == "characteristic") %>%
            transmute(yintercept = y_position - 0.5)

        ggplot(
            section_df,
            aes(
                x = irr,
                y = y_plot,
                colour = analysis,
                shape = analysis,
                group = analysis
            )
        ) +
            geom_vline(
                xintercept = 1,
                colour = "grey60",
                linetype = "dashed",
                linewidth = 0.6
            ) +
            geom_hline(
                data = separator_lines,
                aes(yintercept = yintercept),
                inherit.aes = FALSE,
                colour = "grey95",
                linewidth = 0.25
            ) +
            # Confidence intervals for non-reference estimates only.
            geom_errorbar(
                data = function(d) {
                    d %>% filter(!is_reference)
                },
                aes(
                    xmin = lci,
                    xmax = uci
                ),
                orientation = "y",
                width = 0,
                linewidth = 0.5
            ) +
            # Includes reference-category points at IRR = 1.
            geom_point(size = 2.2) +
            scale_colour_manual(
                name = NULL,
                values = subgroup_colours,
                limits = sub_group,
                breaks = sub_group,
                labels = unname(subgroup_names[sub_group]),
                drop = FALSE
            ) +
            scale_shape_manual(
                name = NULL,
                values = subgroup_shapes,
                limits = sub_group,
                breaks = sub_group,
                labels = unname(subgroup_names[sub_group]),
                drop = FALSE
            ) +
            scale_x_log10(
                breaks = x_breaks,
                labels = scales::number_format(accuracy = 0.1)
            ) +
            scale_y_continuous(
                breaks = section_rows$y_position,
                labels = section_rows$y_label,
                limits = c(
                    0.5,
                    nrow(section_rows) + 0.5
                ),
                expand = expansion(mult = 0)
            ) +
            coord_cartesian(xlim = x_limits) +
            facet_spec +
            labs(
                title = section_name,
                x = if (is_last_section) {
                    "Incidence rate ratio and 95% confidence interval"
                } else {
                    NULL
                },
                y = NULL
            ) +
            theme_bw() +
            theme(
                plot.title = ggtext::element_markdown(
                    hjust = 0,
                    size = 20,
                    margin = margin(b = 10)
                ),
                panel.border = element_blank(),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.grid.major.x = element_line(
                    colour = "grey88",
                    linewidth = 0.3
                ),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.spacing = unit(40, "pt"),
                axis.ticks = element_blank(),
                axis.text.x = element_text(size = 16),
                axis.title.x = element_text(
                    size = 18,
                    margin = margin(t = 14)
                ),
                axis.text.y = ggtext::element_markdown(
                    size = 18
                ),
                plot.margin = margin(
                    t = 14,
                    r = 10,
                    b = 14,
                    l = 10
                ),
                legend.position = "bottom",
                legend.box = "vertical",
                legend.key.width = unit(30, "pt"),
                legend.key.height = unit(22, "pt"),
                legend.text = element_text(size = 17),
                legend.spacing.x = unit(8, "pt")
            ) +
            guides(
                colour = guide_legend(
                    nrow = 1,
                    byrow = TRUE,
                    override.aes = list(size = 4, alpha = 1)
                ),
                shape = guide_legend(
                    nrow = 1,
                    byrow = TRUE
                )
            )
    }


    # Outcome headings shown once at the top -----------------------------------

    outcome_header <- ggplot(
        tibble(
            outcome = factor(
                outcome_names,
                levels = outcome_names
            )
        )
    ) +
        facet_spec +
        theme_void() +
        theme(
            strip.background = element_blank(),
            strip.text.x = element_text(
                size = 20,
                face = "bold",
                hjust = 0.5,
                margin = margin(t = 5, b = 10)
            ),
            panel.spacing = unit(12, "pt"),
            plot.margin = margin(
                t = 0,
                r = 10,
                b = 0,
                l = 10
            )
        )


    # Combine sections ---------------------------------------------------------

    section_plots <- purrr::map(
        section_order,
        make_section_plot
    )

    section_heights <- vapply(
        section_order,
        function(section_name) {
            sum(row_structure$section == section_name)
        },
        integer(1)
    )

    p <- (
        patchwork::wrap_plots(
            c(list(outcome_header), section_plots),
            ncol = 1
        ) +
            patchwork::plot_layout(
                heights = c(0, section_heights),
                guides = "collect"
            )
    ) & theme(legend.position = "bottom")


    # Save figure --------------------------------------------------------------

    panel_width <- if (is_ec) 5.8 else 5.3

    plot_width <- max(
        12,
        panel_width * length(outcome_names)
    )

    plot_height <- max(
        10,
        0.45 * sum(section_heights) +
            0.7 * length(section_order) + 2
    )

    outcome_suffix <- if (is_ec) {
        "ec"
    } else if (all(str_detect(outcome_names, "acsc"))) {
        "apc_acsc"
    } else {
        "apc"
    }

    filename <- file.path(
        plot_dir,
        paste0(
            "forest-subgroups-", outcome_suffix, "-", cohorts, ".png"
        )
    )

    ggsave(
        filename = filename,
        plot = p,
        width = plot_width,
        height = plot_height,
        dpi = 300
    )

    message("Saved ", filename)
    invisible(p)
}


for (cohort_to_plot in c(
    "precovid",
    "postcovid1",
    "postcovid2",
    "postcovid3"
)) {
    # A&E: available for post-COVID cohorts only.
    if (cohort_to_plot != "precovid") {
        plot_irr(
            regression = "negbin",
            sub_group = subgroups_to_compare,
            outcome_names = c("ec", "ec_acsc_any"),
            cohorts = cohort_to_plot,
            practice_char = "all"
        )
    }

    # All-cause admissions.
    plot_irr(
        regression = "negbin",
        sub_group = subgroups_to_compare,
        outcome_names = c("apc", "apc_unpl"),
        cohorts = cohort_to_plot,
        practice_char = "all"
    )

    # ACSC-related admissions.
    plot_irr(
        regression = "negbin",
        sub_group = subgroups_to_compare,
        outcome_names = c(
            "apc_acsc_any",
            "apc_unpl_acsc_any",
            "apc_plan_acsc_any"
        ),
        cohorts = cohort_to_plot,
        practice_char = "all"
    )
}
