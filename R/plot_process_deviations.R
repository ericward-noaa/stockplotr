#' Plot process deviations
#'
#' Plot the estimated deviations of random-effect or time-varying processes
#' (e.g., recruitment, fishing mortality, catchability, selectivity) over
#' time, with one panel per process and fleet.
#'
#' @inheritParams plot_spawning_biomass
#' @param process Character vector. Processes to plot, from
#' "recruitment", "fishing_mortality", "fishing_mortality_discards",
#' "catchability", "selectivity", "natural_mortality", and "abundance".
#' NULL plots every process found. Default: NULL
#' @param interval Number. Coverage of intervals around each deviation,
#' from its standard error (\eqn{\pm z \cdot SE}). NULL draws no intervals.
#' Default: 0.95
#'
#' @returns A `ggplot2` object showing deviations over time, with a
#' horizontal line at 0 (no deviation).
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#'
#' Deviations are found by their labels, such as
#' "log_recruitment_deviations" (FIMS and BAM), "recruitment_deviations"
#' (Stock Synthesis), "log_F_deviations" and "q_RW_log_dev" (BAM), and
#' "sel_inf_dev" (AMAK). Index and discard residuals (labeled "dev" in
#' Stock Synthesis) are fits to data, not process deviations, and are not
#' included; see [plot_index()] and [plot_runs_test()]. When a model reports
#' the same process in more than one place (e.g., BAM reports recruitment
#' deviations in both `parm.tvec` and `t.series`), the source with
#' uncertainty, then with the most rows, is used.
#'
#' Series that are zero in every year (not estimated) are left out.
#' Intervals are drawn for deviations with a standard error (uncertainty
#' labeled "se", "sd", or "log_sd"). Deviations without a year (e.g., some
#' TMB `sdreport` output) can't be placed in time and are left out with a
#' message. Deviations that vary by age within a year (e.g., numbers-at-age
#' process errors) aren't plotted yet.
#'
#' Plotting process deviations doesn't need one-step-ahead (OSA) residuals,
#' but formal tests of them (normality, autocorrelation) would.
#'
#' @seealso [plot_recruitment_deviations()], [convert_output()],
#' [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_process_deviations(stockplotr::vermilion_snapper)
#' plot_process_deviations(
#'   stockplotr::vermilion_snapper,
#'   process = c("recruitment", "catchability")
#' )
plot_process_deviations <- function(
  dat,
  process = NULL,
  era = NULL,
  interval = 0.95,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  check_data(dat)
  devs <- process_deviations_data(dat, process = process, era = era)

  if (!is.null(interval)) {
    if (!is.numeric(interval) || length(interval) != 1 || interval <= 0 || interval >= 1) {
      cli::cli_abort("{.arg interval} must be a number between 0 and 1 (e.g., 0.95), or NULL.")
    }
    z <- stats::qnorm(1 - (1 - interval) / 2)
    type <- tolower(as.character(devs$uncertainty_label))
    se <- ifelse(type %in% c("se", "sd", "log_sd", "logsd", "stddev", "std"), devs$uncertainty, NA_real_)
    devs$estimate_lower <- devs$estimate - z * se
    devs$estimate_upper <- devs$estimate + z * se
  }

  multi_model <- length(unique(devs$model)) > 1
  # One colour per model when comparing models; otherwise fixed colours
  layer <- function(geom, mapping = NULL, colour, ...) {
    if (multi_model) {
      geom(mapping = mapping, ...)
    } else {
      geom(mapping = mapping, colour = colour, ...)
    }
  }

  aesthetics <- if (multi_model) {
    ggplot2::aes(x = .data$year, y = .data$estimate, group = .data$model, colour = .data$model)
  } else {
    ggplot2::aes(x = .data$year, y = .data$estimate, group = .data$model)
  }
  plt <- ggplot2::ggplot(devs, aesthetics) +
    ggplot2::geom_hline(yintercept = 0, colour = "#6e6e6e", linewidth = 0.6)
  if (!is.null(interval) && any(!is.na(devs$estimate_lower))) {
    plt <- plt +
      layer(
        ggplot2::geom_linerange,
        ggplot2::aes(ymin = .data$estimate_lower, ymax = .data$estimate_upper),
        colour = "#5798fa",
        alpha = 0.6,
        na.rm = TRUE
      )
  }
  plt <- plt +
    layer(ggplot2::geom_line, colour = "grey40", linewidth = 0.3, alpha = 0.6) +
    layer(ggplot2::geom_point, colour = "black", size = 1.2) +
    ggplot2::labs(x = "Year", y = "Deviation")
  if (multi_model) plt <- plt + ggplot2::labs(colour = "Model")
  if (length(unique(devs$panel)) > 1) {
    plt <- plt + ggplot2::facet_wrap(~panel, scales = "free_y")
  }
  plt <- plt + theme_noaa()

  if (make_rda) {
    proc.dev.start.year <- min(devs$year)
    proc.dev.end.year <- max(devs$year)
    proc.dev.processes <- paste(unique(devs$process_name), collapse = ", ")
    export_kqs(
      proc.dev.start.year,
      proc.dev.end.year,
      proc.dev.processes,
      dir = figures_dir
    )
    insert_kqs(
      proc.dev.start.year,
      proc.dev.end.year,
      proc.dev.processes,
      dir = figures_dir
    )
    create_rda(
      object = plt,
      topic_label = "process_deviations",
      fig_or_table = "figure",
      dat = if (is.data.frame(dat)) dat else dat[[1]],
      dir = figures_dir
    )
  }
  plt
}

# Process names, in plotting order, and their panel titles
process_deviation_names <- c(
  recruitment = "Recruitment",
  fishing_mortality = "F",
  fishing_mortality_discards = "F (discards)",
  catchability = "Catchability",
  selectivity = "Selectivity",
  natural_mortality = "Natural mortality",
  abundance = "Abundance"
)

# The process a deviation label belongs to, or NA if the label is not a
# process deviation
classify_process_deviation <- function(label) {
  label <- tolower(label)
  is_dev <- grepl("dev", label) &
    !label %in% c("dev", "raw_dev", "discard_deviation", "discard_dev") &
    !grepl("cohort_?grow", label)
  out <- dplyr::case_when(
    !is_dev ~ NA_character_,
    grepl("rec(ruit(ment)?)?[._]?dev|logr[._]dev", label) ~ "recruitment",
    grepl("(^|[._])(log[._])?f(mort)?[._]dev|fishing_mortality[._]dev", label) & grepl("discard", label) ~ "fishing_mortality_discards",
    grepl("(^|[._])(log[._])?f(mort)?[._]dev|fishing_mortality[._]dev", label) ~ "fishing_mortality",
    grepl("(^|[._])q[._]|catchability", label) ~ "catchability",
    grepl("sel", label) ~ "selectivity",
    grepl("(^|[._])m[._]dev|natural_mortality", label) ~ "natural_mortality",
    grepl("naa|abundance|numbers_at_age|init", label) ~ "abundance",
    TRUE ~ NA_character_
  )
  out
}

# Process deviations in long format, one source per model and process, with
# a `panel` column for faceting
process_deviations_data <- function(dat, process = NULL, era = NULL) {
  if (!is.data.frame(dat)) {
    if (is.null(names(dat))) names(dat) <- paste0("model_", seq_along(dat))
    dat <- purrr::imap(dat, function(d, nm) {
      if (!"model" %in% names(d)) d$model <- nm
      d
    }) |>
      dplyr::bind_rows()
  }
  if (!"model" %in% names(dat)) dat$model <- "1"
  for (col in c("module_name", "fleet", "age", "year", "era", "uncertainty", "uncertainty_label")) {
    if (!col %in% names(dat)) dat[[col]] <- NA
  }
  if (!is.null(process)) {
    bad <- setdiff(process, names(process_deviation_names))
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "Unknown {.arg process}: {.val {bad}}.",
        "i" = "Choose from {.val {names(process_deviation_names)}}."
      ))
    }
  }

  devs <- dat |>
    dplyr::mutate(
      process_name = classify_process_deviation(.data$label),
      year = suppressWarnings(as.numeric(.data$year)),
      estimate = suppressWarnings(as.numeric(.data$estimate)),
      uncertainty = suppressWarnings(as.numeric(.data$uncertainty)),
      fleet = as.character(.data$fleet)
    ) |>
    dplyr::filter(
      !is.na(.data$process_name),
      # Index fits are labeled "dev" in some Stock Synthesis output
      !grepl("index|cpue|survey", tolower(.data$module_name)),
      !is.na(.data$estimate)
    )
  if (!is.null(process)) {
    devs <- dplyr::filter(devs, .data$process_name %in% process)
  }
  if (!is.null(era)) {
    era_selection <- era
    devs <- dplyr::filter(devs, .data$era %in% era_selection)
  }
  if (nrow(devs) == 0) {
    cli::cli_abort("No process deviations found in {.arg dat}.")
  }

  # One source (module and label) per model and process: prefer the one
  # with uncertainty, then the one with the most rows
  sources <- devs |>
    dplyr::filter(!is.na(.data$year)) |>
    dplyr::group_by(.data$model, .data$process_name, .data$module_name, .data$label) |>
    dplyr::summarise(
      n_unc = sum(!is.na(.data$uncertainty)),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$model, .data$process_name, dplyr::desc(.data$n_unc), dplyr::desc(.data$n)) |>
    dplyr::distinct(.data$model, .data$process_name, .keep_all = TRUE)

  no_year <- devs |>
    dplyr::anti_join(sources, by = c("model", "process_name")) |>
    dplyr::distinct(.data$label) |>
    dplyr::pull()
  if (length(no_year) > 0) {
    cli::cli_alert_info("Left out deviations with no year: {.val {no_year}}.")
  }

  devs <- devs |>
    dplyr::semi_join(sources, by = c("model", "process_name", "module_name", "label")) |>
    dplyr::filter(!is.na(.data$year))

  # Series fixed at zero (e.g., catchability deviations of fleets without a
  # random walk) were not estimated
  devs <- devs |>
    dplyr::group_by(.data$model, .data$process_name, .data$fleet) |>
    dplyr::filter(any(.data$estimate != 0)) |>
    dplyr::ungroup()

  by_age <- devs |>
    dplyr::group_by(.data$model, .data$process_name, .data$fleet, .data$year) |>
    dplyr::filter(dplyr::n_distinct(.data$age, na.rm = TRUE) > 1) |>
    dplyr::ungroup() |>
    dplyr::distinct(.data$process_name) |>
    dplyr::pull()
  if (length(by_age) > 0) {
    cli::cli_alert_info("Age-specific deviations aren't plotted yet: {.val {by_age}}.")
    devs <- dplyr::filter(devs, !.data$process_name %in% by_age)
  }
  if (nrow(devs) == 0) {
    cli::cli_abort("No process deviations with years found in {.arg dat}.")
  }

  devs |>
    dplyr::mutate(
      process_name = factor(.data$process_name, levels = names(process_deviation_names)),
      panel = ifelse(
        is.na(.data$fleet) | .data$fleet == "",
        process_deviation_names[as.character(.data$process_name)],
        paste0(process_deviation_names[as.character(.data$process_name)], ": ", .data$fleet)
      )
    ) |>
    dplyr::arrange(.data$process_name, .data$fleet, .data$model, .data$year) |>
    dplyr::mutate(
      panel = factor(.data$panel, levels = unique(.data$panel)),
      process_name = as.character(.data$process_name)
    ) |>
    dplyr::select(dplyr::any_of(c(
      "model", "process_name", "panel", "label", "module_name", "fleet",
      "era", "year", "estimate", "uncertainty", "uncertainty_label"
    )))
}
