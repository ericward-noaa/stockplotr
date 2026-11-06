#' Plot the fit to indices of abundance
#'
#' Plot observed index values with confidence intervals and the expected
#' (model-predicted) index for each fleet. This is the index fit plot from
#' `r4ss::SSplotIndices()` (subplots 2 and 5), and it works for any model
#' output that [prepare_fit_data()] can read, including FIMS.
#'
#' @param dat Model output read by [prepare_fit_data()]: a tibble from
#' [convert_output()] or `FIMS::get_estimates()`, a `FIMSFit` object, an
#' `r4ss::SS_output()` list, or a tidy data frame.
#' @param data_type String. Type of data. Options: "index" or "landings".
#' Default: "index"
#' @param fleet Character vector. Fleet(s) to plot. NULL plots all fleets.
#' Default: NULL
#' @param log Logical. TRUE/FALSE; plot the y-axis on a log scale. Default: FALSE
#' @param interval Number. Coverage of the confidence intervals around the
#' observations, based on the observation error (`se`). Default: 0.95
#' @param start_year Number. First model year, used to convert FIMS year
#' indices to years. Default: NULL
#' @param ncol Number. Number of columns of panels. Default: NULL
#' @param direction String. Direction to fill panels, "v" (vertical) or "h"
#' (horizontal). Default: "v"
#' @param make_rda Logical. TRUE/FALSE; indicate whether to save the object and
#' make an automated caption and alternative text in the form of an `rda`
#' object. If TRUE, the rda will be exported to the folder indicated in the
#' argument "figures_dir". The caption and alternative text describe the
#' data and options used to make the plot. Default: `FALSE`
#' @param figures_dir Path. Path to the "figures" folder, which is created if
#' it does not exist. Default: `getwd()`
#'
#' @returns A `ggplot2` object.
#'
#' @details For lognormal errors, the interval is
#' \eqn{o \exp(\pm z \cdot se)}; for normal errors, \eqn{o \pm z \cdot se}.
#' Observations that were not used in the likelihood (e.g., `Use = -1` in
#' Stock Synthesis) are shown as open points.
#'
#' @seealso [plot_index()], [plot_landings_fit()], [prepare_fit_data()]
#' @export
#'
#' @examples
#' plot_index_fit(stockplotr::example_data)
#' plot_index_fit(stockplotr::example_data, log = TRUE)
plot_index_fit <- function(
  dat,
  data_type = c("index", "landings"),
  fleet = NULL,
  log = FALSE,
  interval = 0.95,
  start_year = NULL,
  ncol = NULL,
  direction = "v",
  make_rda = FALSE,
  figures_dir = getwd()
) {
  data_type <- match.arg(data_type)
  fits <- prepare_fit_data(dat, data_type = data_type, fleet = fleet, start_year = start_year)
  z <- stats::qnorm(1 - (1 - interval) / 2)
  lognormal <- grepl("log", fits$distribution, ignore.case = TRUE)
  fits <- fits |>
    dplyr::mutate(
      lower = dplyr::if_else(lognormal, .data$observed * exp(-z * .data$se), .data$observed - z * .data$se),
      upper = dplyr::if_else(lognormal, .data$observed * exp(z * .data$se), .data$observed + z * .data$se),
      used = factor(dplyr::if_else(.data$used, "Used", "Not used"), levels = c("Used", "Not used"))
    )
  if (log) {
    fits$lower[fits$lower <= 0] <- NA
  }
  ylab <- fit_type_label(data_type)

  plt <- ggplot2::ggplot(fits, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      color = "gray50",
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$observed, shape = .data$used),
      size = 2,
      fill = "white"
    ) +
    ggplot2::geom_line(
      data = fits[!is.na(fits$expected), ],
      ggplot2::aes(y = .data$expected),
      color = "#0072B2",
      linewidth = 0.8
    ) +
    ggplot2::scale_shape_manual(values = c("Used" = 16, "Not used" = 21), drop = TRUE) +
    ggplot2::labs(
      x = "Year",
      y = if (log) paste(ylab, "(log scale)") else ylab,
      shape = NULL,
      caption = glue::glue("Points: observed with {round(interval * 100)}% intervals; line: expected")
    ) +
    theme_noaa() +
    year_axis() +
    fit_facet("fleet", ncol = ncol, scales = "free_y", direction = direction)
  if (all(fits$used == "Used")) {
    plt <- plt + ggplot2::guides(shape = "none")
  }
  if (log) {
    plt <- plt + ggplot2::scale_y_log10()
  } else {
    plt <- plt + ggplot2::expand_limits(y = 0)
  }
  if (make_rda) {
    fit_export_rda(
      plt,
      topic_label = paste0(data_type, "_fit"),
      tags = fit_tags_fit(fits, data_type, log = log, interval = interval),
      dat = dat,
      figures_dir = figures_dir
    )
  }
  plt
}

#' Plot the fit to landings
#'
#' Plot observed landings with confidence intervals and expected landings for
#' each fleet, as in `r4ss::SSplotCatch()`. Landings are usually fit with
#' small observation errors, so large deviations between observed and
#' expected landings can indicate a problem with the model.
#'
#' @inheritParams plot_index_fit
#' @param ... Other arguments passed to [plot_index_fit()].
#'
#' @returns A `ggplot2` object.
#'
#' @seealso [plot_index_fit()], [plot_landings()], [prepare_fit_data()]
#' @export
#'
#' @examples
#' plot_landings_fit(stockplotr::example_data)
plot_landings_fit <- function(dat, fleet = NULL, ...) {
  plot_index_fit(dat, data_type = "landings", fleet = fleet, ...)
}

#------------------------------------------------------------------------------
# Caption and alternative text tags
#------------------------------------------------------------------------------

# plot_index_fit() and plot_landings_fit(); `fits` include the intervals
fit_tags_fit <- function(fits, data_type, log, interval) {
  p <- paste0(data_type, ".fit.")
  with_exp <- fits[!is.na(fits$expected), , drop = FALSE]
  covered <- sum(with_exp$expected >= with_exp$lower & with_exp$expected <= with_exp$upper, na.rm = TRUE)
  tags <- list(
    fleets = fit_list_text(fits$fleet),
    years = fit_year_range(fits$year),
    start.year = min(fits$year),
    end.year = max(fits$year),
    scale.desc = if (log) "on a log scale" else "on a linear scale",
    interval = round(interval * 100),
    used.desc = if (all(fits$used == "Used")) {
      "All observations were used in the model fit."
    } else {
      "Open points are observations that were not used in the model fit."
    },
    panel.desc = fit_panel_text(fits$fleet),
    summary = paste0(
      "Observed values range from ", fit_num(min(fits$observed)),
      " to ", fit_num(max(fits$observed)), ". ",
      "The expected value is within the ", round(interval * 100),
      "% interval of ", covered, " of ", nrow(with_exp), " observations."
    )
  )
  stats::setNames(tags, paste0(p, names(tags)))
}
