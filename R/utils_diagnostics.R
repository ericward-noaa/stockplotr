#' Prepare index or landings fits for diagnostic plots
#'
#' Reshape observed and expected index (survey or fishery CPUE) or landings
#' values into the tidy format used by the fit diagnostic plots, such as
#' [plot_index_fit()] and [plot_landings_fit()]. Each of these functions calls
#' `prepare_fit_data()` itself, so it only needs to be called directly to
#' inspect or modify the data.
#'
#' @param dat Model output. One of:
#' \describe{
#'   \item{FIMS}{A tibble from `FIMS::get_estimates()` (directly or passed
#'   through [convert_output()]), or a `FIMSFit` object. Index rows have a
#'   `label` of "index_expected" and landings rows "catch_expected" or
#'   "landings_expected", with an `observed` value. The expected value on the
#'   data scale is taken from `estimated` and the observation error from
#'   `log_sd`.}
#'   \item{Standard output}{A tibble from [convert_output()] (e.g., Stock
#'   Synthesis or BAM) with labels "index_observed" and "index_predicted" (or
#'   "index_expected"), or "landings_observed" and "landings_predicted" (with
#'   or without a "_weight" suffix). The observation error is taken from the
#'   `uncertainty` column when `uncertainty_label` is "se", "sd", "log_sd",
#'   or "cv".}
#'   \item{r4ss output}{A list returned by `r4ss::SS_output()`. Indices come
#'   from its `cpue` element and landings from its `catch` element.}
#'   \item{Tidy data}{A data frame with columns `year`, `observed`, and
#'   `expected`, and optionally `fleet`, `se` (log-scale standard error), and
#'   `distribution`.}
#' }
#' @param data_type String. Type of data. Options: "index" or "landings".
#' Default: "index"
#' @param fleet Character vector. Fleet(s) to keep. NULL keeps all fleets.
#' Default: NULL
#' @param start_year Number. First model year. Only used for FIMS output,
#' which reports years as indices (`year_i`, 1 = first model year). NULL
#' leaves years as indices. Default: NULL
#'
#' @returns A tibble with one row per fleet (or series) and year and columns
#' `data_type`, `fleet`, `year`, `observed`, `expected`, `se`,
#' `distribution`, `used`, `residual`, `std_residual`, and `loglike`.
#'
#' @details No observations are dropped. When a fleet has observations in
#' more than one area, season, subseason, month, or sex (e.g., a survey
#' index in two seasons of a year), each is a separate series named after the
#' fleet and the column that distinguishes it, e.g., "Survey (season 2)".
#' Rows that are exact copies of each other are combined. Only values that
#' are not observations are excluded: FIMS observations coded as missing
#' (-999), landings of zero (years or fleets without catch), rows in the
#' forecast period (`era` "fore"), and expected values without an
#' observation. Stock Synthesis reports the initial equilibrium catch in the
#' year before the model starts, and it is included. Observations without a
#' year cannot be plotted and are skipped with a warning. `dat` with
#' estimates from more than one model (a `model` column with more than one
#' value) is an error; use [plot_index()] to compare models. Any other
#' fleet and year with more than one observed or expected value is also an
#' error, because the values cannot be told apart.
#'
#' Residuals are calculated on the scale of the likelihood. For
#' lognormal errors (the default) the residual is
#' \eqn{\log(o) - \log(e)}, and for normal errors \eqn{o - e}. Standardized
#' residuals divide the residual by the standard error `se`. `loglike` is the
#' log-likelihood of each observation when the model reports it (FIMS and
#' Stock Synthesis), otherwise `NA`.
#'
#' @seealso [plot_index_fit()], [plot_landings_fit()]
#' @export
#'
#' @examples
#' fits <- prepare_fit_data(stockplotr::example_data)
#' head(fits)
#'
#' # Tidy data
#' prepare_fit_data(data.frame(
#'   fleet = "survey",
#'   year = 2001:2005,
#'   observed = c(1.2, 0.9, 1.4, 1.1, 0.8),
#'   expected = c(1.0, 1.0, 1.2, 1.1, 1.0),
#'   se = 0.2
#' ))
prepare_fit_data <- function(
  dat,
  data_type = c("index", "landings"),
  fleet = NULL,
  start_year = NULL
) {
  data_type <- match.arg(data_type)
  if (inherits(dat, "stockplotr_fit")) {
    out <- dat[dat$data_type == data_type, , drop = FALSE]
  } else {
    dat <- fims_estimates_from_fit(dat)
    out <- switch(detect_fit_format(dat),
      "r4ss" = fit_from_r4ss(dat, data_type),
      "fims" = fit_from_fims(dat, data_type, start_year),
      "long" = fit_from_long(dat, data_type),
      "tidy" = fit_from_tidy(dat, data_type),
      cli::cli_abort(c(
        "Could not find {data_type} data in {.arg dat}.",
        "i" = "Supply output from {.fn convert_output}, {.code FIMS::get_estimates()}, {.code r4ss::SS_output()}, or a data frame with columns {.field year}, {.field observed}, and {.field expected}."
      ))
    )
  }
  if (!is.null(fleet)) {
    out <- out[out$fleet %in% as.character(fleet), , drop = FALSE]
  }
  if (is.null(out) || nrow(out) == 0) {
    cli::cli_abort("No {data_type} data found in {.arg dat}{if (!is.null(fleet)) ' for the selected fleet(s)'}.")
  }
  if (!inherits(out, "stockplotr_fit")) {
    out <- finish_fit(out, data_type)
  }
  out
}

#------------------------------------------------------------------------------
# Format readers
#------------------------------------------------------------------------------

# Columns returned by prepare_fit_data()
fit_cols <- c(
  "data_type", "fleet", "year", "observed", "expected", "se",
  "distribution", "used", "residual", "std_residual", "loglike"
)

# Call FIMS::get_estimates() on a FIMSFit object without a hard dependency
fims_estimates_from_fit <- function(dat) {
  if (!inherits(dat, "FIMSFit")) {
    return(dat)
  }
  get_estimates <- tryCatch(
    getExportedValue("FIMS", "get_estimates"),
    error = function(e) {
      cli::cli_abort("The {.pkg FIMS} package is needed to read a {.cls FIMSFit} object.")
    }
  )
  get_estimates(dat)
}

detect_fit_format <- function(dat) {
  if (!is.data.frame(dat) && is.list(dat) && any(c("cpue", "catch") %in% names(dat))) {
    return("r4ss")
  }
  if (!is.data.frame(dat)) {
    return("unknown")
  }
  nm <- tolower(names(dat))
  if (all(c("label", "observed", "estimated") %in% nm)) {
    return("fims")
  }
  if (all(c("label", "estimate") %in% nm)) {
    return("long")
  }
  if (all(c("year", "observed") %in% nm) && any(c("expected", "predicted") %in% nm)) {
    return("tidy")
  }
  "unknown"
}

fit_from_r4ss <- function(dat, data_type) {
  if (data_type == "index") {
    cpue <- dat[["cpue"]]
    if (is.null(cpue) || nrow(cpue) == 0) {
      return(NULL)
    }
    tibble::tibble(
      fleet = as.character(fit_pull_col(cpue, c("Fleet_name", "Name", "Fleet"))),
      year = fit_pull_col(cpue, "Yr"),
      observed = fit_pull_col(cpue, "Obs"),
      expected = fit_pull_col(cpue, "Exp"),
      se = fit_pull_col(cpue, "SE"),
      distribution = "log_normal",
      used = is.na(fit_pull_col(cpue, "Use")) | fit_pull_col(cpue, "Use") > 0,
      # Stock Synthesis reports the negative log-likelihood
      loglike = -fit_pull_col(cpue, "Like"),
      area = fit_pull_col(cpue, "Area"),
      season = fit_pull_col(cpue, "Seas"),
      subseason = fit_pull_col(cpue, "Subseas"),
      month = fit_pull_col(cpue, "Month")
    )
  } else {
    catch <- dat[["catch"]]
    if (is.null(catch) || nrow(catch) == 0) {
      return(NULL)
    }
    out <- tibble::tibble(
      fleet = as.character(fit_pull_col(catch, c("Fleet_Name", "Fleet_name", "Fleet"))),
      year = fit_pull_col(catch, "Yr"),
      area = fit_pull_col(catch, "Area"),
      season = fit_pull_col(catch, "Seas"),
      observed = fit_pull_col(catch, "Obs"),
      expected = fit_pull_col(catch, "Exp"),
      se = fit_pull_col(catch, "se"),
      distribution = "log_normal",
      used = TRUE,
      loglike = -fit_pull_col(catch, "Like")
    )
    # Years and fleets without catch are reported with zero observed catch.
    # The initial equilibrium catch (in the year before the model starts) is
    # an observation and is kept.
    out[!is.na(out$observed) & out$observed > 0, , drop = FALSE]
  }
}

fit_from_fims <- function(dat, data_type, start_year = NULL) {
  names(dat) <- tolower(names(dat))
  pattern <- if (data_type == "index") {
    "^index_?expected$"
  } else {
    "^(catch|landings)_?expected$"
  }
  rows <- dat[grepl(pattern, dat$label, ignore.case = TRUE) &
    !is.na(dat$observed) & dat$observed != -999, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  year <- if ("year" %in% names(rows) && any(!is.na(rows$year))) {
    rows$year
  } else {
    rows$year_i + if (is.null(start_year)) 0 else start_year - 1
  }
  dist <- fit_pull_col(rows, "distribution", "log_normal")
  dist[is.na(dist)] <- "log_normal"
  # The expected value on the data scale is in `estimated`; FIMS stores the
  # log of the expected value in `expected` for lognormal data
  expected <- rows$estimated
  missing_exp <- is.na(expected) & !is.na(rows$expected)
  expected[missing_exp] <- ifelse(
    grepl("log", dist[missing_exp]),
    exp(rows$expected[missing_exp]),
    rows$expected[missing_exp]
  )
  log_sd <- fit_pull_col(rows, "log_sd")
  tibble::tibble(
    fleet = as.character(fit_pull_col(rows, c("fleet", "module_id"), "All")),
    year = year,
    observed = rows$observed,
    expected = expected,
    se = exp(log_sd),
    distribution = dist,
    used = TRUE,
    loglike = fit_pull_col(rows, "likelihood")
  )
}

fit_from_long <- function(dat, data_type) {
  names(dat) <- tolower(names(dat))
  module <- tolower(as.character(fit_pull_col(dat, "module_name", "")))
  module[is.na(module)] <- ""
  if (data_type == "index") {
    obs_pattern <- "^index_observed$"
    exp_pattern <- "^index_(predicted|expected)$"
    # convert_output() labels Stock Synthesis catch fits from an r4ss list
    # with index labels in the "catch" module
    keep <- module != "catch"
  } else {
    obs_pattern <- "^landings_observed(_weight)?$"
    exp_pattern <- "^landings_(predicted|expected)(_weight)?$"
    keep <- module != "cpue"
    if (!any(grepl(obs_pattern, dat$label[keep])) && any(module == "catch")) {
      keep <- module == "catch"
      obs_pattern <- "^index_observed$"
      exp_pattern <- "^index_(predicted|expected)$"
    }
  }
  # Forecasts are not observations
  if ("era" %in% names(dat)) {
    keep <- keep & (is.na(dat$era) | !dat$era %in% c("fore", "forecast"))
  }
  dat <- dat[keep, , drop = FALSE]
  # Rows with ages or lengths are compositions or values at age
  if ("age" %in% names(dat)) dat <- dat[is.na(dat$age), , drop = FALSE]
  if ("length_bins" %in% names(dat)) dat <- dat[is.na(dat$length_bins), , drop = FALSE]
  dat$kind_ <- dplyr::case_when(
    grepl(obs_pattern, dat$label) ~ "observed",
    grepl(exp_pattern, dat$label) ~ "expected",
    TRUE ~ NA_character_
  )
  dat <- dat[!is.na(dat$kind_), , drop = FALSE]
  if (nrow(dat) == 0 || !"observed" %in% dat$kind_) {
    return(NULL)
  }
  unc_label <- tolower(as.character(fit_pull_col(dat, "uncertainty_label", NA_character_)))
  unc <- suppressWarnings(as.numeric(fit_pull_col(dat, "uncertainty")))
  se <- dplyr::case_when(
    unc_label %in% c("se", "sd", "log_sd", "logsd", "stddev") ~ unc,
    unc_label == "cv" ~ sqrt(log(1 + unc^2)),
    TRUE ~ NA_real_
  )
  long <- tibble::tibble(
    fleet = as.character(fit_pull_col(dat, "fleet", "All")),
    year = suppressWarnings(as.numeric(fit_pull_col(dat, "year"))),
    kind = dat$kind_,
    value = suppressWarnings(as.numeric(dat$estimate)),
    se = se,
    loglike = -suppressWarnings(as.numeric(fit_pull_col(dat, "likelihood")))
  )
  long$fleet[is.na(long$fleet)] <- "All"
  ids <- intersect(fit_id_cols, names(dat))
  for (col in ids) long[[col]] <- as.character(dat[[col]])
  # One value of each kind per fleet, year, and series; rows that repeat the
  # same value (e.g., the same index reported twice) are combined
  long <- dplyr::distinct(long)
  keys <- c("fleet", "year", "kind", ids)
  fit_abort_duplicates(long, keys, "value")
  out <- tidyr::pivot_wider(
    long,
    id_cols = dplyr::all_of(c("fleet", "year", ids)),
    names_from = "kind",
    values_from = c("value", "se", "loglike"),
    values_fn = dplyr::first
  )
  res <- tibble::tibble(
    fleet = out$fleet,
    year = out$year,
    observed = out$value_observed,
    expected = if ("value_expected" %in% names(out)) out$value_expected else NA_real_,
    se = out$se_observed,
    distribution = "log_normal",
    used = TRUE,
    loglike = out$loglike_observed
  )
  for (col in ids) res[[col]] <- out[[col]]
  # Years and fleets without catch are reported with zero observed catch
  if (data_type == "landings") {
    res <- res[is.na(res$observed) | res$observed > 0, , drop = FALSE]
  }
  res
}

fit_from_tidy <- function(dat, data_type) {
  if (!is.null(fit_find_col(dat, "data_type"))) {
    dat <- dat[dat[[fit_find_col(dat, "data_type")]] %in% data_type, , drop = FALSE]
  }
  dist <- fit_pull_col(dat, "distribution", "log_normal")
  dist[is.na(dist)] <- "log_normal"
  tibble::tibble(
    fleet = as.character(fit_pull_col(dat, c("fleet", "fleet_name"), "All")),
    year = fit_pull_col(dat, "year"),
    observed = fit_pull_col(dat, "observed"),
    expected = fit_pull_col(dat, c("expected", "predicted")),
    se = fit_pull_col(dat, c("se", "log_sd", "sd")),
    distribution = dist,
    used = as.logical(fit_pull_col(dat, "used", TRUE)),
    loglike = fit_pull_col(dat, c("loglike", "likelihood")),
    model = fit_pull_col(dat, "model"),
    area = fit_pull_col(dat, "area"),
    season = fit_pull_col(dat, "season"),
    subseason = fit_pull_col(dat, "subseason"),
    month = fit_pull_col(dat, "month"),
    sex = fit_pull_col(dat, "sex")
  )
}

finish_fit <- function(out, data_type) {
  out <- tibble::as_tibble(out)
  out$data_type <- data_type
  out$fleet[is.na(out$fleet)] <- "All"
  for (col in c("year", "observed", "expected", "se", "loglike")) {
    out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
  }
  out$used[is.na(out$used)] <- TRUE
  # Expected values without an observation are not plotted
  out <- out[!is.na(out$observed), , drop = FALSE]
  if (anyNA(out$year)) {
    cli::cli_warn("Skipped {sum(is.na(out$year))} {data_type} observation{?s} without a year.")
    out <- out[!is.na(out$year), , drop = FALSE]
  }
  out <- fit_split_series(out)
  lognormal <- grepl("log", out$distribution, ignore.case = TRUE)
  out$residual <- dplyr::if_else(
    lognormal,
    suppressWarnings(log(out$observed) - log(out$expected)),
    out$observed - out$expected
  )
  out$residual[!is.finite(out$residual)] <- NA_real_
  out$std_residual <- out$residual / out$se
  out$std_residual[!is.finite(out$std_residual)] <- NA_real_
  out <- out[, fit_cols] |>
    dplyr::arrange(.data$fleet, .data$year)
  class(out) <- unique(c("stockplotr_fit", class(out)))
  out
}

# Columns that distinguish observations of a fleet in the same year
fit_id_cols <- c("model", "area", "season", "subseason", "month", "sex")

# Abort when a key has more than one distinct value in `value_col`
fit_abort_duplicates <- function(dat, keys, value_col) {
  dups <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::filter(dplyr::n_distinct(.data[[value_col]]) > 1) |>
    dplyr::ungroup() |>
    dplyr::distinct(.data$fleet, .data$year)
  if (nrow(dups) > 0) {
    examples <- utils::head(paste0(dups$fleet, " ", dups$year), 3)
    cli::cli_abort(c(
      "{.arg dat} has more than one value for {nrow(dups)} fleet{?s} and year{?s} that cannot be told apart (e.g., {examples}).",
      "i" = "Filter {.arg dat} so there is one observation per fleet and year, or add a column (e.g., {.field season} or {.field area}) that distinguishes them."
    ))
  }
  invisible(NULL)
}

# Keep every observation: one model only, and fleets with observations in
# more than one area, season, subseason, month, or sex are split into one
# series for each. Exact copies are combined; other duplicates are an error.
fit_split_series <- function(out) {
  ids <- intersect(fit_id_cols, names(out))
  if ("model" %in% ids) {
    models <- unique(stats::na.omit(as.character(out$model)))
    if (length(models) > 1) {
      cli::cli_abort(c(
        "{.arg dat} has estimates from {length(models)} models ({.val {models}}).",
        "i" = "Diagnostic plots show one model at a time; filter {.arg dat} to one model, or use {.fn plot_index} to compare models."
      ))
    }
  }
  extra <- rep("", nrow(out))
  for (col in setdiff(ids, "model")) {
    vals <- as.character(out[[col]])
    split <- tapply(vals, out$fleet, function(v) length(unique(stats::na.omit(v))) > 1)
    use <- out$fleet %in% names(split)[split %in% TRUE] & !is.na(vals)
    extra[use] <- paste0(extra[use], ifelse(extra[use] == "", "", ", "), col, " ", vals[use])
  }
  out$fleet <- ifelse(extra == "", out$fleet, paste0(out$fleet, " (", extra, ")"))
  out <- out[, setdiff(names(out), ids), drop = FALSE]
  out <- dplyr::distinct(out)
  dup <- duplicated(out[, c("fleet", "year")]) | duplicated(out[, c("fleet", "year")], fromLast = TRUE)
  if (any(dup)) {
    fit_abort_duplicates(
      dplyr::mutate(out[dup, ], value = paste(.data$observed, .data$expected)),
      c("fleet", "year"),
      "value"
    )
  }
  out
}

# Labels for data types
fit_type_label <- function(x) {
  dplyr::recode(
    x,
    index = "Index",
    landings = "Landings"
  )
}

# x-axis with whole-year breaks
year_axis <- function() {
  ggplot2::scale_x_continuous(
    breaks = function(lims) {
      b <- pretty(lims)
      b[b == round(b)]
    },
    labels = function(x) format(x, scientific = FALSE, trim = TRUE)
  )
}

#------------------------------------------------------------------------------
# Column and facet helpers
#------------------------------------------------------------------------------

# Find a column in dat ignoring case; returns NULL if absent
fit_find_col <- function(dat, options) {
  nm <- names(dat)
  for (o in options) {
    hit <- nm[tolower(nm) == tolower(o)]
    if (length(hit) > 0) {
      return(hit[1])
    }
  }
  NULL
}

# Pull a column (by any of several names) or return a default vector
fit_pull_col <- function(dat, options, default = NA) {
  col <- fit_find_col(dat, options)
  if (is.null(col)) {
    return(rep(default, nrow(dat)))
  }
  dat[[col]]
}

# Facet a diagnostic plot by the variables that vary in the data
fit_facet <- function(vars, ncol = NULL, scales = "fixed", direction = "v") {
  if (length(vars) == 0) {
    return(NULL)
  }
  list(
    ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(vars)),
      ncol = ncol,
      dir = match.arg(direction, c("v", "h")),
      scales = scales,
      labeller = ggplot2::labeller(.multi_line = FALSE)
    ),
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
  )
}
