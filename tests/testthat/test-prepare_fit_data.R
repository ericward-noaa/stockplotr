test_that("prepare_fit_data reads FIMS output", {
  est <- fims_estimates()
  idx <- prepare_fit_data(est)
  expect_s3_class(idx, "stockplotr_fit")
  expect_equal(nrow(idx), 30)
  expect_equal(unique(idx$fleet), "survey1")
  rows <- est[est$label == "index_expected" & !is.na(est$observed), ]
  # Expected values on the data scale come from `estimated`, not the
  # log-scale `expected` column
  expect_equal(idx$expected, rows$estimated)
  expect_equal(idx$se, exp(rows$log_sd))
  expect_equal(idx$residual, log(rows$observed) - log(rows$estimated))
  expect_equal(idx$loglike, rows$likelihood)

  lnd <- prepare_fit_data(est, data_type = "landings")
  expect_equal(nrow(lnd), 30)
  expect_equal(unique(lnd$fleet), "fleet1")

  # Year indices are converted to years with start_year
  expect_equal(range(prepare_fit_data(est, start_year = 2001)$year), c(2001, 2030))
})

test_that("prepare_fit_data reads FIMS output passed through convert_output", {
  est <- fims_estimates()
  converted <- suppressMessages(convert_output(est, model = "fims"))
  expect_equal(
    prepare_fit_data(converted)$residual,
    prepare_fit_data(est)$residual
  )
})

test_that("prepare_fit_data matches Stock Synthesis residuals", {
  rep <- r4ss_simple()
  idx <- prepare_fit_data(rep)
  cpue <- rep$cpue[order(rep$cpue$Fleet_name, rep$cpue$Yr), ]
  expect_equal(idx$observed, cpue$Obs)
  expect_equal(idx$se, cpue$SE)
  # Stock Synthesis reports log(obs / exp) as Dev
  expect_equal(idx$residual, cpue$Dev, tolerance = 1e-4)

  lnd <- prepare_fit_data(rep, data_type = "landings")
  expect_true(all(lnd$observed > 0))
  expect_equal(unique(lnd$fleet), "FISHERY")
})

test_that("prepare_fit_data reads the standard output and tidy data", {
  idx <- prepare_fit_data(stockplotr::example_data)
  expect_setequal(unique(idx$fleet), c("Triennial", "WCGBTS"))
  expect_true(all(!is.na(idx$expected)))

  tidy <- data.frame(
    year = 1:4,
    observed = c(1, 2, 3, 4),
    expected = c(1.5, 2, 2.5, 4),
    se = 0.5,
    distribution = "normal"
  )
  fit <- prepare_fit_data(tidy)
  expect_equal(fit$residual, c(-0.5, 0, 0.5, 0))
  expect_equal(fit$std_residual, c(-1, 0, 1, 0))
  expect_equal(fit$fleet, rep("All", 4))
  expect_equal(nrow(prepare_fit_data(fit, fleet = "All")), 4)
})

test_that("prepare_fit_data errors without data", {
  expect_error(prepare_fit_data(data.frame(a = 1)), "Could not find")
  expect_error(prepare_fit_data(stockplotr::example_data, fleet = "none"), "No index data")
})

test_that("convert_output uses the model argument for objects", {
  expect_no_error(suppressMessages(convert_output(fims_estimates(), model = "fims")))
})

test_that("prepare_fit_data keeps every observation", {
  # Two seasons of an index in the same years are separate series
  long <- data.frame(
    label = rep(c("index_observed", "index_predicted"), each = 6),
    fleet = "Survey (north)",
    year = rep(rep(2001:2003, 2), 2),
    season = rep(rep(1:2, each = 3), 2),
    estimate = c(1, 2, 3, 10, 20, 30, 1.1, 2.1, 2.9, 11, 19, 31)
  )
  fit <- prepare_fit_data(long)
  expect_equal(nrow(fit), 6)
  expect_setequal(unique(fit$fleet), c("Survey (north) (season 1)", "Survey (north) (season 2)"))
  expect_equal(fit$observed[fit$fleet == "Survey (north) (season 2)"], c(10, 20, 30))

  # Exact copies are combined
  expect_equal(nrow(prepare_fit_data(rbind(long, long))), 6)

  # Values that cannot be told apart are an error
  long$season <- NULL
  expect_error(prepare_fit_data(long), "more than one value for 3 fleets and years")

  # As is more than one model
  two_models <- dplyr::bind_rows(
    dplyr::mutate(stockplotr::example_data, model = "base"),
    dplyr::mutate(stockplotr::example_data, model = "sensitivity")
  )
  expect_error(prepare_fit_data(two_models), "estimates from 2 models")

  # Tidy data and r4ss output with more than one observation per year
  tidy <- data.frame(fleet = "a", year = c(1, 1), observed = c(1, 2), expected = 1)
  expect_error(prepare_fit_data(tidy), "cannot be told apart")
  tidy$month <- c(3, 9)
  expect_setequal(prepare_fit_data(tidy)$fleet, c("a (month 3)", "a (month 9)"))

  rep <- r4ss_simple()
  extra <- rep$cpue
  extra$Seas <- 2
  extra$Obs <- extra$Obs * 2
  rep$cpue <- rbind(rep$cpue, extra)
  fit <- prepare_fit_data(rep)
  expect_equal(nrow(fit), 2 * nrow(extra))
  expect_setequal(unique(fit$fleet), c("SURVEY1 (season 1)", "SURVEY1 (season 2)", "SURVEY2 (season 1)", "SURVEY2 (season 2)"))
})
