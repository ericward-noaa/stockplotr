# prepare_fit_data() and plot_index_fit() with Stock Synthesis and FIMS output.
# Every observation in the model output should be read, with the values the
# model reports.

### Stock Synthesis ----

test_that("SS3 indices from convert_output() match the converted rows", {
  rows <- dplyr::filter(
    stockplotr::example_data,
    label %in% c("index_observed", "index_predicted"), is.na(age), is.na(length_bins)
  )
  obs <- rows[rows$label == "index_observed", ]
  obs <- obs[order(obs$fleet, obs$year), ]
  pred <- rows[rows$label == "index_predicted", ]
  pred <- pred[order(pred$fleet, pred$year), ]
  fit <- prepare_fit_data(stockplotr::example_data)
  expect_equal(nrow(fit), nrow(obs))
  expect_equal(fit$observed, obs$estimate)
  expect_equal(fit$expected, pred$estimate)
  expect_equal(fit$se, as.numeric(obs$uncertainty))
  expect_equal(fit$residual, log(obs$estimate / pred$estimate))
})

test_that("SS3 landings from convert_output() keep every year with catch", {
  obs <- dplyr::filter(
    stockplotr::example_data,
    label == "landings_observed_weight", is.na(age), is.na(length_bins)
  )
  # The only rows not read are the forecast placeholder (no year) and years
  # without catch
  obs <- dplyr::filter(obs, era != "fore", estimate > 0)
  fit <- prepare_fit_data(stockplotr::example_data, data_type = "landings")
  expect_equal(nrow(fit), nrow(obs))
  expect_setequal(unique(fit$fleet), unique(obs$fleet))
  expect_equal(sort(fit$observed), sort(obs$estimate))
})

test_that("SS3 r4ss output keeps the initial equilibrium catch", {
  rep <- r4ss_simple()
  fit <- prepare_fit_data(rep, data_type = "landings")
  catch <- rep$catch[rep$catch$Obs > 0, ]
  expect_equal(nrow(fit), nrow(catch))
  expect_true((rep$startyr - 1) %in% fit$year)
  expect_equal(fit$observed, catch$Obs[order(catch$Yr)])
  expect_equal(fit$se, catch$se[order(catch$Yr)])
  expect_equal(fit$loglike, -catch$Like[order(catch$Yr)])
})

test_that("SS3 r4ss output and convert_output() give the same fits", {
  rep <- r4ss_simple()
  long <- suppressMessages(convert_output(rep))
  for (type in c("index", "landings")) {
    a <- prepare_fit_data(rep, data_type = type)
    b <- prepare_fit_data(long, data_type = type)
    expect_equal(b$fleet, a$fleet)
    expect_equal(b$year, a$year)
    expect_equal(b$observed, a$observed, tolerance = 1e-6)
    expect_equal(b$expected, a$expected, tolerance = 1e-6)
    expect_equal(b$se, a$se)
  }
})

test_that("SS3 observations not used in the fit are kept and shown as open points", {
  rep <- r4ss_simple()
  rep$cpue$Use[1] <- -1
  fit <- prepare_fit_data(rep)
  expect_equal(nrow(fit), nrow(rep$cpue))
  expect_false(fit$used[fit$fleet == rep$cpue$Fleet_name[1] & fit$year == rep$cpue$Yr[1]])
  p <- plot_index_fit(rep)
  expect_equal(sum(p$data$used == "Not used"), 1)
  expect_no_error(ggplot2::ggplot_build(p))
})

### FIMS ----

test_that("FIMS landings match the FIMS estimates", {
  est <- fims_estimates()
  rows <- est[est$label == "catch_expected" & !is.na(est$observed), ]
  fit <- prepare_fit_data(est, data_type = "landings")
  expect_equal(nrow(fit), nrow(rows))
  expect_equal(fit$observed, rows$observed)
  expect_equal(fit$expected, rows$estimated)
  expect_equal(fit$se, exp(rows$log_sd))
  expect_equal(fit$loglike, rows$likelihood)
  expect_equal(
    prepare_fit_data(suppressMessages(convert_output(est, model = "fims")), data_type = "landings")$observed,
    fit$observed
  )
})

test_that("FIMS observations coded as missing are excluded and no others", {
  est <- fims_estimates()
  idx <- which(est$label == "index_expected" & !is.na(est$observed))
  est$observed[idx[c(3, 7)]] <- -999
  fit <- prepare_fit_data(est)
  expect_equal(nrow(fit), length(idx) - 2)
  expect_false(any(c(3, 7) %in% fit$year))
})

test_that("FIMS fleets are read separately and repeated runs are an error", {
  est <- fims_estimates()
  survey2 <- est[est$label == "index_expected", ]
  survey2$fleet <- "survey2"
  survey2$observed <- survey2$observed * 2
  fit <- prepare_fit_data(rbind(est, survey2))
  expect_setequal(unique(fit$fleet), c("survey1", "survey2"))
  expect_equal(nrow(fit), 60)

  # Two runs of a model bound together cannot be told apart
  run2 <- est
  run2$estimated <- run2$estimated * 1.1
  expect_error(prepare_fit_data(rbind(est, run2)), "cannot be told apart")
  # Exact copies are combined
  expect_equal(nrow(prepare_fit_data(rbind(est, est))), 30)
})

test_that("FIMS intervals use the observation error", {
  est <- fims_estimates()
  p <- plot_index_fit(est, interval = 0.9, start_year = 2001)
  z <- stats::qnorm(0.95)
  expect_equal(p$data$lower, p$data$observed * exp(-z * p$data$se))
  expect_equal(p$data$upper, p$data$observed * exp(z * p$data$se))
  expect_equal(range(p$data$year), c(2001, 2030))
})

test_that("a FIMSFit object needs FIMS", {
  skip_if(requireNamespace("FIMS", quietly = TRUE))
  methods::setClass("FIMSFit", representation(x = "numeric"), where = environment())
  expect_error(prepare_fit_data(methods::new("FIMSFit", x = 1)), "FIMS")
})

test_that("observations without a year are skipped with a warning", {
  tidy <- data.frame(fleet = "a", year = c(1, NA, 3), observed = 1:3, expected = 1)
  expect_warning(fit <- prepare_fit_data(tidy), "Skipped 1 index observation without a year")
  expect_equal(fit$year, c(1, 3))
})
