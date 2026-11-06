test_that("index and landings fit plots work for each model", {
  for (dat in list(fims_estimates(), r4ss_simple(), stockplotr::example_data)) {
    expect_s3_class(plot_index_fit(dat), "ggplot")
    expect_s3_class(plot_index_fit(dat, log = TRUE, interval = 0.9), "ggplot")
    expect_s3_class(plot_landings_fit(dat), "ggplot")
  }
  p <- plot_index_fit(stockplotr::example_data)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(p$labels$y, "Index")
  expect_equal(plot_landings_fit(stockplotr::example_data)$labels$y, "Landings")
  expect_equal(plot_index_fit(fims_estimates(), start_year = 2001)$data$year[1], 2001)
})

test_that("index fit caption tags describe the data and options", {
  fits <- prepare_fit_data(stockplotr::example_data)
  fits$lower <- fits$observed * exp(-1.96 * fits$se)
  fits$upper <- fits$observed * exp(1.96 * fits$se)
  fits$used <- factor(ifelse(fits$used, "Used", "Not used"), levels = c("Used", "Not used"))
  tags <- fit_tags_fit(fits, "index", log = TRUE, interval = 0.95)
  expect_equal(tags$index.fit.fleets, "Triennial and WCGBTS")
  expect_equal(tags$index.fit.scale.desc, "on a log scale")
  expect_match(tags$index.fit.summary, "within the 95% interval of 26 of 28 observations")
  expect_equal(tags$index.fit.used.desc, "All observations were used in the model fit.")

  fits$used[1] <- "Not used"
  tags <- fit_tags_fit(fits, "landings", log = FALSE, interval = 0.9)
  expect_equal(tags$landings.fit.interval, 90)
  expect_match(tags$landings.fit.used.desc, "Open points")
})

test_that("make_rda saves index and landings fits with captions and alt text", {
  rda <- fit_rda(plot_index_fit(stockplotr::example_data, log = TRUE, make_rda = TRUE), "index_fit")
  expect_tags_filled(rda, "index.fit.")
  expect_match(rda$caption, "for Triennial and WCGBTS, 1980-2022, on a log scale")

  rda <- fit_rda(plot_landings_fit(stockplotr::example_data, make_rda = TRUE), "landings_fit")
  expect_tags_filled(rda, "landings.fit.")
  expect_match(rda$caption, "landings for North and South")

  # A captions file made before these plots existed is updated
  template <- utils::read.csv(
    system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
  )
  old_caps <- template[!template$label %in% c("index_fit", "landings_fit"), ]
  rda <- fit_rda(plot_index_fit(stockplotr::example_data, make_rda = TRUE), "index_fit", captions = old_caps)
  expect_tags_filled(rda, "index.fit.")
})
