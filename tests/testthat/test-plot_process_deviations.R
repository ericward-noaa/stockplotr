fims_dat <- function() {
  suppressMessages(convert_output(
    test_path("fixtures", "fims_output", "fims_estimates.rds"),
    model = "fims",
    start_year = 1990
  ))
}

test_that("process deviations are classified by label", {
  expect_equal(
    classify_process_deviation(c(
      "log_recruitment_deviations", "recruitment_deviations",
      "log_F_deviations", "log_F_deviations_discard", "q_RW_log_dev",
      "log.q.dev.rwq", "index_q_dev", "sel_inf_dev", "log_sel_slp_dev",
      "init_dev", "dev", "raw_dev", "discard_deviation", "cohortgrowdev",
      "spawning_biomass"
    )),
    c(
      "recruitment", "recruitment", "fishing_mortality",
      "fishing_mortality_discards", "catchability", "catchability",
      "catchability", "selectivity", "selectivity", "abundance",
      NA, NA, NA, NA, NA
    )
  )
})

test_that("plot_process_deviations plots FIMS recruitment deviations with intervals", {
  p <- plot_process_deviations(fims_dat())
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLinerange" %in% geoms)
  points <- built$data[[which(geoms == "GeomPoint")]]
  expect_equal(nrow(points), 29)
  # 95% interval is estimate +/- 1.96 SE
  d <- process_deviations_data(fims_dat())
  ranges <- built$data[[which(geoms == "GeomLinerange")]]
  expect_equal(
    ranges$ymax - ranges$y,
    stats::qnorm(0.975) * d$uncertainty,
    tolerance = 1e-8
  )
  # No intervals when interval = NULL
  p0 <- plot_process_deviations(fims_dat(), interval = NULL)
  expect_false("GeomLinerange" %in% vapply(p0$layers, function(l) class(l$geom)[1], character(1)))
})

test_that("BAM deviations are split by process and fleet, one source each", {
  d <- process_deviations_data(vermilion_snapper)
  expect_setequal(
    unique(d$process_name),
    c("recruitment", "fishing_mortality", "fishing_mortality_discards", "catchability")
  )
  # Recruitment deviations are in both parm.tvec and t.series; only one is used
  expect_equal(dplyr::n_distinct(d$module_name[d$process_name == "recruitment"]), 1)
  expect_false(anyDuplicated(d[c("panel", "year")]) > 0)
  # Catchability deviations fixed at zero are left out
  expect_false(any(c("sft", "stv") %in% d$fleet[d$process_name == "catchability"]))
  expect_true("F: chl" %in% levels(d$panel))
  expect_s3_class(plot_process_deviations(vermilion_snapper), "ggplot")
})

test_that("process argument selects processes", {
  d <- process_deviations_data(vermilion_snapper, process = c("recruitment", "catchability"))
  expect_setequal(unique(d$process_name), c("recruitment", "catchability"))
  expect_error(plot_process_deviations(vermilion_snapper, process = "growth"), "Unknown")
})

test_that("SS3 index residuals are not process deviations", {
  d <- process_deviations_data(ss3_simple)
  expect_equal(unique(d$process_name), "recruitment")
  expect_false(any(grepl("cpue|index", d$module_name, ignore.case = TRUE)))
  d <- process_deviations_data(example_data)
  expect_equal(unique(d$module_name), "PARAMETERS")
})

test_that("deviations without years are left out with a message", {
  expect_message(
    d <- process_deviations_data(pollock),
    "no year"
  )
  expect_equal(unique(d$process_name), "recruitment")
})

test_that("multiple models are coloured by model", {
  dat <- list(
    a = vermilion_snapper,
    b = dplyr::mutate(vermilion_snapper, estimate = estimate * 0.8)
  )
  dat <- lapply(dat, function(x) dplyr::select(x, -dplyr::any_of("model")))
  p <- plot_process_deviations(dat, process = "recruitment")
  expect_setequal(unique(p$data$model), c("a", "b"))
  expect_false(is.null(p$mapping$colour))
})

test_that("no process deviations is an error", {
  dat <- data.frame(label = "spawning_biomass", estimate = 1, year = 2000)
  expect_error(plot_process_deviations(dat), "No process deviations")
})

test_that("rda file is made when indicated", {
  dir <- withr::local_tempdir()
  plot_process_deviations(fims_dat(), make_rda = TRUE, figures_dir = dir)
  rda_file <- file.path(dir, "figures", "process_deviations_figure.rda")
  expect_true(file.exists(rda_file))
  env <- new.env()
  load(rda_file, envir = env)
  rda <- get(ls(env)[1], envir = env)
  expect_match(rda$caption, "recruitment")
  expect_match(rda$alt_text, "from 1991 to 2019")
})
