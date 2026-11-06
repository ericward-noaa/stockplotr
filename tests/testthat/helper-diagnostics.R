# Test data and helpers for the fit diagnostic plots

# FIMS::get_estimates() output for the FIMS data_big example
# (see fixtures/fims_output/README.md)
fims_estimates <- function() {
  readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
}

# r4ss::SS_output() list for the Simple model
r4ss_simple <- function() {
  readRDS(test_path("fixtures", "r4ss_output", "simple_small", "simple_r4ss.rds"))
}

# Evaluate plotting code with `make_rda = TRUE` in a temporary directory and
# return the saved rda (a list with the figure, caption, and alt text).
# `captions` is an optional captions_alt_text.csv to start from.
fit_rda <- function(code, topic_label, captions = NULL) {
  dir <- tempfile("fit_rda")
  dir.create(dir)
  old <- setwd(dir)
  on.exit(
    {
      setwd(old)
      unlink(dir, recursive = TRUE)
    },
    add = TRUE
  )
  if (!is.null(captions)) {
    utils::write.csv(captions, "captions_alt_text.csv", row.names = FALSE)
  }
  suppressMessages(invisible(utils::capture.output(force(code))))
  rda <- NULL
  load(file.path("figures", paste0(topic_label, "_figure.rda")))
  rda
}

# Expect that every placeholder starting with `prefix` (e.g., "index.fit.")
# was replaced in the caption and alt text
expect_tags_filled <- function(rda, prefix) {
  expect_s3_class(rda$figure, "ggplot")
  expect_no_match(c(rda$caption, rda$alt_text), prefix, fixed = TRUE)
  expect_gt(nchar(rda$caption), 50)
  expect_gt(nchar(rda$alt_text), 100)
}
