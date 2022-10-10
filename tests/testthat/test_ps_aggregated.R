
#### eventlog ####

test_that("test ps_aggregated on eventlog", {

  load("./testdata/patients.rda")

  # No errors with default params
  expect_error(
    ps <- patients %>%
      ps_aggregated(),
    NA
  )
  expect_s3_class(ps, "ggplot")

  # No errors with 'segment_coverage' param
  expect_error(
    ps <- patients %>%
      ps_aggregated(segment_coverage = 0.5),
    NA
  )
  expect_s3_class(ps, "ggplot")

  # No errors with 'n_segments' param
  expect_error(
    ps <- patients %>%
      ps_aggregated(n_segments = 2),
    NA
  )
  expect_s3_class(ps, "ggplot")

  # No errors with 'classification' param
  expect_error(
    ps <- patients %>%
      ps_aggregated(classification = "resource"),
    NA
  )
  expect_s3_class(ps, "ggplot")

  # No errors with 'grouping' = "start"
  expect_error(
    ps <- patients %>%
      ps_aggregated(grouping = "start"),
    NA
  )
  expect_s3_class(ps, "ggplot")

  # No errors with 'grouping' = "complete"
  expect_error(
    ps <- patients %>%
      ps_aggregated(grouping = "complete"),
    NA
  )
  expect_s3_class(ps, "ggplot")

  # No errors nor warnings with 'bins' param
  expect_error(
    ps <- patients %>%
      ps_aggregated(bins = 20),
    NA
  )
  expect_warning(
    ps <- patients %>%
      ps_aggregated(bins = 20),
    NA
  )
  expect_s3_class(ps, "ggplot")
})

test_that("test ps_aggregated on eventlog fails when 'segment_coverage' != [0,1]", {

  load("./testdata/patients.rda")

  expect_snapshot_error(
    patients %>%
      ps_aggregated(segment_coverage = -0.1)
  )

  expect_snapshot_error(
    patients %>%
      ps_aggregated(segment_coverage = 2)
  )

  expect_snapshot_error(
    patients %>%
      ps_aggregated(segment_coverage = "0.5")
  )
})

test_that("test ps_aggregated on eventlog fails when 'n_segments' < 0 or not an integer", {

  load("./testdata/patients.rda")

  expect_snapshot_error(
    patients %>%
      ps_aggregated(n_segments = -1)
  )

  expect_snapshot_error(
    patients %>%
      ps_aggregated(n_segments = 2.5)
  )

  expect_snapshot_error(
    patients %>%
      ps_aggregated(n_segments = "5")
  )
})

test_that("test ps_aggregated on eventlog fails when both 'segment_coverage' and 'n_segments' are provided", {

  load("./testdata/patients.rda")

  expect_snapshot_error(
    patients %>%
      ps_aggregated(segment_coverage = 0.2, n_segments = 5)
  )
})

test_that("test ps_aggregated on eventlog fails on invalid classification", {

  load("./testdata/patients.rda")

  expect_snapshot_error(
    patients %>%
      ps_aggregated(classification = "var")
  )
})

test_that("test ps_aggregated on grouped_eventlog", {

  load("./testdata/patients.rda")

  # No errors with default params
  expect_error(
    ps <- patients %>%
      group_by_resource() %>%
      ps_aggregated(),
    NA
  )

  expect_s3_class(ps, "ggplot")

  # 'fill' of ggplot should be set to grouping var ("resource")
  expect_equal(rlang::as_label(ps$mapping$fill), "resource")
})


#### activitylog ####

test_that("test ps_aggregated on activitylog", {

  load("./testdata/patients_act.rda")

  # No errors with default params
  expect_error(
    ps <- patients_act %>%
      ps_aggregated(),
    NA
  )
  expect_s3_class(ps, "ggplot")
})

test_that("test ps_aggregated on grouped_activitylog", {

  load("./testdata/patients_act.rda")

  # No errors with default params
  expect_error(
    ps <- patients_act %>%
      group_by_resource() %>%
      ps_aggregated(),
    NA
  )

  expect_s3_class(ps, "ggplot")

  # 'fill' of ggplot should be set to grouping var ("resource")
  expect_equal(rlang::as_label(ps$mapping$fill), "resource")
})