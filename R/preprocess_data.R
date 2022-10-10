
construct_segments <- function(log, classification) {
  start_t <- NULL
  end_t <- NULL
  segment <- NULL
  ACTIVITY <- NULL
  ACTIVITY_INSTANCE <- NULL
  ya <- NULL
  yb <- NULL
  
  
  log %>%
    data.table() -> dt

  if (is.eventlog(log)) {
    dt <- construct_segments.eventlog(log, classification, dt)
  } else if (is.activitylog(log)) {
    dt <- construct_segments.activitylog(log, classification, dt)
  }

  setnames(dt, activity_id(log), "ACTIVITY")
  by_case <- case_id(log)

  if(classification == "quartile") {
    dt[, .(ACTIVITY,
           ACTIVITY_INSTANCE,
           start_t,
           end_t,
           yb = shift(ACTIVITY, n = 1L, type = "lead"),
           tb = shift(start_t, n = 1L, type = "lead")),
         by = by_case] -> dt
  } else {
    dt[, .(ACTIVITY,
           ACTIVITY_INSTANCE,
           start_t,
           end_t,
           get(classification),
           yb = shift(ACTIVITY, n = 1L, type = "lead"),
           tb = shift(start_t, n = 1L, type = "lead")),
         by = by_case] -> dt

    # For some reason, the classification column is renamed to "V5" => manually rename it back to the value of classification.
    setnames(dt, "V5", classification)
  }

  dt <- na.omit(dt, cols = "yb")
  setnames(dt, c("ACTIVITY", "end_t"), c("ya", "ta"))

  dt[, segment := stringi::stri_c(ya, yb, sep = " >> ")]

  return(dt)
}

construct_segments.eventlog <- function(log, classification, dt) {
  
  TIMESTAMP_CLASSIFIER <- NULL
  
  setnames(dt, c(timestamp(log), activity_instance_id(log)), c("TIMESTAMP_CLASSIFIER", "ACTIVITY_INSTANCE"))

  if(classification == "quartile") {
    dt[, .(start_t = min(TIMESTAMP_CLASSIFIER),
           end_t = max(TIMESTAMP_CLASSIFIER)),
         by = c(case_id(log), activity_id(log), "ACTIVITY_INSTANCE")] -> dt
  } else {
    #setnames(dt, classification, "CLASSIFICATION")

    dt[, .(start_t = min(TIMESTAMP_CLASSIFIER),
           end_t = max(TIMESTAMP_CLASSIFIER)),
         by = c(case_id(log), activity_id(log), "ACTIVITY_INSTANCE", classification)] -> dt
  }

  return(dt)
}

construct_segments.activitylog <- function(log, classification, dt) {
  ACTIVITY_INSTANCE <- NULL
  
  TIMESTAMP_CLASSIFIERS <- get_col_index(dt, timestamps(log))

  dt[, ACTIVITY_INSTANCE := .I]

  if (classification == "quartile") {
    dt[, ":="(start_t = do.call(pmin, c(na.rm = TRUE, .SD)),
              end_t = do.call(pmax, c(na.rm = TRUE, .SD))),
         .SDcols = TIMESTAMP_CLASSIFIERS] -> dt
  } else {
    #setnames(dt, classification, "CLASSIFICATION")

    dt[, ":="(start_t = do.call(pmin, c(na.rm = TRUE, .SD)),
              end_t = do.call(pmax, c(na.rm = TRUE, .SD))),
         .SDcols = TIMESTAMP_CLASSIFIERS,
         by = c(case_id(log), activity_id(log), "ACTIVITY_INSTANCE", classification)] -> dt
  }

  return(dt)
}

# Needs the construct_segments data.table
build_classifier <- function(dt, classification) {
  delta <- NULL
  ta <- NULL
  tb <- NULL

  if(classification == "quartile") {
    dt[, delta := as.double(tb - ta, units = "days")]

    dt[, (classification) := fcase(delta <= quantile(delta, 0.25), 1L,
                                   delta <= quantile(delta, 0.5), 2L,
                                   delta <= quantile(delta, 0.75), 3L,
                                   default = 4L),
         by = "segment"][,
         (classification) := as.factor(get(classification))]

    # Old code => replaced by above
    # log <- log %>%
    #   mutate(delta = as.double(tb - ta, units = "days")) %>%
    #   group_by(segment) %>%
    #   mutate(
    #     CLASSIFICATION = if_else(
    #       delta <= quantile(delta, 0.25), 1,
    #       if_else(
    #         delta <= quantile(delta, 0.5), 2,
    #         if_else(
    #           delta <= quantile(delta, 0.75), 3, 4
    #         )
    #       )
    #     ),
    #     CLASSIFICATION = as.factor(CLASSIFICATION)
    #   )
  }

  return(dt)
}

# Needs the build_classifier data.table + original log for mapping vars
filter_segments <- function(dt, log, segment_coverage = NULL, n_segments = NULL) {

  cases <- n_cases(log)

  dt[, .(n_cases = uniqueN(get(case_id(log))),
         n = .N),
       by = "segment"] -> counts

  #dt %>%
  #  group_by(segment) %>%
  #  summarize(n_cases = n_distinct(!!bupaR:::case_id_(log)), n = n()) -> counts

  if(is.null(n_segments)) {
    counts[n_cases >= segment_coverage * cases] -> counts
  } else {
    setorderv(counts, cols = "n", order = -1L)
    counts[1:n_segments] -> counts
    #counts %>%
    #  arrange(-n) %>%
    #  slice(1:n_segments) -> counts
  }

  # semi_join dt with counts
  setkeyv(counts, cols = "segment")
  dt <- unique(dt[counts$segment, on = "segment", nomatch = 0L])

  #dt %>%
  #  semi_join(counts, by = "segment")

  return(dt)
}

# Needs the filter_segments data.table + original log for mapping vars
order_segments <- function(dt, log) {
  i <- NULL
  setorderv(dt, cols = "start_t")
  dt[, i := rowidv(dt, cols = case_id(log))][,
       .(seg_order = median(i)),
       by = "segment"] -> seg_ordering

  dt <- seg_ordering[dt, on = "segment"]

  return(dt)

  # seg_ordering <- log %>%
  #   group_by(!!bupaR:::case_id_(mapping)) %>%
  #   mutate(i = row_number(start_t)) %>%
  #   group_by(segment) %>%
  #   summarise(seg_order = median(i))
  #
  # log <- left_join(log, seg_ordering, by = "segment")
}
