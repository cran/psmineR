#' @title Aggregated Performance Spectrum
#'
#' @description
#' Plots the aggregated performance spectrum. The performance spectrum describes the event data in terms of segments, i.e.,
#' pairs of related process steps. The performance of each segment is measured and plotted for any occurrences of this segment
#' over time and can be classified, e.g., regarding the overall population. The aggregated performance spectrum visualises
#' the amount of cases of particular performance over time (Denisov _et al._, 2018). See **References** for more details.
#'
#' @param log [`log`][`bupaR::log`]: Object of class [`log`][`bupaR::log`] or derivatives ([`grouped_log`][`bupaR::grouped_log`],
#' [`eventlog`][`bupaR::eventlog`], [`activitylog`][`bupaR::activitylog`], etc.).
#' @param segment_coverage,n_segments \code{\link{numeric}}: Provide either `segment_coverage` or `n_segments`. If neither is
#' provided, `segment_coverage = 0.2` will be used.\cr
#' `segment_coverage`: The percentage of cases (default `0.2`) in which each segment must be present to be visualised in the spectrum.
#' Ignored if `n_segments` is specified.\cr
#' `n_segments`: Visualise only the top `n` segments based on frequency.
#' @param classification [`character`] (default [`NULL`]): The variable defining the colour legend. This variable should be present in `log`.\cr
#' If [`NULL`] (default) when `log` is a [`grouped_log`][`bupaR::grouped_log`], the first grouping variable will be used as `classification`.\cr
#' If [`NULL`] (default) or `"quartile"` when `log` is an [`eventlog`][`bupaR::eventlog`] or [`activitylog`][`bupaR::activitylog`],
#' a quartile variable dividing the durations of the segments in quartiles is calculated.
#' @param grouping [`character`] (default `"start"`): The timestamps, `"start"` or `"complete"`, which are binned in the histogram.
#' @param bins [`numeric`] (default `30`): The number of bins in the aggregated performance spectrum.
#' @param scale [`ggplot2`] scale function (default [`scale_fill_discrete_bupaR`][`bupaR::scale_fill_discrete_bupaR`]):
#' Set color scale. Defaults to [`scale_fill_discrete_bupaR`][`bupaR::scale_fill_discrete_bupaR`]. 
#' 
#' @return A [`ggplot2`] object describing the aggregated performance spectrum.
#'
#' @seealso [`ps_detailed()`]
#'
#' @references
#' Denisov, V., Fahland, D., & van der Aalst, W. M. P. (2018). Unbiased, Fine-Grained Description of Processes Performance from Event Data.
#' In M. Weske, M. Montali, I. Weber, & J. vom Brocke (Eds.), Proceedings of the 16th International Conference on Business Process Management
#' (Vol. 11080, pp. 139–157). Springer International Publishing. \doi{10.1007/978-3-319-98648-7_9}
#'
#' @examples
#' library(psmineR)
#' library(eventdataR)
#'
#' sepsis %>%
#'  ps_aggregated(segment_coverage = 0.2,
#'                classification = "quartile",
#'                grouping = "start",
#'                bins = 15)
#'
#' @export ps_aggregated
ps_aggregated <- function(log,
                          segment_coverage,
                          n_segments,
                          classification = NULL,
                          grouping = c("start", "complete"),
                          scale = NULL,
                          bins = 30) {
  UseMethod("ps_aggregated")
}

#' @describeIn ps_aggregated Plot aggregated performance spectrum for a [`log`][`bupaR::log`].
#' @export
ps_aggregated.log <- function(log,
                              segment_coverage,
                              n_segments,
                              classification = NULL,
                              grouping = c("start", "complete"),
                              scale = NULL,
                              bins = 30) {

  grouping <- arg_match(grouping)

  segment_coverage <- check_segment_args(maybe_missing(segment_coverage),
                                         maybe_missing(n_segments))

  classification <- check_classification_arg(log, classification)

  seg <- get_segments(log, segment_coverage, maybe_missing(n_segments, NULL), classification)

  class(seg) <- c("ps_aggregated", class(seg))

  # Select necessary columns
  # log <- eventlog %>%
  #   as.data.frame() %>%
  #   select(case_id, activity, lifecycle, timestamp, .order)

  # log <- log %>% preprocess_data(segment_coverage, classification_attribute)

  # Create the plot
  #plot_aggregated(seg, "CLASSIFICATION", grouping, bins)
  seg %>%
    plot(classification, grouping, bins, scale)
}

#' @describeIn ps_aggregated Plot aggregated performance spectrum for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
ps_aggregated.grouped_log <- function(log,
                                      segment_coverage,
                                      n_segments,
                                      classification = NULL,
                                      grouping = c("start", "complete"),
                                      scale = NULL,
                                      bins = NULL) {

  if (is.null(classification)) {
    classification <- as.character(groups(log)[[1L]])
  }

  ps_aggregated.log(ungroup_eventlog(log),
                    maybe_missing(segment_coverage),
                    maybe_missing(n_segments),
                    classification,
                    grouping,
                    bins)
}


get_segments <- function(log, segment_coverage, n_segments, classification) {

  log %>%
    construct_segments(classification) %>%
    build_classifier(classification) %>%
    filter_segments(log = log, segment_coverage = segment_coverage, n_segments = n_segments) %>%
    order_segments(log) -> seg

  return(seg)
}

check_segment_args <- function(segment_coverage, n_segments, call = caller_env()) {

  if (is_missing(segment_coverage) && is_missing(n_segments)) {
    return(0.2)
  } else if (!is_missing(segment_coverage) && is_missing(n_segments)) {
    if (!is.numeric(segment_coverage) || is.na(segment_coverage) || segment_coverage < 0 || segment_coverage > 1) {
      cli_abort(c("{.arg segment_coverage} must be a {.cls numeric} between 0 and 1.",
                  "x" = "You supplied a {.cls {class(segment_coverage)}}: {.val {segment_coverage}}"),
                call = call)
    } else {
      return(segment_coverage)
    }
  } else if (!is_missing(n_segments) && is_missing(segment_coverage)) {
    if (!is_integerish(n_segments, n = 1) || is.na(n_segments) || n_segments < 0) {
      cli_abort(c("{.arg n_segments} must be an interger-like {.cls numeric} larger than 0.",
                  "x" = "You supplied a {.cls {class(n_segments)}}: {.val {n_segments}}"),
                call = call)
    }
  } else {
    cli_abort("Must supply {.arg segment_coverage} or {.arg n_segments}, but not both.",
              call = call)
  }
}

check_classification_arg <- function(log, classification, arg = caller_arg(classification), call = caller_env()) {

  if (is.null(classification)) {
    return("quartile")
  } else if (classification != "quartile" && !(classification %in% colnames(log))) {
    cli_abort(c("Invalid {.arg {arg}}.",
                "x" = "{.val {classification}} is not present in log."),
              call = call)
  }

  return(classification)
}
