#' @title Detailed Performance Spectrum
#'
#' @description
#' Plots the detailed performance spectrum. The performance spectrum describes the event data in terms of segments, i.e.,
#' pairs of related process steps. The performance of each segment is measured and plotted for any occurrences of this segment
#' over time and can be classified, e.g., regarding the overall population. The detailed performance spectrum visualises
#' variability of durations in a segment across cases and time (Denisov _et al._, 2018). See **References** for more details.
#' @param scale [`ggplot2`][`ggplot`] scale function (default [`scale_color_discrete_bupaR`][`bupaR::scale_color_discrete_bupaR`]):
#' Set color scale. Defaults to [`scale_color_discrete_bupaR`][`bupaR::scale_color_discrete_bupaR`]. 
#' 
#' @inherit ps_aggregated params references
#'
#' @return A [`ggplot2`][`ggplot`] object describing the detailed performance spectrum.
#'
#' @seealso [`ps_aggregated()`]
#'
#' @examples
#' \donttest{
#' library(psmineR)
#' library(eventdataR)
#'
#' sepsis %>%
#'  ps_detailed(segment_coverage = 0.2,
#'              classification = "quartile")
#' }
#' @export
ps_detailed <- function(log,
                        segment_coverage,
                        n_segments,
                        classification = NULL,
                        scale = NULL) {
  UseMethod("ps_detailed")
}

#' @describeIn ps_detailed Plot detailed performance spectrum for a [`log`][`bupaR::log`].
#' @export
ps_detailed.log <- function(log,
                            segment_coverage,
                            n_segments,
                            classification = NULL,
                            scale = NULL) {
  
  key <- NULL
  y <- NULL
  
  segment_coverage <- check_segment_args(maybe_missing(segment_coverage),
                                         maybe_missing(n_segments))

  classification <- check_classification_arg(log, classification)

  seg <- get_segments(log, segment_coverage, maybe_missing(n_segments, NULL), classification)

  # Transform to long format to construct the detailed performance spectrum
  melt(seg,
       measure.vars = c("ta", "tb"),
       variable.name = "key",
       variable.factor = FALSE,
       value.name = "x",
       value.factor = FALSE) -> seg

  seg[which(key %chin% c("ta", "tb")),
      y := fifelse(key == "ta", 1L, 0L)]
  # seg %>%
  #   gather(key, x, ta, tb) %>%
  #   mutate(y = if_else(key == "ta", 1, 0)) %>%
  #   filter(key %in% c("ta", "tb")) -> seg

  class(seg) <- c("ps_detailed", class(seg))

  # Create the plot
  seg %>%
    plot(classification, case_id(log), scale = scale)
}

#' @describeIn ps_detailed Plot detailed performance spectrum for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
ps_detailed.grouped_log <- function(log,
                                    segment_coverage,
                                    n_segments,
                                    classification = NULL,
                                    scale = NULL) {

  if (is.null(classification)) {
    classification <- as.character(groups(log)[[1L]])
  }

  ps_detailed.log(ungroup_eventlog(log),
                  maybe_missing(segment_coverage),
                  maybe_missing(n_segments),
                  classification)
}
