
plot_ps_aggregated <- function(x, classification, grouping, bins, scale) {

  if(grouping == "start") plot_x <- "ta" else plot_x <- "tb"

  x %>%
    ggplot(aes_(sym(plot_x), fill = sym(classification))) +
      geom_histogram(bins = bins) +
      facet_grid(fct_reorder(segment, seg_order) ~ ., switch = "y", scales = "free_y") +
      theme_bw() +
      theme(strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank(),
            panel.spacing.y = unit(0, "lines")) -> p

  if(is.null(scale)) {
    if(classification == "quartile") {
      p <- p + scale_fill_brewer(palette = "Blues")
    } else {
      p <- p + scale_fill_discrete_bupaR()
    } }
  else {
      p <- p + scale()
    }
  
  
  return(p)
}
