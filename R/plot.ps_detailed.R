
plot_ps_detailed <- function(x, classification, label, scale) {
  
  x %>%
    ggplot(aes_(~x, ~y, label = sym(label), group = ~ACTIVITY_INSTANCE, colour = sym(classification))) +
    geom_line() +
    facet_grid(fct_reorder(segment, seg_order) ~ ., switch = "y") +
    theme_bw() +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0, "lines")) -> p
  
  if(is.null(scale)) {
    if(classification == "quartile") {
      p <- p + scale_color_brewer(palette = "RdYlBu", direction = -1)
    } else {
      p <- p + scale_color_discrete_bupaR()
    }
    } else
      p <- p + scale()
  
  return(p)
}
