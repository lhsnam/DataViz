theme_set(
  theme(plot.title = element_text(size = 30, face = "bold", vjust = -10, hjust = 0.5, family = "Helvetica"),
                plot.subtitle = element_text(size = 20, color = "grey50", vjust = -16, hjust = 0.5, family = "Helvetica"),
                axis.title.x = element_text(vjust = -6, hjust = 0.5, size = 20, family = "Helvetica"),
                axis.text.x = element_text(vjust = -3, size = 15, family = "Helvetica"),
                axis.line.x = element_line(size = .1, color="grey70"),
                axis.line.y = element_blank(),
                plot.margin = unit(c(0.6,0.5,0.5,0.5),"in"),
                aspect.ratio = 4/7,
                axis.title.y = element_blank(),
                strip.placement = "outside",
                strip.background = element_rect(fill=NA,color= "grey80"),
                legend.position = "none")
)