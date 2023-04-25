```{r}
area.plot <- dt04_abund %>%
    ggplot() + 
    geom_area(aes(x=day, y=mean_abundance, group = OTUID, fill = OTUID), stat = "identity", position = position_fill(reverse = TRUE), color = "white") +
    geom_text(data = day1.04en_abund, aes(x = 0.94, y = c(0.22, 0.04, 0.61, 0.845, 0.92), label = formatC(round(mean_abundance,2),2,format="f"), size = 25), hjust = 1, nudge_x = 0.05, color = "black", size = 5) +
    geom_text(data = day14.04en_abund, aes(x = 3.01, y = c(0.33, 0.1, 0.7, 0.93, 0.97), hjust = 0, label = round(mean_abundance, 2), size = 25), color = "black", size = 5) +
    geom_label(data = day1.04en_abund, aes(x= 0.7, y = c(0.22, 0.04, 0.61, 0.845, 0.92), label= OTUID, fill = OTUID, size = 25), hjust = 1, color = "black", label.size = NA, family = "Helvetica") +
    scale_fill_manual(values = phyla.col) +
    labs(title = "Relative abundance of 04EN data",
       subtitle = "Top highest mean abundance phyla",
       x = "Day",
       y = "Mean relative abundance ") +
    scale_x_discrete(expand = expansion(mult = c(1,1))) +
    scale_y_continuous(expand = expansion(mult = c(0.01,0.2))) +
    theme_void() +
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
        #legend.key.height= unit(0.4, 'in'),
        #legend.key.width= unit(0.4, 'in'),
        #legend.title = element_text(size=17, face = "bold"),
        #legend.text = element_text(size=12)) + 
    #guides(fill = guide_legend(reverse = TRUE, title="Phylum")) +
)
```

#Area plot of the top highest relative abundance:
<img src="https://github.com/lhsnam/DataViz-in-R/blob/52c582d4ac592fb488637c22021fcb053a4ad9f1/themeR/Area%20Plot/area_non_legend.png" alt="Area plot" width="77%" height="77%"/>
