patient_df <- read.csv('/data/namlhs/visualization/t2d_probs.csv', sep = '\t')

patient_df$METSIM_ID <- as.factor(patient_df$METSIM_ID)
patient_df$DMType <- as.factor(patient_df$DMType)

library(ggplot2)

(plot <- ggplot(patient_df, aes(x = Time_Point, y = Proba)) +
    geom_line(aes(group = METSIM_ID), alpha = 0.3) +
    geom_point(aes(color = DMType), size = 2, alpha = 0.75) +
    scale_color_discrete(labels = c('No', 'Yes')) +
    scale_x_continuous(limits = c(0,48),
                       expand = c(0.01,0),
                       breaks = c(0, 18, 48)) +
    scale_y_continuous(expand = c(0.01,0)) +
    labs(title = 'Diabetes probatbilities predicted by time point',
         subtitle = 'Logistic regression model on Demographic, Diet factors, and top 50 abundant OTUs',
         color = 'Diabetes',
         x = 'Timepoint (months after admission)',
         y = 'Diabetes probabilities') +
    theme_classic() +
    theme(axis.line = element_line(color = 'grey70'),
          title = element_text(face = 'bold', size = 10),
          subtitle)
  )

ggsave('/data/namlhs/visualization/t2d_probs.pdf',
       plot,
       height = 7,
       width = 7,
       dpi = 'retina')
