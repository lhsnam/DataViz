setwd("D:/Work/April 24")
df <- read.csv("factors.csv")

library(ggplot2)

pos <- subset(df, coef >0)$factors
df <- mutate(df, corr = ifelse(df$factors %in% pos, 'positive', 'negative'))

demo_df <- filter(df, factors %in% c('WHR', 'BMI', 'Age', 'systbp', 'diastbp', 'fmass'))
factor_list1 <- demo_df[order(demo_df$coef),'factors']

nutri_df <- filter(df, !factors %in% c('WHR', 'BMI', 'Age', 'systbp', 'diastbp', 'fmass'))
factor_list2 <- nutri_df[order(nutri_df$coef),'factors']

(plot1 <- ggplot(demo_df, aes(coef, factors)) +
    geom_col(aes(fill = corr)) +
    geom_vline(xintercept = 1, color = "white") +
    geom_vline(xintercept = 0.5, color = "white") +
    scale_y_discrete(limits = factor_list1,
                     expand = c(0,0)) + 
    scale_x_continuous(limits = c(-1.5, 1.5),
                       breaks = seq(-1.5, 1.5, by = 0.5),
                       position = 'top') +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(color = 'grey90'),
          aspect.ratio = 3/14)
  )

(plot2 <- ggplot(nutri_df, aes(coef, factors)) +
    geom_col(aes(fill = corr)) +
    geom_vline(xintercept = 1, color = "white") +
    geom_vline(xintercept = 0.5, color = "white") +
    geom_vline(xintercept = -0.5, color = "white") +
    geom_vline(xintercept = -1, color = "white") +
    scale_y_discrete(limits = factor_list2,
                     expand = c(0,0)) + 
    scale_x_continuous(limits = c(-1.5, 1.5),
                       breaks = seq(-1.5, 1.5, by = 0.5),
                       position = 'bottom') +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(color = 'grey90'),
          aspect.ratio = 14/14)
)

ggplot2::ggsave(filename = "demo.pdf", 
                plot = plot1,
                device = "pdf", 
                path = "D:/Work/April 24/image", 
                width = 7, 
                height = 1.5, 
                units = "in",
                dpi = 1000,
                limitsize = TRUE,
                bg = "white")

ggplot2::ggsave(filename = "nutri.pdf", 
                plot = plot2,
                device = "pdf", 
                path = "D:/Work/April 24/image", 
                width = 7, 
                height = 7, 
                units = "in",
                dpi = 1000,
                limitsize = TRUE,
                bg = "white")

mat <- matrix(c(287, 122, 12, 9), nrow = 2, ncol = 2)

rownames(mat) <- c('non', 'yes')
colnames(mat) <- c('non', 'yes')

library(tidyr)
library(tibble)
df_acc <- mat %>% 
  as.data.frame() %>%
  rownames_to_column("predict") %>%
  pivot_longer(-c(predict), names_to = "actual", values_to = "counts")

(plot3 <- ggplot(df_acc, aes(x = predict, y = actual)) +
    geom_tile(aes(fill = counts)) + 
    geom_text(aes(label = counts)) +
    theme_bw() +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    scale_fill_gradientn(colors = c("#f1faee", "#457b9d")) +
    theme(legend.position = 'none',
          aspect.ratio = 1)
  )

ggplot2::ggsave(filename = "performance.pdf", 
                plot = plot3,
                device = "pdf", 
                path = "D:/Work/April 24/image", 
                width = 5, 
                height = 5, 
                units = "in",
                dpi = 1000,
                limitsize = TRUE,
                bg = "white")
