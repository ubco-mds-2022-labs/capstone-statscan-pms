library(ggplot2)
pmd <- read.csv("../local_data/PMD-en.csv")
# create a melted data frame for the boxplot
df <- reshape2::melt(pmd[, "prox_idx_educpri"])
# replace ".." and "F" with NA 
df$value <- ifelse(df$value %in% c("..", "F"), NA, df$value)
df <- na.omit(df)
df$value <- as.numeric(df$value)

df_log <- df
# log-transformation
df_log$value <- log(df_log$value+0.0001)

ggplot(df, aes(y = value)) +
  geom_boxplot(width = 0.1) +
  theme(panel.background = element_blank(),
    axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  xlim(-0.025,0.025)

ggplot(df_log, aes(y = value)) +
  geom_boxplot() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
        )