set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)
library(RColorBrewer)


















###########################################
# avi code
###########################################

library(tidyr)
results_df_2 <- results_df[2:8, ]
# convert data to long format
results_df_long <- pivot_longer(results_df_2, 
                                cols = c(sil_coef, davies_bouldin, calinski_harabasz, dunn),
                                names_to = "Coefficient", 
                                values_to = "Value")


# rename the coefficient names
results_df_long <- results_df_long %>% 
  mutate(Coefficient = case_when(
    Coefficient == "sil_coef" ~ "Silhouette Coefficient",
    Coefficient == "davies_bouldin" ~ "Davies Bouldin",
    Coefficient == "calinski_harabasz" ~ "Calinski Harabasz",
    Coefficient == "dunn" ~ "Dunn Index",
    TRUE ~ Coefficient
  ))

# Plot coefficients for each number of clusters
plt = ggplot(results_df_long, aes(x = num_clusters, y = Value, color = Coefficient)) + 
  geom_line(size = 1) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  xlab("Number of Clusters") + 
  ylab("Coefficient Value") + 
  ggtitle("Coefficients vs Number of Clusters") +
  facet_wrap(~Coefficient, scales = "free_y") + 
  guides(color = FALSE) + 
  theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank(), #removes x axis ticks
      plot.background = element_rect(fill = "#f3f5f600", colour = "#f3f5f600"),
      panel.background = element_rect(fill = "#f3f5f600", colour = "#f3f5f600")
    )
  
  
  
  
  
  
#export
ggsave("valid_compare.png", plt, dpi = 400, width=8, height=5)
