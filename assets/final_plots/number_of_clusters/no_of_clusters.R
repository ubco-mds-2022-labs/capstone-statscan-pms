
results_df <- read.csv("assets/final_plots/number_of_clusters/res_df.csv")

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
p <- ggplot(results_df_long, aes(x = num_clusters, y = Value, color = Coefficient)) + 
  geom_line(size = 0.7) + 
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  xlab("Number of Clusters") + 
  ylab("Coefficient Value") + 
  # ggtitle("Coefficients vs Number of Clusters") +
  facet_wrap(~Coefficient, scales = "free_y") + 
  theme_minimal() +
  guides(color = FALSE) +
  theme(axis.title = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5, linetype = "solid"),
        axis.ticks.x = element_line(color="black", size = 0.5),
        
        
    panel.grid = element_blank())
  
ggsave('assets/noc_update.png', p, bg='transparent')