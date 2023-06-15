set.seed(2023)
library(ggplot2)
library(tidyverse)
library(dplyr) 


# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

#amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

#labels
labs = c('Employment', 'Pharmacy', 'Child\n care', 'Health\n care', 'Grocery', 'Primary\n Education', 'Secondary\n Education', 'Library', 'Parks', 'Transit')



#boxplot
df <- na.omit(reshape2::melt(master[,amenities]))

# Create the boxplot
bp = ggplot(df, aes(x = variable, y = value, fill=TRUE)) +
  geom_boxplot() +
  scale_fill_manual(values="#B10026") + 
  xlab("") + ylab("") + guides(fill = 'none') + 
  #ggtitle("Boxplots of proximity indices") +
  scale_x_discrete(labels = labs) + 
  theme(
  	  panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0.1,0.1,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), #removes x axis ticks
      plot.background = element_rect(fill = "#f3f5f600", colour = "#f3f5f600"),
      panel.background = element_rect(fill = "#c0c0c000", colour = "#c0c0c000"),
      )
  
  
  
  
  
#log transformed boxplot
#log
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}

df <- na.omit(reshape2::melt(master[,amenities]))

# Create the boxplot
bp_log = ggplot(df, aes(x = variable, y = value, fill=TRUE)) +
  geom_boxplot() +
  scale_fill_manual(values="#B10026") + 
  xlab("") + ylab("") + guides(fill = 'none') + 
  #ggtitle("Boxplots of log-transformed proximity indices") +
  scale_x_discrete(labels = labs) + 
  theme(
  	  panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0.1,0.1,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), #removes x axis ticks
      plot.background = element_rect(fill = "#f3f5f600", colour = "#f3f5f600"),
      panel.background = element_rect(fill = "#c0c0c000", colour = "#c0c0c000"),
      )
  
  
  #export 
ggsave("boxplot.png", bp, dpi = 400, width=8, height=5)
ggsave("logged_boxplot.png", bp_log, dpi = 400, width=8, height=5)



