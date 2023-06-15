set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)


# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

#amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

#labels
labs = c('Employment', 'Pharmacy', 'Child care', 'Health care', 'Grocery', 'Primary Education', 'Secondary Education', 'Library', 'Parks', 'Transit')


#comparison plot
t = list()








#log transformed
#log transform
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}

counter = 1
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=TRUE)) + 
  scale_fill_manual(values="#B10026") + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0.1,0.1,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank(), #removes x axis ticks
      plot.background = element_rect(fill = "#f3f5f600", colour = "#f3f5f600"),
      panel.background = element_rect(fill = "#c0c0c000", colour = "#c0c0c000"),
      ) +
    ggtitle(labs[counter]) + xlab('') + 
    xlim(-4, 0)
  p[[i]] = plt
  counter = counter+1
}
t[[1]] = p[[6]] + ggtitle('Primary Education Full Data')

layout_mat = rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
distr2 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))




#subsample
set.seed(2023)
perc = 3 #percentage of data to subsample
subsample = (nrow(master)/100)*perc
master = master[sample(nrow(master), subsample),]


temp = na.omit(master[,amenities[6]])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=TRUE)) + 
  scale_fill_manual(values="#B10026") + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0.1,0.1,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank(), #removes x axis ticks
      plot.background = element_rect(fill = "#f3f5f600", colour = "#f3f5f600"),
      panel.background = element_rect(fill = "#c0c0c000", colour = "#c0c0c000"),
      ) +
     xlab('') + xlim(-4, 0)

t[[2]] = plt + ggtitle('Primary Education 3% Subsample')





#comparison plot
layout_mat = rbind(c(1),
					c(2))
compare = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))




#export 
#ggsave("distributions.png", distr, dpi = 400, width=8, height=5)
#ggsave("log_distributions.png", distr2, dpi = 400, width=8, height=5)
ggsave("compare_distributions.png", compare, dpi = 400, width=4, height=5)
