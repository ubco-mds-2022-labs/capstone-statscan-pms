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


#not log transformed
counter = 1
p <- list()
most = 0
for(i in amenities){
  srt = sort(na.omit(master[,i]))
  df = data.frame(
  	index = 1:length(srt),
  	value = srt
  )	
  plt = ggplot(df, aes(x=index, y=value)) + geom_line(aes(col=TRUE), linewidth=1.2) + 
  scale_fill_manual(values="#B10026") + 
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
      axis.ticks.x = element_blank() #removes x axis ticks
      ) +
    ggtitle(labs[counter]) + xlab('') + guides(col = 'none') 
  p[[i]] = plt
  counter = counter+1
}
t[[1]] = p[[6]]



layout_mat = rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
sort1 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))




#log transformed
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}
counter = 1
p <- list()
most = 0
for(i in amenities){
  srt = sort(na.omit(master[,i]))
  df = data.frame(
  	index = 1:length(srt),
  	value = srt
  )	
  plt = ggplot(df, aes(x=index, y=value)) + geom_line(aes(col=TRUE), linewidth=1.2) +
  scale_fill_manual(values="#B10026") +  
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
      axis.ticks.x = element_blank() #removes x axis ticks
      ) +
    ggtitle(labs[counter]) + xlab('') + guides(col = 'none') 
  p[[i]] = plt
  counter = counter+1
}
t[[2]] = p[[6]] + ggtitle('Primary Education Log-transformed')


layout_mat = rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
sort2 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))





#comparison plot
layout_mat = rbind(c(1:2))
compare = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))







#export 
ggsave("sort_plot.png", sort1, dpi = 400, width=8, height=5)
ggsave("log_sort_plot.png", sort2, dpi = 400, width=8, height=5)
ggsave("sort_comparison.png", compare, dpi = 400, width=8, height=3)

