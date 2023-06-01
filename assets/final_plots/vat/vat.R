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



#need to subsample data
perc = 1 #percentage of data to subsample
subsample = (nrow(master)/100)*perc
master = master[sample(nrow(master), subsample),]


#comparison plot
t = list()


library(factoextra)

#not transformed
counter = 1
for (i in amenities){
	# visual assessment of cluster tendency (VAT)
	plt = fviz_dist(dist(as.matrix(na.omit(master[, amenities[1]]))), show_labels = FALSE) +
  labs(title = labs[counter])
  
  #export 
  ggsave(paste0(labs[counter], "_vat.png"), plt, dpi = 400, width=8, height=5)
  counter = counter + 1
}
  
t[[1]] = plt
  
  
  
#log transformed

for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}

counter = 1
for (i in amenities){
	# visual assessment of cluster tendency (VAT)
	plt = fviz_dist(dist(as.matrix(na.omit(master[, amenities[1]]))), show_labels = FALSE) +
  labs(title = labs[counter])
  
  #export 
  ggsave(paste0(labs[counter], "_vat_log.png"), plt, dpi = 400, width=8, height=5)
  counter = counter + 1
}
  
t[[2]] = plt




#comparison plot
layout_mat = rbind(c(1:2))
compare = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))



#export comparison plot
  ggsave("compare_vat.png", compare, dpi = 400, width=8, height=5)



  

