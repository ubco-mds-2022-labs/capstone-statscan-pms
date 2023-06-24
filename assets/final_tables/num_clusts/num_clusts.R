#this script produces a table that compares the number of clusters from a variety of appraoches  
library(xtable)

# import data
nquintiles = rep(5, 10)
nmanual = c(5, 3, 3, 4, 3, 3, 2, 2, 3, 4)
nhdbscan = c(2, 3, 2, 2, 3, 4, 3, 4, 2, 2)
nmixall = c(2, 2, 2, 2, 2, 2, 3, 2, 2, 2)
nmclust = c(9, 7, 3, 4, 3, 7, 8, 7, 8, 3)
npamkmeans = c(2, 2, 2, 2, 8, 2, 4, 2, 2, 2)
all_nums = list(nquintiles, nmanual, nhdbscan, nmixall, nmclust, npamkmeans)

# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# labels
labs = c('Emp.', 'Pharm.', 'Child.', 'Health.', 'Groc.', 'Pri. Educ.', 'Sec. Educ.', 'Lib.', 'Parks', 'Transit')

#algo names
approaches = c('Quintiles', 'Min/Max', 'HDBSCAN', 'MixAll', 'MCLUST', 'PAM k-means')

#convert to string
switche = function(x){
	return(format(round(x, 0), nsmall=0))
}


all_nums = lapply(all_nums, switche)




#adding rows
summary_table = all_nums[[1]]
for (i in 2:length(all_nums)){
	summary_table = rbind(summary_table, all_nums[[i]])
}



#change row and col names
colnames(summary_table) = labs
row.names(summary_table) = approaches



#export 
print(summary_table)
print(xtable(summary_table, type = "latex"), file = "num_clusts_table.txt")







