#this script produces a table that compares the number of clusters from a variety of appraoches  
library(xtable)

# import data
approach1 = rep(5, 10)
approach2 = 1:10
approach3 = rep(3:4, 5)

# amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# labels
labs = c('Emp.', 'Pharm.', 'Child.', 'Health.', 'Groc.', 'Pri. Educ.', 'Sec. Educ.', 'Lib.', 'Parks', 'Transit')

#algo names
approaches = c('Quintiles', 'Min/Max', 'Best Algo')


#producing table
summary_table = data.frame(stringsAsFactors=FALSE)


#convert to string
switche = function(x){
	return(format(round(x, 0), nsmall=0))
}

approach1 = switche(approach1)
approach2 = switche(approach2)
approach3 = switche(approach3)



#adding rows
summary_table = as.character(rbind(summary_table, approach1))
summary_table = rbind(summary_table, approach2)
summary_table = rbind(summary_table, approach3)


#change row and col names
colnames(summary_table) = labs
row.names(summary_table) = approaches



#export 
print(summary_table)
print(xtable(summary_table, type = "latex"), file = "num_clusts_table.txt")







