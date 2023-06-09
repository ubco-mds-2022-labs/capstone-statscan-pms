set.seed(2023)
library(xtable)

# loading data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

# covert populations to numeric from factor
master$PMS_DBPOP = as.numeric(gsub("[^0-9.-]", "", as.character(master$PMS_DBPOP)))

# amenity names
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

# labels
labs = c('Employment', 'Pharmacy', 'Childcare', 'Healthcare', 'Grocery', 'Pri. Educ.', 'Sec. Educ.', 'Library', 'Parks', 'Transit')

# var names
#var_names = c('# of DBs', 'DB Population', 'Median IoR', 'CMA Type', '#1 Province', '#2 Province', '#3 Province', 'Amenity Dense')
var_names = c('# of DBs', 'DB Population', 'Median IoR', 'CMA Type', 'Province', 'Amenity Dense')

#cma type
cmatype_old = c('', 'B', 'D', 'K')
cmatype_new = c('None', 'CMA', 'uCA', 'tCA')

#amenity dense
amendense_old = c('0', '1', '2', 'F')
amendense_new = c('Low', 'Med', 'High', 'Unreliable')

#change words function
switche = function(var, olist, nlist){
  master[,var] = as.character(master[,var])
  for (i in 1:length(olist)){
    master[master[,var] == olist[i], var] = nlist[i]
  }
  return(master[,var])
}

#make more intuitive category names
master[,'PMS_CMATYPE'] = switche('PMS_CMATYPE', cmatype_old, cmatype_new)
master[,'PMS_amenity_dense'] = switche('PMS_amenity_dense', amendense_old, amendense_new)

#print functions
printe = function(count, p_rows){
  perc = paste0("(", format(round(count*100/p_rows, 1), nsmall=1), "%)")
  text = paste(formatC(count, format="d", big.mark=","), perc)
  return(text)
}

# make output file
file.create("profiles_all_tables.tex")
write('
  \\documentclass[10pt, a4paper]{article} \n
  \\usepackage[utf8]{inputenc} \n
  \\usepackage[margin=1in]{geometry} \n
  \\usepackage{rotating} \n
  \\begin{document} \n',
  file = 'profiles_all_tables.tex', append = TRUE
)




#cutoff values list
chdbscan =  list(PMS_prox_idx_emp = 0.22985, PMS_prox_idx_pharma = c(0.01175, 0.0525), PMS_prox_idx_childcare = 0.009, PMS_prox_idx_health = 0.1052,PMS_prox_idx_grocery = c(0.0763, 0.0124), PMS_prox_idx_educpri = c(0.04495,0.22045, 0.1449), PMS_prox_idx_educsec = c(0.0576, 0.0863), PMS_prox_idx_lib = c(0.06575, 0.0691, 0.05465), PMS_prox_idx_parks = 0.01995,PMS_prox_idx_transit = 0.03875)

cquintiles = list(
  PMS_prox_idx_emp = c(0.0004, 0.0030, 0.0127, 0.0368),
  PMS_prox_idx_pharma = c(0.0098, 0.0193, 0.0341, 0.0641),
  PMS_prox_idx_childcare = c(0.0152, 0.0348, 0.0636, 0.1167),
  PMS_prox_idx_health = c(0.0007, 0.0032, 0.0074, 0.0184),
  PMS_prox_idx_grocery = c(0.0221, 0.0348, 0.0555, 0.0985),
  PMS_prox_idx_educpri = c(0.0416, 0.0720, 0.1105, 0.1720),
  PMS_prox_idx_educsec = c(0.0421, 0.0586, 0.0910, 0.1492),
  PMS_prox_idx_lib = c(0.0558, 0.0707, 0.0960, 0.1488),
  PMS_prox_idx_parks = c(0.0203, 0.0372, 0.0614, 0.1050),
  PMS_prox_idx_transit = c(0.0026, 0.0067, 0.0131, 0.0272)
)

cmixall = list(
  PMS_prox_idx_emp = c(0.003649668),
  PMS_prox_idx_pharma = c(0.02654995),
  PMS_prox_idx_childcare = c(0.03634996),
  PMS_prox_idx_health = c(0.002549527),
  PMS_prox_idx_grocery = c(0.04844996),
  PMS_prox_idx_educpri = c(0.08565002),
  PMS_prox_idx_educsec = c(0.06324952, 0.14094998),
  PMS_prox_idx_lib = c(0.09929952),
  PMS_prox_idx_parks = c(0.04474999),
  PMS_prox_idx_transit = c(0.00654981)
)

cpamkmeans = list(
  PMS_prox_idx_emp = c(0.003549657),
  PMS_prox_idx_pharma = c(0.02634995),
  PMS_prox_idx_childcare = c(0.03644997),
  PMS_prox_idx_health = c(0.002249469),
  PMS_prox_idx_grocery = c(0.01129957, 0.01894993, 0.02754996, 0.03894998, 0.05744995, 0.09185003, 0.16739932),
  PMS_prox_idx_educpri = c(0.08265002),
  PMS_prox_idx_educsec = c(0.05569919, 0.09904984, 0.17829930),
  PMS_prox_idx_lib = c(0.09434931),
  PMS_prox_idx_parks = c(0.04504995),
  PMS_prox_idx_transit = c(0.007649841)
) 

cmclust = list(
  PMS_prox_idx_emp = c(0.00004142133, 0.0004477224, 0.001249074, 0.003349638, 0.008549853, 0.02064994, 0.05184998, 0.1629499),
  PMS_prox_idx_pharma = c(0.006417666, 0.010799537, 0.018149931, 0.033198664, 0.055421553, 0.011361244),
  PMS_prox_idx_childcare = c(0.001874842, 0.066850014),
  PMS_prox_idx_health = c(0.0002464102, 0.0034496474, 0.0093498633),
  PMS_prox_idx_grocery = c(0.011689830, 0.007219839),
  PMS_prox_idx_educpri = c(0.02349916, 0.02650828, 0.04439987, 0.09005003, 0.13115005, 0.18500001),
  PMS_prox_idx_educsec = c(0.03474969, 0.03459509, 0.04384998, 0.06179871, 0.10114965, 0.14344998, 0.08546577),
  PMS_prox_idx_lib = c(0.04879960, 0.05379961, 0.06824909, 0.09274833, 0.11634910, 0.04169711),
  PMS_prox_idx_parks = c(0.08254624, 0.01594993, 0.03244996, 0.04634999, 0.06235000, 0.08445002, 0.12194994),
  PMS_prox_idx_transit = c(0.001048913, 0.011649895)
)

cmanual = list(
  PMS_prox_idx_emp = c(0.0000423603, 0.0001573125, 0.0002743458, 0.0005010775),
  PMS_prox_idx_pharma = c(0.01142592, 0.01948168),
  PMS_prox_idx_childcare = c(0.008407409, 0.013850871),
  PMS_prox_idx_health = c(0.0000405736, 0.0001524638, 0.0002658077),
  PMS_prox_idx_grocery = c(0.01208361, 0.01846220),
  PMS_prox_idx_educpri = c(0.04965835, 0.08169011),
  PMS_prox_idx_educsec = c(0.06609424),
  PMS_prox_idx_lib = c(0.6149259),
  PMS_prox_idx_parks = c(0.01834893, 0.02944933),
  PMS_prox_idx_transit = c(0.0000390986, 0.0001481958, 0.0002512994, 0.0003514904)
)
all_cutoffs = list(cquintiles, cmanual, chdbscan, cmixall, cmclust, cpamkmeans)
anames = c('Quintiles', 'Min/Max', 'HDBSCAN', 'MixAll', 'MCLUST', 'PAM k-means')
all_cutoffs = lapply(all_cutoffs, function(x) lapply(x, sort))



counter = 1
for (i in amenities){
  
  #producing table
  summary_table = data.frame()
  
  #remove NA values
  amen = master[!(is.na(master[,i])),]
  
  #assign cluster labels
  n_most = 0
  for (j in 1:length(all_cutoffs)){
  	clusts = findInterval(amen[,i], all_cutoffs[[j]][[i]]) + 1
  	amen[,anames[j]] = clusts
  	
  	#get largest num of clusters
  	if (length(all_cutoffs[[j]][[i]]) > n_most){
  		n_most = length(all_cutoffs[[j]][[i]])
  	}
  }
  n_most = n_most + 1


  
  #overall pop column 
  summary_table[1:8,1] = c(
    printe(nrow(amen), nrow(amen)),
    formatC(median(amen$PMS_DBPOP, na.rm = T), format="d", big.mark=","),
    format(round(median(amen$'IOR_Index_of_remoteness', na.rm = T), 2), nsmall=2),
    paste0(names(sort(table(amen$'PMS_CMATYPE'), decreasing=T)[1]), " (", format(round(unname(sort(table(amen$'PMS_CMATYPE'), decreasing=T)[1])*100/nrow(amen), 1), nsmall=1), "%)"),
    paste0(names(sort(table(amen$PROVINCE), decreasing=T)[1]), " (", format(round(unname(sort(table(amen$PROVINCE), decreasing=T)[1])*100/nrow(amen), 1), nsmall=1), "%)"), 
    #paste0(names(sort(table(amen$PROVINCE), decreasing=T)[2]), " (", format(round(unname(sort(table(amen$PROVINCE), decreasing=T)[1])*100/nrow(amen), 1), nsmall=1), "%)"),
    #paste0(names(sort(table(amen$PROVINCE), decreasing=T)[3]), " (", format(round(unname(sort(table(amen$PROVINCE), decreasing=T)[1])*100/nrow(amen), 1), nsmall=1), "%)"),
    paste0(names(sort(table(amen$'PMS_amenity_dense'), decreasing=T)[1]), " (", format(round(unname(sort(table(amen$'PMS_amenity_dense'), decreasing=T)[1])*100/nrow(amen), 1), nsmall=1), "%)"),
    format(round(median(amen[,i], na.rm = T), 3), nsmall=3),
    '0 - 1' 
  )
  
  rownames = c()
  counterr = 1
  for (j in 1:n_most){
  	for (k in 1:length(all_cutoffs)){ 
	  	if (length(all_cutoffs[[k]][[i]])+1 >= j) {
	  		#rownames update
	  		rownames = c(rownames, paste0(anames[k], " C", j))
	  		
			# num of DBs
			summary_table[1,counterr+1] = printe(nrow(amen[amen[,anames[k]] == j,]), nrow(amen))
			# median poulation
			summary_table[2,counterr+1] = formatC(median(amen[amen[,anames[k]] == j,'PMS_DBPOP'], na.rm = T), format="d", big.mark=",")
			# IoR
			summary_table[3,counterr+1] = format(round(median(amen[amen[,anames[k]] == j,'IOR_Index_of_remoteness'], na.rm = T), 2), nsmall=2)
			#CMA type
			summary_table[4,counterr+1] = paste0(names(sort(table(amen[amen[,anames[k]] == j,'PMS_CMATYPE']), decreasing=T)[1]), " (", format(round(unname(sort(table(amen[amen[,anames[k]] == j,'PMS_CMATYPE']), decreasing=T)[1])*100/nrow(amen[amen[,anames[k]] == j,]), 1), nsmall=1), "%)")
			#province
			summary_table[5,counterr+1] = paste0(names(sort(table(amen[amen[,anames[k]] == j,'PROVINCE']), decreasing=T)[1]), " (", format(round(unname(sort(table(amen[amen[,anames[k]] == j,'PROVINCE']), decreasing=T)[1])*100/nrow(amen[amen[,anames[k]] == j,]), 1), nsmall=1), "%)") 
			#amenity dense
			summary_table[6,counterr+1] = paste0(names(sort(table(amen[amen[,anames[k]] == j,'PMS_amenity_dense']), decreasing=T)[1]), " (", format(round(unname(sort(table(amen[amen[,anames[k]] == j,'PMS_amenity_dense']), decreasing=T)[1])*100/nrow(amen[amen[,anames[k]] == j,]), 1), nsmall=1), "%)")
			#amenity
			summary_table[7,counterr+1] = format(round(median(amen[amen[,anames[k]] == j,i], na.rm = T), 3), nsmall=3)
			#range/cutoffs
			if (j==1){ #first
			  summary_table[8,counterr+1] = paste('0 -', format(round(all_cutoffs[[k]][[i]][j], 4), nsmall=4))
			} else if (j==length(unique(amen[,anames[k]]))) { #last
			  summary_table[8,counterr+1] = paste(format(round(all_cutoffs[[k]][[i]][j-1], 4), nsmall=4), '- 1')
			} else { #middle
			  summary_table[8,counterr+1] = paste(format(round(all_cutoffs[[k]][[i]][j-1], 4), nsmall=4), '-', format(round(all_cutoffs[[k]][[i]][j], 4), nsmall=4))
			}
		  	#print(counterr)
			counterr = counterr + 1
		  }
	  }
  }
  
  #switch columns and rows
  summary_table = t(summary_table)
  
  print(summary_table)

  #change row and col names
  row.names(summary_table) = c('Entire Population', rownames)
  colnames(summary_table) = c(var_names, labs[counter], 'Range')
  
  #write table to output file
  #write(paste0('\\section*{', labs[counter], '} \n\n'), file = 'profiles_all_tables.tex', append = TRUE)
  write(print(xtable(summary_table, type = "latex", caption = labs[counter]), floating = TRUE, floating.environment = "sidewaystable"), file = 'profiles_all_tables.tex', append = TRUE)
  write('\n\n\n\n \\pagebreak \n ', file = 'profiles_all_tables.tex', append = TRUE)
  
  rm(amen)
  rm(summary_table)
  counter = counter + 1
}

write('\n \\end{document}', file = 'profiles_all_tables.tex', append = TRUE)






