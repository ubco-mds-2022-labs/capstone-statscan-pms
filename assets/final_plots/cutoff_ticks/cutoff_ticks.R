set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)
library(RColorBrewer)






#cutoffs
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

# put all in same list
cutoff_list <- list(cquintiles, cmanual, chdbscan, cmixall, cmclust, cpamkmeans )

names(cutoff_list) <- c("Quintiles", "Minima", "HDBSCAN", "MixAll", "MCLUST", "PAM k-means" )


amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

#labels
labs = c('Employment', 'Pharmacy', 'Child care', 'Health care', 'Grocery', 'Primary Education', 'Secondary Education', 'Library', 'Parks', 'Transit')




for (k in 1:length(amenities)){
	# amen number
	ameni = k #pri educ
	# initialize df with first
	df_cutof <- cbind(rep(names(cutoff_list)[1], length(cutoff_list[[1]][[ameni]])), log(cutoff_list[[1]][[amenities[k]]]+0.0001))

	# add
	for (algo in 2:length(cutoff_list)){
	  # add to temporary
	  # add to full
	  df_cutof <- rbind(df_cutof, cbind(rep(names(cutoff_list)[algo], length(cutoff_list[[algo]][[ameni]])), log(cutoff_list[[algo]][[ameni]]+0.0001)))
	}
	colnames(df_cutof) <- c('algo', 'cutoff')
	df_cutof <- as.data.frame(df_cutof)

	#print(df_cutof)

	# make plot
	plt = ggplot(df_cutof,aes(x = as.numeric(as.character(cutoff)),y = algo)) + 
	  #geom_rect(aes(xmin = log(0.0001), xmax = log(as.numeric(cquintiles[[ameni]][1])+0.0001), ymin = -Inf, ymax = Inf), alpha = .02, fill = "#B10026", show.legend = F) + 
	  #geom_rect(aes(xmin = log(as.numeric(cquintiles[[ameni]][2])+0.0001), xmax = log(as.numeric(cquintiles[[ameni]][3])+0.0001),ymin = -Inf, ymax = Inf), alpha = .02, fill = "#B10026",show.legend = F) + 
	  #geom_rect(aes(xmin = log(as.numeric(cquintiles[[ameni]][4])+0.0001), xmax = log(1.0001),ymin = -Inf, ymax = Inf), alpha = .02, fill = "#B10026",show.legend = F)+ 
	  geom_point(shape="|",size=11, col="#1e7898") + labs(x = "Cutoffs", y=' ') + 
	  theme(
	  	#panel.grid.major = element_blank(),
	  	panel.grid.major.x = element_line(size = 0.8),
		panel.grid.minor.x = element_blank(),
		panel.grid.minor = element_blank(),
		panel.grid.major.y = element_blank(),
		axis.ticks.y = element_blank()
	  ) + 
	  scale_y_discrete(labels = rev(c("Quintiles", "Minima", "HDBSCAN", "MixAll", "MCLUST", "PAM k-means")))  +
	  scale_x_continuous(breaks=log(c(0.0002, 0.002, 0.02, seq(0.05,0.15,0.05), 0.25, 0.5)+0.0001), labels=c(0.0002, 0.002, 0.02, seq(0.05,0.15,0.05), 0.25, 0.5), lim=c(min(as.numeric(as.character(df_cutof$cutoff))), max(as.numeric(as.character(df_cutof$cutoff)))))
	  #xlim(-5, 0)
	  
	  
	 
	#export
	ggsave(paste0(labs[k], "_ticks.png"), plt, dpi = 400, width=8, height=4)
}

  
