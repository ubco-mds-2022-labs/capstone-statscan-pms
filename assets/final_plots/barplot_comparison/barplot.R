#plotting horizontal barplots for the number of people and number of DBs classified in each cluster (remote, not remote, etc.)

set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)
library(RColorBrewer)



# load data
load('../../../../local_data/codes/create_master/master_pms_df.Rdata')

# covert populations to numeric from factor
master$PMS_DBPOP = as.numeric(gsub("[^0-9.-]", "", as.character(master$PMS_DBPOP)))

#amenities
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")

#labels
labs = c('Employment', 'Pharmacy', 'Child care', 'Health care', 'Grocery', 'Primary Education', 'Secondary Education', 'Library', 'Parks', 'Transit')

#g legend function
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}




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
all_cutoffs = list(cquintiles, cmanual, chdbscan, cmixall, cmclust, cpamkmeans)
all_cutoffs = lapply(all_cutoffs, function(x) lapply(x,sort))

approaches = c('Quintiles', 'Minima', 'HDBSCAN', 'MixAll', 'MCLUST', 'PAM k-means')



# produce 1 table per amenity
counter = 1
for (i in amenities){
	#plot list
	t = list()

	#remove NA values
	amen = master[!(is.na(master[,i])),]
 	
 	for (j in 1:length(approaches)){
 		cutoffs = all_cutoffs[[j]]
 		
 		#assign cluster numbers
		clusts = findInterval(amen[,i], cutoffs[[i]]) + 1
	  amen[,approaches[j]] = clusts
 	}
 	
 	#total population
 	population = sum(amen$PMS_DBPOP, na.rm=T)
 	
 	#create plotting dataframe
	df_long_count = amen[,approaches] %>% 
		pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>% 
		group_by(variable, value) %>% 
		summarize(count = n()) %>% 
		mutate(percent = count / sum(count))
	df_long_pop = tibble()
	for (approach in approaches){
	  summed = amen %>%
	    group_by(!!!syms(approach)) %>%
	    summarize(total = sum(PMS_DBPOP, na.rm=T))
	  percentages = summed %>%
	    mutate(percentage = total / sum(total))
	  percentages$variable = approach
	  names(percentages)[1] = 'value'
	  df_long_pop = bind_rows(percentages, df_long_pop)
	}

  
  #add number of clusters to var names
  per_count = c()
  for (p in rev(approaches)){
  	per_count = c(per_count, length(which(df_long_count$variable==p)))
  }
  #df_long_count$variable = paste0(df_long_count$variable, " (", rep(per, per), ")")
  
  per_pop = c()
  for (p in rev(approaches)){
  	per_pop = c(per_pop, length(which(df_long_pop$variable==p)))
  }
  #df_long_pop$variable = paste0(df_long_pop$variable, " (", rep(per, per), ")")
  
  
  
  #reassign factor levels for approaches
  df_long_count$variable = factor(df_long_count$variable, levels=rev(approaches))
  df_long_pop$variable = factor(df_long_pop$variable, levels=rev(approaches))
  
	
  #re-assign factor numbers for coloring purposes
  max_n = max(df_long_count$value)
  for (approach in approaches){
    len = max(df_long_count$value[df_long_count$variable == approach])
    df_long_count[df_long_count$variable == approach,'value'] = round(seq(1, max_n, length.out = len))
    df_long_pop[df_long_pop$variable == approach,'value'] = round(seq(1, max_n, length.out = len))
  }


  #print(head(df_long_count))
  #print(df_long_pop)

  
  
	#make plot
	t[[1]] = ggplot(df_long_count, aes(y = variable, x = percent, fill = as.factor(value))) +
		geom_col(position = position_fill(reverse = TRUE)) +
		scale_fill_manual(values = brewer.pal(max_n, "YlGnBu")) + 
		xlab('') + ylab('') + guides(fill = 'none') + ggtitle('# of DBs') + 
    theme( 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      #axis.ticks.x = element_blank() #removes x axis ticks
    ) + 
    scale_y_discrete(labels = paste0(rev(approaches), " (", per_count, ")"))
	t[[2]] = ggplot(df_long_pop, aes(y = variable, x = percentage, fill = as.factor(value))) +
		geom_col(position = position_fill(reverse = TRUE)) +
		scale_fill_manual(values = brewer.pal(max_n, "YlGnBu")) + 
		xlab('Proportion') + ylab('') + guides(fill = 'none') + ggtitle('Population') + 
    theme( 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      #axis.text.x = element_blank(), #removes x axis labels
      #axis.ticks.x = element_blank() #removes x axis ticks
    ) + 
    scale_y_discrete(labels = paste0(rev(approaches), " (", per_pop, ")"))
	legend_plt = ggplot(df_long_pop, aes(y = variable, x = percentage, fill = as.factor(value))) +
		geom_col(position = "fill") + labs(fill=" ") +
		scale_fill_manual(values = brewer.pal(max_n, "YlGnBu"), labels=c('Least Proximal', as.character(2:(max_n-1)), 'Most Proximal')) + 
	  guides(fill = guide_legend(title.position = "top", label.hjust = 0.5, nrow=1)) + 
		theme(
        legend.direction = "horizontal",
        legend.position = "bottom"
		) 
	
	#arrange in grid
	t[[3]] = g_legend(legend_plt)

	layout_mat = rbind(c(1),
                    c(1),
                    c(1),
                    #c(1),
                    c(2),
                    c(2),
                    c(2),
                    c(2),
                    #c(2),
                    #c(3),
                    c(3) )
	barp = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat,top=labs[counter]))
	
	#export
	ggsave(paste0(labs[counter], "_barplot.png"), barp, dpi = 400, width=8, height=5)
  counter = counter + 1
}










