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



#pass cutoffs 
cut_offs_mclust = list(
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
cut_offs_kmeans_pam = list(
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
cut_offs_mixall = list(
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
all_cutoffs = list(cut_offs_mclust, cut_offs_kmeans_pam, cut_offs_mixall)

approaches = c('MCLUST', 'PAM k-means', 'MixAll')

nice_colors = rev(brewer.pal(9, "YlGnBu"))



# produce 1 table per amenity
counter = 1
for (i in amenities){
	#plot list
	t = list()

	#remove NA values
	amen = master[!(is.na(master[,i])),]
 	
 	for (j in 1:length(approaches)){
 		cutoffs = all_cutoffs[[j]]
 		
		clusts = rep(length(cutoffs[[i]]) + 1, nrow(amen))
		for (k in 1:nrow(amen)){
			for (p in 1:length(cutoffs[[i]])) {
				if (amen[k,i] < cutoffs[[i]][p]) {  
					clusts[k] = p
					break
				}
			}
		}
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

	
	#make plot
	t[[1]] = ggplot(df_long_count, aes(y = variable, x = percent, fill = as.factor(value))) +
		geom_col(position = "fill") +
		scale_fill_manual(values = nice_colors) + 
		xlab('') + ylab('') + guides(fill = 'none') + ggtitle('# of DBs') + 
    theme( 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    )
	t[[2]] = ggplot(df_long_pop, aes(y = variable, x = percentage, fill = as.factor(value))) +
		geom_col(position = "fill") +
		scale_fill_manual(values = nice_colors) + 
		xlab('') + ylab('') + guides(fill = 'none') + ggtitle('Population') + 
    theme( 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    )
	legend_plt = ggplot(df_long_pop, aes(y = variable, x = percentage, fill = as.factor(value))) +
		geom_col(position = "fill") + labs(fill="Cluster") +
		scale_fill_manual(values = nice_colors) + 
	  guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) + 
		theme(
        legend.direction = "horizontal"
		) 
	
	#arrange in grid
	t[[3]] = g_legend(legend_plt)

	layout_mat = rbind(c(1),
                    c(1),
                    c(2),
                    c(2),
                    c(3) )
	barp = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat,top=labs[counter]))
	
	#export
	ggsave(paste0(labs[counter], "barplot.png"), barp, dpi = 400, width=8, height=5)
  counter = counter + 1
}










