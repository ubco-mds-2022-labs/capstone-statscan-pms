set.seed(2023)
library(ggplot2)
library(tidyverse)

library(data.table)
library(gridExtra)


###################
# IMPORT DATA
###################

# pms
load('../../../local_data/codes/create_master/master_pms_df.Rdata')



# num clusters
nquartiles = rep(4, 10)
nmixall = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#nhdbscan = c(2, 2, 3, 2, 0, 2, 2, 0, 2, 0)
nhdbscan = c(2, 2, 2, 2, 2, 2, 3, 2, 2, 3)
num_df = data.frame(nquartiles, nmixall, nhdbscan)



# silhouette
squartiles = c(0.5301638, 0.5352868, 0.5071032, 0.5340226, 0.4664641, 0.5497091, 0.5222450, 0.5211492, 0.5146823, 0.5116242)
smixall = c(0.62416982312449, 0.578246901617834, 0.579170989394048, 0.581603902641992, 0.555919113100562, 0.589022193745387, 0.594242094440351, 0.587765890776524, 0.573329830620795, 0.549231317427407)
#shdbscan = c(0.669838211728378, 0.443449260743095, 
#             0.400812993352801, 0.71150342405053, 
#             NA, 0.491369087890728, 
#             0.502911585364128, NA, 
#             0.343965286339036, NA)
shdbscan = c(0.687337430388787, 0.434971414389449, 
			0.410851184885891, 0.726800535711154, 
			0.405722984464385, 0.532067955884544, 
			0.457023848620274, 0.454816989136992, 
			0.332151764469907, 0.340639023025164)
sil_df = data.frame(squartiles, smixall, shdbscan)


# labels
library(stringr)
amenities = c("PMS_prox_idx_emp", "PMS_prox_idx_pharma", "PMS_prox_idx_childcare", "PMS_prox_idx_health", "PMS_prox_idx_grocery", "PMS_prox_idx_educpri", "PMS_prox_idx_educsec", "PMS_prox_idx_lib", "PMS_prox_idx_parks", "PMS_prox_idx_transit")
labs = str_sub(amenities, 14) 


# cutoffs
cquartiles = list(PMS_prox_idx_emp = c(0.000700000047463944, 0.0062999994950429,  0.0280000130208772), 
                  PMS_prox_idx_pharma = c(0.013100000022532,  0.026199999882501, 0.0529000000575213), 
                  PMS_prox_idx_childcare = c(0.0198000060793181,  0.0481000119893211, 0.0970999914075018), 
                  PMS_prox_idx_health = c(0.00120000004095931,  0.00490000016561574, 0.014100000530935), 
                  PMS_prox_idx_grocery = c(0.0258999997948698,  0.0430999998469438, 0.0850999972059218), 
                  PMS_prox_idx_educpri = c(0.0482999998981212,  0.0897000019827969, 0.153999994119748), 
                  PMS_prox_idx_educsec = c(0.0443000021061397,  0.0728999963809679, 0.12629999661621), 
                  PMS_prox_idx_lib = c(0.0592000016053752,  0.0814000034662971, 0.132099993821641), 
                  PMS_prox_idx_parks = c(0.0244000001259242,  0.047700002381118, 0.0900999976966481), 
                  PMS_prox_idx_transit = c(0.0035999998872406,  0.00959999976535963, 0.0224999990090526))
cmixall = list(
  PMS_prox_idx_emp = c(0.00365),
  PMS_prox_idx_pharma = c(0.02675),
  PMS_prox_idx_childcare = c(0.03565),
  PMS_prox_idx_health = c(0.00245),
  PMS_prox_idx_grocery = c(0.05225),
  PMS_prox_idx_educpri = c(0.08515),
  PMS_prox_idx_educsec = c(0.08125),
  PMS_prox_idx_lib = c(0.1002),
  PMS_prox_idx_parks = c(0.04425),
  PMS_prox_idx_transit = c(0.00595))
#chdbscan = list(PMS_prox_idx_emp = 0.16035, PMS_prox_idx_pharma = 0.01155,
#                PMS_prox_idx_childcare = c(0.01205, 0.101), PMS_prox_idx_health = 0.06835,
#                NA, PMS_prox_idx_educpri = 0.04895, PMS_prox_idx_educsec = 0.0655,
#                NA, PMS_prox_idx_parks = 0.01935, NA)
chdbscan = list(PMS_prox_idx_emp = 0.21475, PMS_prox_idx_pharma = 0.0114, PMS_prox_idx_childcare = 0.0087, PMS_prox_idx_health = 0.09555, PMS_prox_idx_grocery = 0.1146, PMS_prox_idx_educpri = 0.05295, PMS_prox_idx_educsec = c(0.0625, 0.096), PMS_prox_idx_lib = 0.0577, PMS_prox_idx_parks = 0.01955, PMS_prox_idx_transit = c(0.0039, 0.0355))


g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

###################
# NUM OF CLUSTERS PLOT
###################

data = pivot_longer(num_df, cols = names(num_df))
data[data$value==0, 'value'] = 1
data$amenities = rep(amenities, each=3)
data$amenities = factor(data$amenities, levels = rev(amenities), ordered = TRUE)
numclusts = ggplot(data, aes(x=amenities, y=value, color=name)) + 
  geom_jitter(width=0.1, height=0, size=5) + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
  ) + 
  ylab('Number of Clusters') + xlab('') +
  scale_colour_discrete(name = "Algorithm",
                        labels = c("Quartiles", "MixAll", "HDBSCAN"),
                        breaks = c("nquartiles", "nmixall", "nhdbscan"))

#bar version
t = list()
t[[1]] = ggplot(data[1:15,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') + 
  scale_x_discrete(labels=labs[5:1]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm")
  ) + guides(fill = 'none') +
  ylab('Number of Clusters') + xlab('') + 
   coord_flip(ylim=c(1,4)) 
t[[2]] = ggplot(data[16:30,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') + 
  scale_x_discrete(labels=labs[10:6]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0.2,0,0),"cm"),
  ) + guides(fill = 'none') +
ylab('Number of Clusters') + xlab('') + 
  coord_flip(ylim=c(1,4))
temp = ggplot(data, aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
    legend.direction = "horizontal"  
    ) +  coord_flip() +
ylab('Number of Clusters') + xlab('') +
  scale_fill_discrete(name = "Algorithm   ",
                      labels = c("Quartiles", "MixAll", "HDBSCAN"),
                      breaks = c("nquartiles", "nmixall", "nhdbscan"))
t[[3]] = g_legend(temp)

layout_mat = rbind(c(1,2),
                    c(1,2),
                    c(1,2),
                    c(1,2),
                    c(3,3))
numclusts = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))








###################
# SILHOUETTE COEFFICIENT PLOT
###################

data = pivot_longer(sil_df, cols = names(sil_df))
data[is.na(data$value), 'value'] = 0.02
data$amenities = rep(amenities, each=3)
data$amenities = factor(data$amenities, levels = rev(amenities), ordered = TRUE)
silcoef = ggplot(data, aes(x=amenities, y=value, color=name)) + 
  geom_jitter(width=0, height=0, size=5) + 
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
  ) + 
  ylab('Silhouette Coefficient') + xlab('') +
  scale_colour_discrete(name = "Algorithm",
                        labels = c("Quartiles", "MixAll", "HDBSCAN"),
                        breaks = c("squartiles", "smixall", "shdbscan"))

#bar version
t = list()
t[[1]] = ggplot(data[1:15,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') +
  scale_x_discrete(labels=labs[5:1]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
  ) + coord_flip() + guides(fill = 'none') + 
  ylab('Silhouette Coefficient') + xlab('') 
t[[2]] = ggplot(data[16:30,], aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') +
  scale_x_discrete(labels=labs[10:6]) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0.2,0,0),"cm"),
  ) + coord_flip() + guides(fill = 'none') + 
  ylab('Silhouette Coefficient') + xlab('') 
temp = ggplot(data, aes(x=amenities, y=value, fill=name)) + 
  geom_bar(stat='identity', position='dodge') +
  scale_x_discrete(labels=labs) + 
  theme(
    axis.text.y = element_text(size=10), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0,0,0),"cm"),
    legend.direction = "horizontal",
    legend.position="bottom"
  ) + coord_flip() +
  ylab('Silhouette Coefficient') + xlab('') +
  scale_fill_discrete(name = "Algorithm    ",
                      labels = c("Quartiles", "MixAll", "HDBSCAN"),
                      breaks = c("squartiles", "smixall", "shdbscan"))
t[[3]] = g_legend(temp)
rm(temp)
layout_mat = rbind(c(1,2),
                    c(1,2),
                    c(1,2),
                    c(1,2),
                    c(3,3))
silcoef = do.call(grid.arrange,list(grobs=t, layout_matrix=layout_mat))










###################
# DISTRIBUTION CUTOFF PLOT
###################

#log transform
for (i in amenities){
  master[,i] = log(master[,i]+0.0001)
}

#hdbscan
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = chdbscan[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
      ) +
    ggtitle(str_sub(i, 14))
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
}
most = names(lapply(chdbscan, length)[lapply(chdbscan, length) == max(unlist(lapply(chdbscan, length)))][1])
p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))


#mixall
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cmixall[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(str_sub(i, 14))
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
}
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
most = names(lapply(chdbscan, length)[lapply(chdbscan, length) == max(unlist(lapply(chdbscan, length)))][1])
p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs2 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))





#quartiles
p <- list()
most = 0
for(i in amenities){
  temp = na.omit(master[,i])
  dt <- data.table(x=1:length(temp),y=temp)
  dens <- density(dt$y)
  df <- data.frame(x=dens$x, y=dens$y)
  cutoff = cquartiles[[i]] #change this for different algorithms!
  logged <- log(cutoff+0.0001)
  df$Cluster <- as.factor(as.numeric(cut(df$x, c(min(df$x), logged, max(df$x)),  include.lowest=T)))
  plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
    #scale_x_continuous(breaks=round(logged, 2)) + 
    ylab('') + guides(fill = 'none') + 
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1, size=8), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin=unit(c(0,0,0.1,0),"cm"),
      plot.title = element_text(size = 10),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(), #removes x axis labels
      axis.ticks.x = element_blank() #removes x axis ticks
    ) +
    ggtitle(str_sub(i, 14))
  if (is.null(cutoff[1])){
    plt = plt + xlab("NO CLUSTERS DETECTED") + theme(axis.title.x = element_text(size = 7, color='gray'))
  } else {
    plt = plt + xlab(paste(round(cutoff, 3), collapse = ', ')) + theme(axis.title.x = element_text(size = 7, color='gray'))
  }
  p[[i]] = plt
  if (length(cutoff) > most){
    most = length(cutoff)
    most_plt = ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=Cluster)) + 
      scale_x_continuous(breaks=round(logged, 2)) + 
      guides(fill = guide_legend(title.position = "top", label.hjust = 0.5)) +
      theme(
        legend.direction = "horizontal"
      ) 
  }
}

p[[11]] = g_legend(most_plt)

layout_mat <- rbind(c(1:4),
                    c(5:8),
                    c(9:11, 11))
cutoffs3 = do.call(grid.arrange,list(grobs=p, layout_matrix=layout_mat))





###################
# SAVING PLOTS
###################

ggsave("numclusts.png", numclusts, dpi = 400, width=8, height=5)
ggsave("silcoef.png", silcoef, dpi = 400, width=8, height=5)
ggsave("cutoffs_hdbscan.png", cutoffs, dpi = 400, width=8, height=5)
ggsave("cutoffs_mixall.png", cutoffs2, dpi = 400, width=8, height=5)
ggsave("cutoffs_quartiles.png", cutoffs3, dpi = 400, width=8, height=5)


