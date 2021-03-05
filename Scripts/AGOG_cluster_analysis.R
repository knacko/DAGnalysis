### Start up stuff #############################################################################
setwd("C:\\Users\\geekb\\Documents\\School\\Internships\\CBDRH\\Data\\Data from CBDRH\\Shortcuts")
list.files(getwd())
getwd()

library(magrittr)
library(openxlsx)
library(qdapTools)
library(dplyr)
library(cluster) 
library(factoextra)
library(NbClust)
library(fpc)
library(ggplot2)
library(reshape2)
library(purrr)
library(dplyr)
library(dendextend)
library(stringr)
library(Rtsne)
library(ggpubr)
library(tidyr)
library(ggdag)

df <- read.xlsx("AGOG_caffeine_n0.xlsx", colNames=TRUE) # clusters: 5
df %<>% select(cec_upn,cola,coffee_inst,coffee,tea,energy) 
#df %<>% select(cec_upn,cola_l,coffee_inst_l,coffee_l,tea_l,energy_l)


df <- read.xlsx("AGOG_medical_conditions2_n0.xlsx", colNames=TRUE)
#df <- read.xlsx("AGOG_mobile_n0.xlsx", colNames=TRUE)
df <- read.xlsx("AGOG_hormonal_n0.xlsx", colNames=TRUE) # clusters: 3
df <- df[df$Female == 0,]

df <- df[rowSums(df == ".x")==0, , drop = FALSE]

# cols <- colnames(df)
# for(id in 2:ncol(df)) {
#   df[,id] <- paste(cols[id],"_",df[,id],sep="")
# }
# 
# df[,2:ncol(df)] %<>% mutate_if(is.character, as.factor)

df[,2:ncol(df)] %<>% mutate_if(is.character, as.numeric)
#df[,2:ncol(df)] %<>% scale()
 
#------------ DISSIMILARITY MATRIX ------------#
gower_dist <- daisy(df[ ,2:ncol(df)], metric = c("gower"))
summary(gower_dist)

max_clusters <- 20
optimal_clusters <- 4 

#------------ VERIFY SIMILARITY -------------#
gower_mat <- as.matrix(gower_dist)
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ] # CLOSEST
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ] # FURTHEST

#--------------- PAM clustering ---------------#
sil_width <- c(NA)
for(i in 2:max_clusters){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:max_clusters, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:max_clusters, sil_width)


pam_fit <- pam(gower_dist, diss = TRUE, k = optimal_clusters)

pam_results <- df %>%
  dplyr::select(-cec_upn) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
df[pam_fit$medoids,]

pam_fit <- pam(gower_dist, diss = TRUE, k = optimal_clusters)
gen_tsne(pam_fit,paste("PAM (clusters =",optimal_clusters,")"))

#------------- FANNY CLUSTERING -------------#
sil_width <- c(NA)
for(i in 2:max_clusters){  
  fanny_fit <- fanny(gower_dist, diss = TRUE, k = i, memb.exp = memb_exp, maxit = 5000)
  sil_width[i] <- fanny_fit$silinfo$avg.width  
}
plot(1:max_clusters, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:max_clusters, sil_width)

memb_exp <- 1.05
fanny_fit <- fanny(gower_dist, diss = TRUE, k = optimal_clusters, memb.exp = memb_exp, maxit = 5000)
gen_tsne(fanny_fit,paste("Fanny (clusters =",optimal_clusters,"; memb.exp =",memb_exp, ")"))

fanny_results <- df %>%
  dplyr::select(-cec_upn) %>%
  mutate(cluster = fanny_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
fanny_results$the_summary
df[fanny_fit$medoids,]

df_wide <- mutate(df,cluster = fanny_fit$clustering) %>%group_by(cluster)
df_long <- gather(df, source, value, select(df_wide,-cec_upn,-cluster))

ggplot(df_long,aes(x=source,y=value,fill=cluster)) + 
  geom_violin() + 
  facet_wrap(~cluster) +
  theme(axis.text.x = element_text(angle = 45))

df_long$value <- as.double(df_long$value)

gen_tsne <- function (df_fit,title) {
  
  a_perplex <- c(2,5,10,30,50,100)
  
  plots <- list(NA)
  for (id in 1:6) {
  
    tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, max_iter = 5000, perplexity = a_perplex[id], eta = 10)
    
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(df_fit$clustering),
             name = df$cec_upn)
    
    plots[[id]] <- ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster, size = 3, alpha = 0.2)) + 
      ggtitle(paste("Perplexity =",a_perplex[id])) +
      theme(legend.position = "none")
    
  }

  groupsize <- toString(table(tsne_data$cluster))
  
  x11()
  annotate_figure(ggarrange(plotlist = plots), top="\n",fig.lab=paste(title,"\nGroup sizes:",groupsize),fig.lab.face = "bold")

}




#------------ DIVISIVE CLUSTERING ------------#
 divisive_clust <- diana(as.matrix(gower_dist), diss = TRUE, keep.diss = TRUE)
# ##plot(divisive.clust, main = "Divisive")
# 
 stats_df_divisive <- cstats.table(gower_dist, divisive_clust, max_clusters)
 stats_df_divisive

#------------ AGGLOMERATIVE CLUSTERING ------------#
aggl_clust_c <- hclust(gower_dist, method = "complete")
##plot(aggl.clust.c, main = "Agglomerative, complete linkages")

stats_df_aggl <- cstats.table(gower_dist, aggl_clust_c, max_clusters) #complete linkages looks like the most balanced approach
stats_df_aggl

#------------ CLUSTER SIZE DETERMINTATION ------------#

##NbClust(df[,2:6], diss=gower.dist, index="silhouette", method = "complete")

ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, max.clusters))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, max.clusters))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

# DENDROGRAM #
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = optimal.clusters, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

# HEATMAP #


clust.num <- cutree(aggl.clust.c, k = optimal.clusters)
df.cl <- cbind(df, clust.num)

df.long <- melt(data.frame(lapply(df.cl, as.character), stringsAsFactors=FALSE), 
                  id = c("cec_upn", "clust.num"), factorsAsStrings=T)

df.long.q <- df.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(cec_upn)) %>%
  distinct(clust.num, variable, value, count)

df.long.p <- df.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num)

cols <- colnames(df)
lbl = NA
for(id in 2:ncol(df)) {
  lbl %<>% c(paste(levels(df[,id]),sep=""))
}
lbl %<>% na.exclude()

heatmap.p <- ggplot(df.long.p, aes(x = clust.num, y = factor(value, levels = c(lbl, ordered = T)))) +
  
  geom_tile(aes(fill = perc), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")

heatmap.p


#------------ Cstats.table ------------#
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}