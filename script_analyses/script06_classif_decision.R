#######################################################################################
#######################################################################################
# Script 6 - Make classification decision
#######################################################################################
#######################################################################################

library(cluster)
library(dendextend)

## set and load data
numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

load("outputs/df_rf_a")
load("outputs/diff_surface_list")
load("outputs/matrice_curves_list")
load("outputs/matrice_proba_curves_list")
load("outputs/clust_list")

varenviroxproba1=as.factor(c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                             "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)"))

replicats <- 200

## plot inertie according to number of groups
pdf(file="figures/figS4.pdf", height = 12, width = 6)
par(mfrow=c(3,2))
for (j in 1:6){
  clust_coeff <- clust_list[[j]]
  inertie <- sort(clust_coeff$height, decreasing = TRUE)
  plot(inertie, pch=16, main=varenviroxproba1[j], xlim=c(0,10), ylab='')
}
dev.off()

## set number of classes/groups per probability x covariable enviro
nb_classe=c(4,4,4,3,3,3) 

## create (& plot) dendrograms
dend_list=list()
for (j in 1:6){
  clust_coeff=clust_list[[j]]
  dend <- as.dendrogram(clust_coeff)
  dend <- dend %>% set("branches_k_color", k = nb_classe[j] ) %>%
    set("branches_lwd", value= 2) %>%
    set("labels_cex", 0.01)
  labels(dend) <- df_rf_a$individual[clust_coeff$order] 
  dend_list[[j]]=dend
  #plot(dend, main = varenviroxproba1[j])
}

## match each sample to its group/class
above=c(4.8,1.9,1,1.5,1.4,1.9)
below=c(8,6,3,3,3.9,5) 

## visualise on dendrogram plot
for (j in 1:6){
  plot(dend_list[[j]], main = varenviroxproba1[j])
  abline(h = above[j], lty = 2, col = "red")
  abline(h = below[j], lty = 2, col = "red")
}

nb_gp_list=list()
for (j in 1:6){
  dend=dend_list[[j]]
  (nb_gp=dend %>% get_nodes_attr("members", 
                                 id = which(dend %>% get_nodes_attr("height") > above[j] &dend %>% get_nodes_attr("height")< below[j])))
  
  if (j==1) {nb_gp=c(nb_gp[1],18*replicats-sum(nb_gp[1:3]),nb_gp[2],nb_gp[3])}
  if (j==2) {nb_gp=c(nb_gp[1],nb_gp[3:5])}
  if (j==3) {nb_gp=c(nb_gp[1:2],nb_gp[4:5])}
  if (j==4|j==5) {nb_gp=c(nb_gp[1:3])}
  if (j==6) {nb_gp=c(nb_gp[1:2],nb_gp[4])}
  
  print(sum(nb_gp))
  nb_gp_list[[j]]=nb_gp
}

## match individual id to each sample, gather in tables
gp_list_list=list()
tables_list=list()
for (j in 1:6){
  
  clust_coeff=clust_list[[j]]
  nb_gp= nb_gp_list[[j]]
  
  if (nb_classe[j]==3) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2],nb_gp[1]+nb_gp[2]+nb_gp[3])}
  if (nb_classe[j]==2) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2])}
  if (nb_classe[j]==4) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2],nb_gp[1]+nb_gp[2]+nb_gp[3],nb_gp[1]+nb_gp[2]+nb_gp[3]+nb_gp[4])}
  if (nb_classe[j]==1) {index_fin_gp=c(nb_gp[1])}
  
  gp_list=list()
  for (i in 1:length(nb_gp)){
    if (i==1) {gp_list[[i]]=df_rf_a$individual[clust_coeff$order][1:index_fin_gp[1]]}
    if (i>1) {gp_list[[i]]=df_rf_a$individual[clust_coeff$order][(index_fin_gp[i-1]+1):(index_fin_gp[i])]}
  }
  
  if (nb_classe[j]==4) {table=data.frame(individual=levels(df_rf_a$individual), gp1=0,gp2=0,gp3=0,gp4=0)}
  if (nb_classe[j]==3) {table=data.frame(individual=levels(df_rf_a$individual), gp1=0,gp2=0,gp3=0)}
  if (nb_classe[j]==2) {table=data.frame(individual=levels(df_rf_a$individual), gp1=0,gp2=0)}
  if (nb_classe[j]==1) {table=data.frame(individual=levels(df_rf_a$individual), gp1=0)}
  
  for (i in 1:length(gp_list)){
    gp=gp_list[[i]]
    for (n in 1:length(unique(gp))){
      row_index=which(table$individual==unique(gp)[n])
      table[row_index,i+1]=length(which(gp==unique(gp)[n]))
    }
  }
  apply(X=as.matrix(table[,2:(nb_classe[j]+1)]), FUN=sum, MARGIN = 1)
  tables_list[[j]]=table
  gp_list_list[[j]]=gp_list
  print(table)
}

## save
save(tables_list, file = "outputs/tables_list")
save(gp_list_list, file = "outputs/gp_list_list")
save(nb_gp_list, file = "outputs/nb_gp_list")

