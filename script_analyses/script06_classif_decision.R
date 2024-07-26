#######################################################################################
#######################################################################################
# Script 6 - Make classification decision
#######################################################################################
#######################################################################################
library(cluster)
library(dendextend)

numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)

load("outputs/df_rf_a")
load("outputs/diff_surface_list")
load("outputs/matrice_curves_list")
load("outputs/matrice_proba_curves_list")
load("outputs/clust_list")

#plot inertie
varenviroxproba1=as.factor(c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                             "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)"))

replicats <- 200

pdf(file="figures/figS4.pdf", height = 12, width = 6)
par(mfrow=c(3,2))
for (j in 1:6){
  clust_coeff <- clust_list[[j]]
  inertie <- sort(clust_coeff$height, decreasing = TRUE)
  plot(inertie, pch=16, main=varenviroxproba1[j], xlim=c(0,10), ylab='')
}
dev.off()


#choisir le nombre de classes
nb_classe=c(4,2,4,2,2,3) #jour #-1.5,1.5
#nb_classe=c(2,2,3,2,3,3) #jour #-2,2

#plot les dendrogrammes
dend_list=list()
# #pdf("plot_cluster.pdf", height = 15, width = 15)
# par(mfrow=c(1,1))
for (j in 1:6){
  clust_coeff=clust_list[[j]]
  dend <- as.dendrogram(clust_coeff)
  dend <- dend %>% set("branches_k_color", k = nb_classe[j] ) %>%
    set("branches_lwd", value= 2) %>%
    set("labels_cex", 0.01)
  #set("branches_lty", c(1,2,1))
  labels(dend) <- df_rf_a$individual[clust_coeff$order] #species[clust_coeff$order] #nbtraj[clust_coeff$order]#
  #  plot(dend, main =j)
  dend_list[[j]]=dend
}

# above=c(12,4,1.8,3.8,4.5,3)
# below=c(15,12,8.2,5,6,6)  #jour -2,2

above=c(4,2.5,1.05,2,2,1.5)
below=c(8,8,3,3,5,5)  #jour -1.5,1.5

# abline(h=above[j], lty=2) #verif du nombre de groupes
# abline(h=below[j], lty=2) 

nb_gp_list=list()
for (j in 1:6){
  dend=dend_list[[j]]
  (nb_gp=dend %>% get_nodes_attr("members", 
                                 id = which(dend %>% get_nodes_attr("height") > above[j] &dend %>% get_nodes_attr("height")< below[j])))
  
  #-2,2
  # if (j==2) {nb_gp=c(nb_gp[1:2])}
  # if (j==3) {nb_gp=c(nb_gp[1:3])}
  # if (j==5) {nb_gp=c(nb_gp[1],18*replicats-sum(nb_gp[1:2]),nb_gp[2])}
  
  
  #-1.5,1.5
  if (j==1|j==3) {nb_gp=c(nb_gp[1],18*replicats-sum(nb_gp[1:3]),nb_gp[2],nb_gp[3])}
  if (j==2|j==5) {nb_gp=c(nb_gp[1:2])}
  if (j==6) {nb_gp=c(nb_gp[1:3])}
  
  #nuit
  # if (j==2) {nb_gp=nb_gp[c(1:2)]}
  # if (j==3) {nb_gp=nb_gp[c(1:3)]}
  # if (j==5) {nb_gp=c(nb_gp[1],18*replicats-sum(nb_gp[1:2]),nb_gp[2])}
  # if (j==6) {nb_gp=nb_gp[c(1:4)]}
  # if (j==3) {nb_gp=c(18*replicats-sum(nb_gp[c(1,2,5)]),nb_gp[c(1,2,5)])}
  
  print(sum(nb_gp))
  
  nb_gp_list[[j]]=nb_gp
}

#on regroupe les individus
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

save(tables_list, file = "outputs/tables_list")
save(gp_list_list, file = "outputs/gp_list_list")
save(nb_gp_list, file = "outputs/nb_gp_list")

