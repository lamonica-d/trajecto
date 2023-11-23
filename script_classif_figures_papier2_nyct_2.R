rm(list=ls())
library(rjags)
library(viridis)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)

numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)

############generer fichier avec les mcmc#############
# mtot_list=list()
# for (j in 1:length(numpoiss_vect0)){
# 
#   load(paste("/home/lamonica/Documents/poissons/sorties_juillet_23/image_inf_individual_",
#              numpoiss_vect0[j],"_v10.Rdata", sep=""))
# 
#   
#     print(numpoiss_vect0[j])
#     print(gelman.diag(jags_res))
#   # print(fin-debut)
#   # plot(jags_res, trace=T, density=F)
#   mtot_list[[j]]=rbind(jags_res[[1]],jags_res[[2]],jags_res[[3]])
# }
##############################CLASSIF#######################

load("/home/lamonica/Documents/poissons/sorties_juillet_23/mtot_list_v5")
param_names=colnames(mtot_list[[1]])

#############la classif
library(cluster)
library(dendextend)
is.odd <- function(x) x %% 2 != 0
library(geiger)

f_derivee <- function(a,b,c,x, mu, sd){ ((b*sd + 2*c*(x-mu))*exp(a + ((x-mu)*(b*sd + c*(x-mu)))/sd^2))/(sd^2*(exp(a+((x-mu)*(b*sd-c*mu+c*x))/sd^2)+1)^2)}
  
  
  # ((b + 2 *c *(x - mu/sd)) *exp(a + b *(x - mu/sd) + c *(x - mu/sd)^2))/(exp(a + b *(x - mu/sd) + c *(x - mu/sd)^2) + 1) 
  # - ((b + 2 *c *(x - mu/sd)) *exp(2 *a + 2 *b *(x - mu/sd) + 2 *c *(x - mu/sd)^2))/(exp(a + b *(x - mu/sd) + c *(x - mu/sd)^2) + 1)^2}


replicats=200
df_rf_a1=matrix(0,ncol=16,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  #jour 35:36
  #nuit 37:38
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(17:30,35,36)]) #temp[10001:10020,17:30])
}
df_rf_a1=data.frame(df_rf_a1[-1,])
df_rf_a1=cbind(df_rf_a1,individual=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=replicats))))
rm(temp)
df_rf_a=df_rf_a1[order(df_rf_a1$individual),]
rm(df_rf_a1)

index_lin=3:8
index_carre=9:14

#on recupere les valeurs de y pour chaque var/proba/replicat
xvalues=seq(-2,2,0.2)
mu=0
sd=1

matrice_curves_list=list()

for (j in 1:6){
  
  matrice_curves=matrix(0,ncol=length(xvalues),nrow=replicats*18)
  
  for (i in 1:(replicats*18)){
    
    if (is.odd(j)==T) {matrice_curves[i,]= f_derivee(a=df_rf_a[i,1]+df_rf_a[i,15], b=df_rf_a[i,index_lin[j]],
                                                     c=df_rf_a[i,index_carre[j]], x=xvalues, mu=mu, sd=sd)
    }
    
    if (is.odd(j)==F) {matrice_curves[i,]= f_derivee(a=df_rf_a[i,2]+df_rf_a[i,16], b=df_rf_a[i,index_lin[j]],
                                                     c=df_rf_a[i,index_carre[j]], x=xvalues, mu=mu, sd=sd)
    }
    
  }
  matrice_curves_list[[j]]=matrice_curves
}

#calcule la surface 2 a 2
diff_surface_list=list()  
for (j in 1:6){
  matrice_curves= matrice_curves_list[[j]]
  diff_surface=matrix(0,ncol=replicats*18,nrow=replicats*18)
  
  for (i in 1:(replicats*18)){
    for (k in 1:(replicats*18)){
      
      diff_surface[i,k]<-geiger:::.area.between.curves(xvalues, pmax(matrice_curves[i,], matrice_curves[k,]), 
                                                       pmin(matrice_curves[i,], matrice_curves[k,])
                                                       ,xrange = c(-2,2))      
    }
  }
  
  diff_surface_list[[j]]= diff_surface #sqrt(diff_surface^2)
}

##calcul des fonctions de probas pour les plots apres
matrice_proba_curves_list=list()

for (j in 1:6){
  
  matrice_proba_curves=matrix(0,ncol=length(xvalues),nrow=replicats*18)
  
  
  for (i in 1:(replicats*18)){
    
    if (is.odd(j)==T) {calcul_proba <- function (x) {exp(df_rf_a[i,1] + df_rf_a[i,15] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2)/ 
        (1 +  exp(df_rf_a[i,1] + df_rf_a[i,15] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2))}
    }
    
    if (is.odd(j)==F) {calcul_proba <- function (x) {exp(df_rf_a[i,2] + df_rf_a[i,16] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2)/ 
        (1 +  exp(df_rf_a[i,2] + df_rf_a[i,16] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2))}
    }
    
    matrice_proba_curves[i,]=calcul_proba(xvalues)
  }
  
  
  matrice_proba_curves_list[[j]]=matrice_proba_curves
  
}

##classif 
clust_list=list()

#plot inertie
par(mfrow=c(1,6))
for (j in 1:6){
  diff_surface=diff_surface_list[[j]]
  truc_coeff=as.dist(diff_surface)
  clust_coeff=hclust(d=truc_coeff, method="ward.D2")
  clust_list[[j]]=clust_coeff
  inertie <- sort(clust_coeff$height, decreasing = TRUE)
  plot(inertie, pch=16, main=j, xlim=c(0,10))
  #abline(h=10, lty=2)
}


#loader les calculs precedents
load("/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/df_rf_a")
load("/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/diff_surface_list")
load("/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/matrice_curves_list")
load("/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/matrice_proba_curves_list")
load("/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/clust_list")

#hoisir le nombre de classes
nb_classe=c(4,2,4,2,2,3) #jour #-1.5,1.5
nb_classe=c(2,2,3,2,3,3) #jour #-2,2

#plot les dendrogrammes
dend_list=list()
#pdf("plot_cluster.pdf", height = 15, width = 15)
par(mfrow=c(1,1))
for (j in 1:6){
  clust_coeff=clust_list[[j]]
  dend <- as.dendrogram(clust_coeff)
  dend <- dend %>% set("branches_k_color", k = nb_classe[j] ) %>%
    set("branches_lwd", value= 2) %>%
    set("labels_cex", 0.01)
  #set("branches_lty", c(1,2,1))
  labels(dend) <- df_rf_a$individual[clust_coeff$order] #species[clust_coeff$order] #nbtraj[clust_coeff$order]#
  plot(dend, main =j)
  dend_list[[j]]=dend
}

above=c(12,4,1.8,3.8,4.5,3)
below=c(15,12,8.2,5,6,6)  #jour -2,2

#above=c(4,2.5,1.05,2,2,1.5)
#below=c(8,8,3,3,5,5)  #jour -1.5,1.5

abline(h=above[j], lty=2) #verif du nombre de groupes
abline(h=below[j], lty=2) 

nb_gp_list=list()
for (j in 1:6){
  dend=dend_list[[j]]
  (nb_gp=dend %>% get_nodes_attr("members", 
                                 id = which(dend %>% get_nodes_attr("height") > above[j] &dend %>% get_nodes_attr("height")< below[j])))
  
  #-2,2
  if (j==2) {nb_gp=c(nb_gp[1:2])}
  if (j==3) {nb_gp=c(nb_gp[1:3])}
  if (j==5) {nb_gp=c(nb_gp[1],18*replicats-sum(nb_gp[1:2]),nb_gp[2])}
  
  
  #-1.5,1.5
  # if (j==1|j==3) {nb_gp=c(nb_gp[1],18*replicats-sum(nb_gp[1:3]),nb_gp[2],nb_gp[3])}
  # if (j==2|j==5) {nb_gp=c(nb_gp[1:2])}
  # if (j==6) {nb_gp=c(nb_gp[1:3])}

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

##save table + gp list

#######################plots classiques###################################################
main_titles=c(rep("Water depth",2),rep("Flow velocity",2),rep("Upstream temperature difference",2))
ylab_vect=c("p(R->M)", "p(M->R)")

###medianes

pdf("plot_enviro_mediane_groupe_surface_derivee_jour_2023.pdf", height = 8, width = 6)

par(mfrow=c(3,2))
for (j in 1:6){
  
  nb_gp=nb_gp_list[[j]]
  
  var_enviro=seq(-1.5,1.5,0.01)
  mu2=0
  sd2=1
  
  p=matrix(0,ncol=length(var_enviro),nrow=1)
  vect_a=as.numeric()
  
  for (i in 1:length(numpoiss_vect0)){   #length(numpoiss_vect0)){
    
    #jour 19,20
    #nuit 21,22
    
    temp=mtot_list[[i]]
    vect_a=as.numeric()
    for (q in 17:length(temp[1,])){
      param_quantile=as.numeric(quantile(temp[,q],probs = 0.5))
      vect_a=c(vect_a,param_quantile)
    }
    
    if (j<7 & is.odd(j)==T) {p=rbind(p,exp(vect_a[1] + vect_a[19] + vect_a[index_lin[j]] * (var_enviro-mu2)/sd2 + vect_a[index_carre[j]] * ((var_enviro-mu2)/sd2)^2)/ 
                                       (1 +  exp(vect_a[1] + vect_a[19] + vect_a[index_lin[j]] * var_enviro + vect_a[index_carre[j]] * ((var_enviro-mu2)/sd2)^2)))
    }
    
    if (j<7 & is.odd(j)==F) {p=rbind(p,exp(vect_a[2] + vect_a[20] + vect_a[index_lin[j]] * (var_enviro-mu2)/sd2 + vect_a[index_carre[j]] * ((var_enviro-mu2)/sd2)^2)/ 
                                       (1 +  exp(vect_a[2] + vect_a[20] + vect_a[index_lin[j]] * (var_enviro-mu2)/sd2 + vect_a[index_carre[j]] * ((var_enviro-mu2)/sd2)^2)))
    }
    
  }
  p=p[-1,]
  
  table=tables_list[[j]]
  indices=match(id_sp,table$individual)
  table2=table[indices,]
  
  if (is.odd(j)==T) {plot(var_enviro, p[1,], ylim=c(0,1), type="n",main=main_titles[j], xlab="", ylab=ylab_vect[1], las=1)}
  if (is.odd(j)==F) {plot(var_enviro, p[1,], ylim=c(0,1), type="n",main=main_titles[j], xlab="", ylab=ylab_vect[2], las=1)}
  
  ##couleur groupe, lty species  
  # col_groupe=c("#F8766D","#7CAE00", "#00BFC4","#C77CFF")
  # 
  # for (i in 1:length(id_sp)){
  #   col_i=col_groupe[which(table2[i,2:(length(nb_gp)+1)]==max(table2[i,2:(length(nb_gp)+1)]))]
  #   species_lty=ifelse(substr(as.character(table2[i,1]), 1,1) =="B", 1, ifelse(substr(as.character(table2[i,1]), 1,1) =="S", 3,5))
  #   lines(var_enviro, p[i,],  col=col_i, lty=species_lty ) #, lwd=vect_tailles[i]/400, lty=species_lty) 
  # }
  
  ##couleur species, lty groupe  
  col_species=c("#F8766D","#7CAE00", "#00BFC4")
  vect_groupe=1:4
  for (i in 1:length(id_sp)){
    lty_i=vect_groupe[which(table2[i,2:(length(nb_gp)+1)]==max(table2[i,2:(length(nb_gp)+1)]))]
    species_index=ifelse(substr(as.character(table2[i,1]), 1,1) =="B", 1, ifelse(substr(as.character(table2[i,1]), 1,1) =="S", 2,3))
    lines(var_enviro, p[i,],  lty=lty_i, col=col_species[species_index] ) #, lwd=vect_tailles[i]/400, lty=species_lty) 
  }
  
}
dev.off()

###replicats version derivees ou version proba
derivee=F
pdf("plot_enviro_replicats_groupe_surface_proba_jour_2023.pdf", height = 15, width = 15)

for (j in 1:6){
  col_groupe=c("#F8766D","#7CAE00", "#00BFC4","#C77CFF")
  if(derivee==T) {matrice = matrice_curves_list[[j]]}else{matrice = matrice_proba_curves_list[[j]]}
  table= tables_list[[j]]
  clust_coeff=clust_list[[j]]
  
  par(mfrow=c(5,4))
  nb_gp= nb_gp_list[[j]]
  
  if (is.odd(j)==T) {ylabel=ylab_vect[1]}
  if (is.odd(j)==F) {ylabel=ylab_vect[2]}
  
  if (nb_classe[j]==4) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2],nb_gp[1]+nb_gp[2]+nb_gp[3],
                                       nb_gp[1]+nb_gp[2]+nb_gp[3]+nb_gp[4])}
  if (nb_classe[j]==3) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2],nb_gp[1]+nb_gp[2]+nb_gp[3])}
  if (nb_classe[j]==2) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2])}
  if (nb_classe[j]==1) {index_fin_gp=c(nb_gp[1])}
  
  if(derivee==T) {ylim_vect = c(-0.5,0.5)}else{ylim_vect = c(0,1)}
  
  
  for (i in seq(1,replicats*length(numpoiss_vect0),replicats)){
    plot(xvalues,matrice[i,], type="n", main=df_rf_a$individual[i], ylab="",xlab="",ylim =ylim_vect)
    
    n=round(i/(replicats)+1-0.01)
    # col_i=col_groupe[which(table[n,2:length(table[n,])]==max(table[n,2:length(table[n,])]))]
    
    for (p in i:(i+replicats-1)){
      index=which(clust_coeff$order==p)
      
      if (nb_classe[j]==4) {index_col=ifelse(index<=index_fin_gp[1],1, ifelse(index<=index_fin_gp[2],2, 
                                                                              ifelse(index>index_fin_gp[3],4,3)))}
      
      if (nb_classe[j]==3) {index_col=ifelse(index<=index_fin_gp[1],1, ifelse(index>index_fin_gp[2],3,2))}
      if (nb_classe[j]==2) {index_col=ifelse(index<=index_fin_gp[1],1,2)}
      
      
      lines(xvalues,matrice[p,], col=col_groupe[index_col])#col=col_i)
    }
    
    # print(n)
    #print(table[n,2:length(table[n,])])
    if (nb_classe[j]==3) {text(c(0.2,0.5,0.8), rep(0.7-0.3,3), as.numeric(table[n,2:length(table[n,])]), cex=1.2, col="red")}
    if (nb_classe[j]==2) {text(c(0.2,0.5), rep(0.7-0.3,2), as.numeric(table[n,2:length(table[n,])]), cex=1.2, col="red")}
    if (nb_classe[j]==4) {text(c(0.05,0.35,0.65,0.95), rep(0.7-0.3,4), as.numeric(table[n,2:length(table[n,])]), cex=1.2, col="red")}
    if (derivee==T) {abline(h=0, lty=2, lwd=1.3, col="darkgrey")}
    
  }
  
  title(paste(main_titles[j],", ",ylabel, sep=''), line = -1, outer = TRUE)
}
dev.off()



##############################ggplot##################################################################################################

####gestion des sd et moy pour naviguer entre varenviro standardisees et vraies valeurs
load("/home/lamonica/Documents/poissons/redaction_papier_poisson_deux/leftovers (copie)/analyse/data_pr_std")
data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)


##########################
#####plots medianes ######
##########################
##dataframe pour plot des medianes################
xvalues=seq(-2,2,0.2)

xvalues_plot=xvalues
individu=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=length(xvalues_plot)*2*3)))
species=as.factor(ifelse(substr(as.character(individu), 1,1) =="B", "Barbel", ifelse(substr(as.character(individu), 1,1) =="S", "Catfish","Chub")))
proba=rep(c(rep("p12",length(xvalues_plot)*length(id_sp)),rep("p21",length(xvalues_plot)*length(id_sp))),3)
var_enviro=c(rep("Water depth", length(xvalues_plot)*length(id_sp)*2),
             rep("Flow velocity", length(xvalues_plot)*length(id_sp)*2),rep("Upstream temperature difference", length(xvalues_plot)*length(id_sp)*2))
xvalues_df=rep(xvalues_plot,6*length(id_sp))

yvalues_mediane=as.numeric()
var_enviro=xvalues_plot
mu=0
sd=1

for (i in 1:length(numpoiss_vect0)){
  temp=mtot_list[[i]]
  vect_a=as.numeric()
  for (q in 17:length(temp[1,])){
    param_quantile=as.numeric(quantile(temp[,q],probs = 0.5))
    vect_a=c(vect_a,param_quantile)
  }
  
  #jour 19,20
  #nuit 21,22
  
  for (j in 1:6){
    if (is.odd(j)==T) {yvalues_mediane=c(yvalues_mediane,exp(vect_a[1] + vect_a[19] + vect_a[index_lin[j]] * (var_enviro-mu)/sd + vect_a[index_carre[j]] * ((var_enviro-mu)/sd)^2)/ 
                                           (1 +  exp(vect_a[1] + vect_a[19] + vect_a[index_lin[j]] * (var_enviro-mu)/sd + vect_a[index_carre[j]] * ((var_enviro-mu)/sd)^2)))
    }
    
    if (is.odd(j)==F) {yvalues_mediane=c(yvalues_mediane,exp(vect_a[2] + vect_a[20] + vect_a[index_lin[j]] * (var_enviro-mu)/sd + vect_a[index_carre[j]] * ((var_enviro-mu)/sd)^2)/ 
                                           (1 +  exp(vect_a[2] + vect_a[20] + vect_a[index_lin[j]] * (var_enviro-mu)/sd + vect_a[index_carre[j]] * ((var_enviro-mu)/sd)^2)))
    
    }
    
  }
}

groupes_vect1=as.numeric()
for (i in 1:length(id_sp)){
  for (j in 1:6){
    table=tables_list[[j]]
    nb_gp=nb_gp_list[[j]]
    indices=match(id_sp,table$individual)
    table2=table[indices,]
    #groupes_vect1=c(groupes_vect1,which(table2[i,2:(length(nb_gp)+1)]==max(table2[i,2:(length(nb_gp)+1)])))
    truc=which(table2[i,2:(length(nb_gp)+1)]==max(table2[i,2:(length(nb_gp)+1)]))
    if (length(truc)>1) {truc=truc[1]}
    groupes_vect1=c(groupes_vect1,truc)
  }
}
groupes_vect=as.factor(unlist(lapply(X=groupes_vect1, FUN=rep, times=length(xvalues_plot))))

varenviroxproba1=c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                   "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)")
varenviroxproba=as.factor(rep(unlist(lapply(X=varenviroxproba1, FUN=rep, times=length(xvalues_plot))),length(id_sp)))

df_mediane=data.frame(individu=individu, species=species, group=groupes_vect, j=varenviroxproba, 
                      xvalues=xvalues_df, yvalues=yvalues_mediane)

sd_vect2=rep(sd_vect, each=2)
moy_vect2=rep(moy_vect, each=2)

df_mediane=cbind(df_mediane,xtruevalues=rep(0,length(df_mediane$individu)))

for (i in 1:length(varenviroxproba1)){
  df_mediane[df_mediane$j==varenviroxproba1[i],]$xtruevalues= df_mediane[df_mediane$j==varenviroxproba1[i],]$xvalues*sd_vect2[i]+moy_vect2[i]
}

############le plot#################################
load("/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/df_mediane2023")

plots <- ggplot(data=df_mediane, aes(x=xvalues, y=yvalues , color =group#color = species, linetype=groupe 
                                     ,group=individu))+
  
  #data=df_croiss, aes(x=size, y=value,group=interaction(quantile, patch) ,
  #                                                      linetype=quantile,color = patch))+
  
  #facet_wrap( j~species, ncol=3)+#, fill=ref))+
  facet_grid( j~ species)+ #facet_grid( species~j)+
  #scale_color_viridis("Forest stand",discrete=TRUE) +
  geom_line(lwd=1)+ #
  #scale_color_viridis(discrete=TRUE,labels = c("S", "S+G", "S+T", "S+D", "S+T+D", "S+D+G", "S+T+G","S+T+D+G", "Ref")) 
  #scale_linetype_manual(values=c(1, 2,2),labels = c("50%", "2.5%", "97.5"))+
  #scale_color_manual(values=c("black", "yellow","gray60"),labels = c("Posteriors", "True values", "Prior"))+
  
  labs(y="", x="")+ ggtitle("")+
  
  theme_minimal(base_size=15)+
  theme(legend.position = "none",axis.text=element_text(size=15),axis.title=element_text(size=15), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 


pdf("figure3_jour_2023.pdf", height = 20, width = 8)
plots
dev.off()


#####le plot sans classe

plots <- ggplot(data=df_mediane, aes(x=xvalues, y=yvalues,group=individu, linetype=species))+
  
  scale_linetype_manual(values=c("solid", "longdash", "dotted"))+
  facet_wrap( j~., ncol=2)+ #facet_grid( species~j)+
  geom_line(lwd=1)+ 
  labs(y="Behaviour switching probability", x="Environmental variable")+ ggtitle("")+
  
  theme_minimal(base_size=15)+
  theme(legend.position = "none",axis.text=element_text(size=15),axis.title=element_text(size=15), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) + 
  scale_y_continuous(limits=c(0,1)) #+
# scale_x_continuous(limits=c(-1,1))

pdf("figure3_jour_2023_v6.pdf", height = 12, width = 10)
plots
dev.off()


####le meme plot mais avec des classes de tailles
load("/home/dominique.lamonica/Bureau/juin2021_ef_nyct/classif_15_07/df_ggplot_median2023")
xvalues_plot=seq(-2,2,0.2)
size=as.factor(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3)))
df_mediane2=data.frame(df_mediane, size=size)
size_class=ifelse(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3))>600,3,
                  ifelse(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3))>=400,2,1))
df_mediane3=data.frame(df_mediane, size_class=as.factor(size_class))
levels(df_mediane3$size_class)=c("< 40 cm", "40 - 60 cm", "60 cm <")

##le plot
plots <- ggplot(data=df_mediane3, aes(x=xvalues, y=yvalues , color =group#color = species, linetype=groupe 
                                      ,group=individu))+
  
  #data=df_croiss, aes(x=size, y=value,group=interaction(quantile, patch) ,
  #                                                      linetype=quantile,color = patch))+
  
  #facet_wrap( j~species, ncol=3)+#, fill=ref))+
  facet_grid( j~ size_class)+ #facet_grid( species~j)+
  #scale_color_viridis("Forest stand",discrete=TRUE) +
  geom_line(lwd=1)+ #
  #scale_color_viridis(discrete=TRUE,labels = c("S", "S+G", "S+T", "S+D", "S+T+D", "S+D+G", "S+T+G","S+T+D+G", "Ref")) 
  #scale_linetype_manual(values=c(1, 2,2),labels = c("50%", "2.5%", "97.5"))+
  #scale_color_manual(values=c("black", "yellow","gray60"),labels = c("Posteriors", "True values", "Prior"))+
  
  labs(y="", x="")+ ggtitle("")+
  
  theme_minimal(base_size=15)+
  theme(legend.position = "none",axis.text=element_text(size=15),axis.title=element_text(size=15), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 


pdf("figure3bis_jour_2023.pdf", height = 20, width = 8)
plots
dev.off()



##############################################
#####plots replicats (proba et derivee) ######
##############################################

########dataframe pour plot des replicats###############
xvalues_plot=xvalues
individu=as.factor(rep(unlist(lapply(X=id_sp, FUN=rep, times=length(xvalues_plot)*replicats)),6))
species=as.factor(ifelse(substr(as.character(individu), 1,1) =="B", "Barbel", ifelse(substr(as.character(individu), 1,1) =="S", "Catfish","Chub")))
xvalues_df=rep(xvalues_plot,6*length(id_sp)*replicats)
replicats_vect=as.factor(rep(unlist(lapply(X=1:replicats, FUN=rep, times=length(xvalues_plot))), 2*3*length(id_sp)))

yvalues_replicats_proba=as.numeric()
for (j in 1:6){
  matrice=t(matrice_proba_curves_list[[j]])
  yvalues_replicats_proba=c(yvalues_replicats_proba,as.vector(matrice))
}

yvalues_replicats_derivee=as.numeric()
for (j in 1:6){
  matrice=t(matrice_curves_list[[j]])
  yvalues_replicats_derivee=c(yvalues_replicats_derivee,as.vector(matrice))
}

groupes_vect1=as.numeric()
for (j in 1:6){
  nb_gp= nb_gp_list[[j]]
  clust_coeff=clust_list[[j]]
  
  if (nb_classe[j]==4) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2],nb_gp[1]+nb_gp[2]+nb_gp[3],
                                       nb_gp[1]+nb_gp[2]+nb_gp[3]+nb_gp[4])}
  if (nb_classe[j]==3) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2],nb_gp[1]+nb_gp[2]+nb_gp[3])}
  if (nb_classe[j]==2) {index_fin_gp=c(nb_gp[1],nb_gp[1]+nb_gp[2])}
  
  for (i in seq(1,replicats*length(numpoiss_vect0),replicats)){
    n=round(i/(replicats)+1-0.01)
    
    for (p in i:(i+replicats-1)){
      index=which(clust_coeff$order==p)
      
      if (nb_classe[j]==4) {index_col=ifelse(index<=index_fin_gp[1],1, ifelse(index<=index_fin_gp[2],2,ifelse(index>index_fin_gp[3],4,3)))}
      if (nb_classe[j]==3) {index_col=ifelse(index<=index_fin_gp[1],1, ifelse(index>index_fin_gp[2],3,2))}
      if (nb_classe[j]==2) {index_col=ifelse(index<=index_fin_gp[1],1,2)}
      
      groupes_vect1=c(groupes_vect1,index_col)  
      
    }
  }
}
groupes_vect=as.factor(unlist(lapply(X=groupes_vect1, FUN=rep, times=length(xvalues_plot))))

varenviroxproba1=c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                   "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)")
varenviroxproba=as.factor(unlist(lapply(X=varenviroxproba1, FUN=rep, times=length(xvalues_plot)*replicats*length(id_sp))))

df_replicats=data.frame(individu=individu, species=species, replicats=replicats_vect, group=groupes_vect, j=varenviroxproba, 
                        xvalues=xvalues_df, yvalues_proba=yvalues_replicats_proba, yvalues_derivee=yvalues_replicats_derivee)


df_replicats=cbind(df_replicats,xtruevalues=rep(0,length(df_replicats$individu)))

for (i in 1:length(varenviroxproba1)){
  df_replicats[df_replicats$j==varenviroxproba1[i],]$xtruevalues= df_replicats[df_replicats$j==varenviroxproba1[i],]$xvalues*sd_vect2[i]+moy_vect2[i]
}


###############plots#####
load("/home/dominique.lamonica/Bureau/juin2021_ef_nyct/classif_15_07/df_ggplot_replicats_jour_2023")

plot_derivee=list()
for (i in 1:6){
  plot_derivee[[i]] <- ggplot(data=df_replicats[df_replicats$j==varenviroxproba1[i],], 
                              aes(x=xvalues, y=yvalues_derivee ,color = group, group=replicats))+
    
    facet_wrap( ~ individu, ncol=4)+#, fill=ref))+
    #scale_color_viridis("Forest stand",discrete=TRUE) +
    geom_line(lwd=0.6)+ #
    geom_hline(yintercept=0, linetype=2, color="black", linewidth=0.8) +
    #scale_color_viridis(discrete=TRUE,labels = c("S", "S+G", "S+T", "S+D", "S+T+D", "S+D+G", "S+T+G","S+T+D+G", "Ref")) 
    #scale_linetype_manual(values=c(1, 2,2),labels = c("50%", "2.5%", "97.5"))+
    #scale_color_manual(values=c("black", "yellow","gray60"),labels = c("Posteriors", "True values", "Prior"))+
    labs(y="", x="")+ ggtitle(varenviroxproba1[i])+
    
    theme_minimal(base_size=12)+
    theme(legend.position = c(0.8,0.01), legend.direction = "horizontal",axis.text=element_text(size=12),axis.title=element_text(size=10), 
          axis.text.x = element_text(size=8.5),
          axis.text.y = element_text(size=8.5),plot.title = element_text(size=12))
}

pdf("figure2_derivee_jour_2023.pdf", height = 15, width = 15)
grid.arrange(plot_derivee[[4]],plot_derivee[[3]],plot_derivee[[6]],
             plot_derivee[[5]],plot_derivee[[2]],plot_derivee[[1]], 
             ncol=2, nrow = 3)
dev.off()



plot_proba_tv=list()
for (i in 1:6){
  plot_proba_tv[[i]] <- ggplot(data=df_replicats[df_replicats$j==varenviroxproba1[i],], 
                            aes(x=xtruevalues, y=yvalues_proba ,color = group, group=replicats))+
    
    facet_wrap( ~ individu, ncol=4)+#, fill=ref))+
    #scale_color_viridis("Forest stand",discrete=TRUE) +
    geom_line(lwd=0.6)+ #
    #scale_color_viridis(discrete=TRUE,labels = c("S", "S+G", "S+T", "S+D", "S+T+D", "S+D+G", "S+T+G","S+T+D+G", "Ref")) 
    #scale_linetype_manual(values=c(1, 2,2),labels = c("50%", "2.5%", "97.5"))+
    #scale_color_manual(values=c("black", "yellow","gray60"),labels = c("Posteriors", "True values", "Prior"))+
    
    labs(y="", x="")+ ggtitle(varenviroxproba1[i])+
    
    theme_minimal(base_size=12)+
    theme(legend.position = c(0.8,0.01), legend.direction = "horizontal",axis.text=element_text(size=12),axis.title=element_text(size=10), 
          axis.text.x = element_text(size=8.5),
          axis.text.y = element_text(size=8.5),plot.title = element_text(size=12))
}


pdf("figure2_proba_jour_truevalues_2023.pdf", height = 15, width = 15)
grid.arrange(plot_proba_tv[[4]],plot_proba_tv[[3]],plot_proba_tv[[6]],
             plot_proba_tv[[5]],plot_proba_tv[[2]],plot_proba_tv[[1]], ncol=2, nrow = 3)
dev.off()


plot_proba_std=list()
for (i in 1:6){
  plot_proba_std[[i]] <- ggplot(data=df_replicats[df_replicats$j==varenviroxproba1[i],], 
                            aes(x=xvalues, y=yvalues_proba ,color = group, group=replicats))+
    
    facet_wrap( ~ individu, ncol=4)+#, fill=ref))+
    #scale_color_viridis("Forest stand",discrete=TRUE) +
    geom_line(lwd=0.6)+ #
    #scale_color_viridis(discrete=TRUE,labels = c("S", "S+G", "S+T", "S+D", "S+T+D", "S+D+G", "S+T+G","S+T+D+G", "Ref")) 
    #scale_linetype_manual(values=c(1, 2,2),labels = c("50%", "2.5%", "97.5"))+
    #scale_color_manual(values=c("black", "yellow","gray60"),labels = c("Posteriors", "True values", "Prior"))+
    
    labs(y="", x="")+ ggtitle(varenviroxproba1[i])+
    
    theme_minimal(base_size=12)+
    theme(legend.position = c(0.8,0.01), legend.direction = "horizontal",axis.text=element_text(size=12),axis.title=element_text(size=10), 
          axis.text.x = element_text(size=8.5),
          axis.text.y = element_text(size=8.5),plot.title = element_text(size=12))
}


pdf("figure2_proba_jour_std_2023.pdf", height = 15, width = 15)
grid.arrange(plot_proba_std[[4]],plot_proba_std[[3]],plot_proba_std[[6]],
             plot_proba_std[[5]],plot_proba_std[[2]],plot_proba_std[[1]], ncol=2, nrow = 3)
dev.off()


########################################
#####95%IC + barplots###################
########################################
####prepa des dataframes##############
load("/home/dominique.lamonica/Bureau/juin2021_ef_nyct/classif_15_07/df_ggplot_replicats_jour_2023")

xvalues=unique(df_replicats$xvalues)
xtruevalues=unique(df_replicats$xtruevalues)
varenviroxproba1=as.factor(c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                             "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)"))

df_ic=data.frame(df_replicats[1,c(1,5,6,9)],ic_inf=0, ic_sup=0) #quantile=0, yvalues=0)

for (i in 1:length(id_sp)){
  
  temp1=df_replicats[df_replicats$individu==id_sp[i],]
  
  for (k in 1:6){
    
    temp2=temp1[temp1$j==varenviroxproba1[k],]
    
    for (x in 1:length(xvalues)){
      
      temp3=temp2[temp2$xvalues==xvalues[x],]
      
      ic_inf=quantile(temp3$yvalues_proba,probs=0.025)
      ic_sup=quantile(temp3$yvalues_proba,probs=0.975)
      
      df_ic=rbind(df_ic,data.frame(individu=id_sp[i],j=as.factor(varenviroxproba1[k]),xvalues=xvalues[x],xtruevalues=xtruevalues[x],ic_inf=ic_inf, ic_sup=ic_sup))
      #df_ic=rbind(df_ic,data.frame(individu=id_sp[i],j=as.factor(varenviroxproba1[k]),xvalues=xvalues[x],quantile=1,yvalues=ic_sup))
      
    }
  }
}
df_ic=df_ic[-1,]
#df_ic$quantile=as.factor(df_ic$quantile)

####load les data + generer les plots IC#####
setwd("/home/dominiquelamonica/Bureau/classif_dec21")

load(file="df_ggplot_replicats_jour_dec21")
xvalues=unique(df_replicats$xvalues)
varenviroxproba1=as.factor(c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                             "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)"))

load(file="df_ggplot_ic_jour_dec21")

#les picto species
library(png)
library(patchwork)

#poiss <- readPNG("/home/dominique.lamonica/Bureau/juin2021_ef_nyct/poissons.png")
baf  <- readPNG("barbeau.png", native=T)
che  <- readPNG("chevesne.png", native=T)
sil  <- readPNG("silure.png", native=T)

picto_list=list()
for (i in 1:18){
  
  if(i<5){picto_list[[i]] <- inset_element(p = baf,left = 0., bottom = 0, right = 1, top = 1)}
  if(i>4 & i<13){picto_list[[i]] <- inset_element(p = che,left = 0., bottom = 0., right = 1, top = 1)}
  if(i>12){picto_list[[i]] <- inset_element(p = sil,left = 0., bottom = 0, right = 1, top = 1)}
}


#IC
plot_ic=list()
for (i in 1:6){
  plot_ic[[i]] <- ggplot(data=df_ic[df_ic$j==varenviroxproba1[i],])+
    
    facet_wrap( ~ individu, ncol=4)+
    geom_ribbon(aes(ymin=ic_inf, ymax=ic_sup, x=xvalues), alpha = 0.4)+
    scale_fill_manual(values=c("gray60"))+
    
    
    labs(y="", x="")+ ggtitle(varenviroxproba1[i])+
    theme_light()#+ annotation_raster(poiss, ymin =-Inf,ymax= Inf,xmin = -Inf,xmax = Inf)
  
}
#######charger les tables de pourcentages + generer les barlots + imprimer les graphes superposes pour chaque coupe (enviro,proba)####

table_names=c('wdepth_prm','wdepth_pmr','flow_velocity_prm','flow_velocity_pmr','difftemp_prm','difftemp_pmr')

#version si on a le fichier avec la liste des tables
load(file="tables_list")

for (k in 1:6){
  
  #version si on a les fichiers txt des tables  
  # table_k=read.table(file=paste("/home/dominique.lamonica/Bureau/juin2021/classif_15_07/table_",table_names[k],sep=""), sep="&")
  
  #version si on a le fichier avec la liste des tables
  table_k=tables_list[[k]]
  table_k[,2:ncol(table_k)] <- table_k[,2:ncol(table_k)]/2
  
  #####    
  df_barplot_k=data.frame(individu=rep(id_sp[1],ncol(table_k)-1), groupe=as.factor(1:(ncol(table_k)-1)), value=as.numeric(table_k[1,2:ncol(table_k)]))
  for (i in 2:18){
    df_barplot_k=rbind(df_barplot_k, data.frame(individu=rep(id_sp[i],ncol(table_k)-1), groupe=as.factor(1:(ncol(table_k)-1)),
                                                value=as.numeric(table_k[i,2:ncol(table_k)])))
    
  }
  
  barplot_list=list()
  for (i in 1:18){
    if(i<5){truc=baf}
    if(i>4 & i<13){truc=che}
    if(i>12){truc=sil}
    
    barplot_list[[i]]<-ggplot(data=df_barplot_k[df_barplot_k$individu==id_sp[i],], aes(x=groupe, y=value, fill=groupe)) + 
      geom_bar(stat = "identity") +scale_fill_hue(c = 100) + labs(y="", x="") +theme_classic()+
      theme(legend.position = "none",plot.margin=unit(rep(-1,4),"lines"))+ 
      scale_y_continuous(limits=c(0,100))#+
    #inset_element(truc,left = 0.8, bottom = 0.8, right = 1, top = 1)
  }
  
  x_pos=rep( seq(0.145,0.87,length.out=4) ,5)
  y_pos= rev(rep(seq(0.16,0.92,length.out=5), each=4))
  
  x_pos_picto=rep( seq(0.244,0.965,length.out=4) ,5)
  y_pos_picto= rev(rep(seq(0.17,0.933,length.out=5), each=4))
  
  #pdf(paste("test2.pdf", sep=""),width=15,height=15)
  pdf(paste("ic_and_percent",table_names[k],"_v1.pdf", sep=""),width=15,height=15)
  print(plot_ic[[k]])
  for (i in 1:18){
    subvp=viewport(width=0.04,height=0.04,x=x_pos[i],y=y_pos[i])
    subvp2=viewport(width=0.055,height=0.055,x=x_pos_picto[i],y=y_pos_picto[i])
    print(barplot_list[i],vp=subvp)
    print(picto_list[[i]],vp=subvp2)
    
  }
  graphics.off()
  
}



########################################
#####boxplots des coeff effets fixes####
########################################

#####prepa du dataframe###############
load("~/Bureau/mtot_list_v5")

replicats=400
df_rf_a1=matrix(0,ncol=8,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  #aube 31:32
  #crepuscule 33:34
  #jour 35:36
  #nuit 37:38
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(31:38)]) #temp[10001:10020,17:30])
}
df_rf_a1=data.frame(df_rf_a1[-1,])
df_rf_a1=cbind(df_rf_a1,individual=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=replicats))))
rm(temp)
df_rf_a=df_rf_a1[order(df_rf_a1$individual),]

#prm
individu=as.factor(rep(unlist(lapply(X=id_sp, FUN=rep, times=replicats)),4))
species=as.factor(ifelse(substr(as.character(individu), 1,1) =="B", "Barbel", ifelse(substr(as.character(individu), 1,1) =="S", "Catfish","Chub")))
nyct=as.factor(rep(c("Dawn", "Dusk", "Day", "Night"),each=18*replicats))
proba=as.factor(rep("p(R->M)",4*replicats*18))
yvalues_coeff=c(df_rf_a[,1],df_rf_a[,3],df_rf_a[,5],df_rf_a[,7])
df1=data.frame(individu=individu, species=species, nyct=nyct, proba=proba, yvalues_coeff=yvalues_coeff)

#pmr
individu=as.factor(rep(unlist(lapply(X=id_sp, FUN=rep, times=replicats)),4))
species=as.factor(ifelse(substr(as.character(individu), 1,1) =="B", "Barbel", ifelse(substr(as.character(individu), 1,1) =="S", "Catfish","Chub")))
nyct=as.factor(rep(c("Dawn", "Dusk", "Day", "Night"),each=18*replicats))
proba=as.factor(rep("p(M->R)",4*replicats*18))
yvalues_coeff=c(df_rf_a[,2],df_rf_a[,4],df_rf_a[,6],df_rf_a[,8])
df2=data.frame(individu=individu, species=species, nyct=nyct, proba=proba, yvalues_coeff=yvalues_coeff)

df_coeff=rbind(df1,df2)  

##################anova test################
library(rstatix)
#pour prm
for (i in 1:length(id_sp)){
temp=df_coeff[df_coeff$individu==id_sp[i] & df_coeff$proba=="p(R->M)",]
print(id_sp[i])
print(anova_test(temp, yvalues_coeff~ nyct))
print(tukey_hsd(temp, yvalues_coeff~ nyct))
}

#pour pmr
for (i in 1:length(id_sp)){
  temp=df_coeff[df_coeff$individu==id_sp[i]  & df_coeff$proba=="p(M->R)",]
  print(id_sp[i])
  print(anova_test(temp, yvalues_coeff~ nyct))
  print(tukey_hsd(temp, yvalues_coeff~ nyct))
}

#####plot###############
load(file="/home/dominique.lamonica/Bureau/juin2021/classif_15_07/df_ggplot_coeff_15_07")

plot_coeff_prm<-ggplot()+ 
  geom_boxplot(data=df_coeff[df_coeff$proba=="p(R->M)",],width=0.8,lwd=1, #1.8
               aes(x=nyct, y=yvalues_coeff, color=nyct))+ 
  facet_wrap( ~ individu, ncol=4)+
  
  # scale_color_manual(values=c("black","dodgerblue1", "cyan4", "grey80"))+
  # scale_fill_manual(values=c("dodgerblue1", "white", "dodgerblue1green", "grey80"))+
  
  
  labs(y="Fixed effect coefficient, posterior distribution", x="")+ggtitle("p(R->M)")+
  theme_minimal()+  theme(legend.position = "none")

plot_coeff_pmr<-ggplot()+ 
  geom_boxplot(data=df_coeff[df_coeff$proba=="p(M->R)",],width=0.8,lwd=1, #1.8
               aes(x=nyct, y=yvalues_coeff, color=nyct))+ 
  facet_wrap( ~ individu, ncol=4)+
  
  # scale_color_manual(values=c("black","dodgerblue1", "cyan4", "grey80"))+
  # scale_fill_manual(values=c("dodgerblue1", "white", "dodgerblue1green", "grey80"))+
  
  
  labs(y="Fixed effect coefficient, posterior distribution", x="Nycthemeral period")+ggtitle("p(M->R)")+
  theme_minimal()+  theme(legend.position = "none")

pdf("effets_fixes_coeff_boxplot_2023.pdf", height = 14, width = 6)
grid.arrange(plot_coeff_prm,plot_coeff_pmr,ncol=1, nrow = 2)
dev.off()


########################################
#####boxplots des coeff effet enviro####
########################################

#####prepa du dataframe###############
load("~/Bureau/mtot_list_v5")

replicats=2
df_rf_a1=matrix(0,ncol=12,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(19:30)]) #temp[10001:10020,17:30])
}
df_rf_a1=data.frame(df_rf_a1[-1,])
df_rf_a1=cbind(df_rf_a1,individual=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=replicats))))
rm(temp)
df_rf_a=df_rf_a1[order(df_rf_a1$individual),]

#prm
individu=as.factor(rep(rep(id_sp, each=replicats),6))

species=as.factor(ifelse(substr(as.character(individu), 1,1) =="B", "Barbel", ifelse(substr(as.character(individu), 1,1) =="S", "Catfish","Chub")))
enviro=as.factor(rep(c("Water depth", "Flow velocity", "Temperature diff."),each=18*replicats*2))
type_coeff=as.factor(rep(rep(c("Lin", "Quad"),each=18*replicats),3))
proba=as.factor(rep("p(R->M)",6*replicats*18))
yvalues_coeff=c(df_rf_a[,1],df_rf_a[,7],df_rf_a[,3],df_rf_a[,9],df_rf_a[,5],df_rf_a[,11])
df1=data.frame(individu=individu, species=species, enviro=enviro, proba=proba, type_coeff=type_coeff, yvalues_coeff=yvalues_coeff)

#pmr
individu=as.factor(rep(unlist(lapply(X=id_sp, FUN=rep, times=replicats)),6))
species=as.factor(ifelse(substr(as.character(individu), 1,1) =="B", "Barbel", ifelse(substr(as.character(individu), 1,1) =="S", "Catfish","Chub")))
enviro=as.factor(rep(c("Water depth", "Flow velocity", "Temperature diff."),each=18*replicats*2))
type_coeff=as.factor(rep(rep(c("Lin", "Quad"),each=18*replicats),3))
proba=as.factor(rep("p(M->R)",6*replicats*18))
yvalues_coeff=c(df_rf_a[,2],df_rf_a[,8],df_rf_a[,4],df_rf_a[,10],df_rf_a[,6],df_rf_a[,12])
df2=data.frame(individu=individu, species=species, enviro=enviro, proba=proba, type_coeff=type_coeff, yvalues_coeff=yvalues_coeff)

df_coeff=rbind(df1,df2)  

#####plot###############
load(file="/home/dominique.lamonica/Bureau/juin2021/classif_15_07/df_ggplot_coeff_enviro_15_07")

plot_coeff_prm<-ggplot()+ 
  geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_coeff[df_coeff$proba=="p(R->M)",],width=0.8,lwd=0.4,
               aes(x=enviro, y=yvalues_coeff, fill=enviro,color=enviro,alpha=type_coeff))+ 
  facet_wrap( ~ individu, ncol=4)+
  
  #scale_color_manual(values=c(""))+
  scale_alpha_manual(values=c(1,0.4)) +
  # scale_fill_manual(values=c("dodgerblue1", "white", "dodgerblue1green", "grey80"))+
  
  
  labs(y="Environmental effect coefficient, posterior distribution", x="")+ggtitle("p(R->M)")+
  theme_minimal()+ 
  theme(legend.position = "none",axis.text.x = element_text( size=8, angle=90))

plot_coeff_pmr<-ggplot()+ 
  geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_coeff[df_coeff$proba=="p(M->R)",],width=0.8,lwd=0.4,
               aes(x=enviro, y=yvalues_coeff, fill=enviro,color=enviro,alpha=type_coeff))+ 
  facet_wrap( ~ individu, ncol=4)+
  
  #scale_color_manual(values=c(""))+
  scale_alpha_manual(values=c(1,0.4)) +
  labs(y="Environmental effect coefficient, posterior distribution", x="")+ggtitle("p(M->R)")+
  theme_minimal()+ 
  theme(legend.position = "none",axis.text.x = element_text( size=8, angle=90))


pdf("effets_enviro_coeff_boxplot_2023.pdf", height = 20, width = 5)
grid.arrange(plot_coeff_prm,plot_coeff_pmr,ncol=1, nrow = 2)
dev.off()


#########################################
###      boxplots des coeff mvt       ###
#########################################

#####prepa du dataframe###############
load("~/Bureau/mtot_list_v5")

replicats=400
df_rf_a1=matrix(0,ncol=16,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(1:16)]) #temp[10001:10020,17:30])
}
df_rf_a1=data.frame(df_rf_a1[-1,])
df_rf_a1=cbind(df_rf_a1,individual=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=replicats))))
rm(temp)
df_rf_mvt=df_rf_a1[order(df_rf_a1$individual),]

param=rep(rep(param_names[1:16], each=replicats),length(numpoiss_vect0))
individu=rep(id_sp,each=replicats*16)

yvalues_param=as.numeric()
for (j in 1:length(id_sp)){
  temp=df_rf_mvt[df_rf_mvt$individual==id_sp[j],]
for (i in 1:16){
  yvalues_param=c(yvalues_param,temp[,i])
}
}
rm(temp)

df_ggplot_mvt=data.frame(individu=individu, param=param, yvalues=yvalues_param)

taille_chr_param=nchar(df_ggplot_mvt$param)
param2=substr(df_ggplot_mvt$param, start=1, stop=taille_chr_param-3)
comp= rep(rep(c(rep(as.factor("R"), replicats),rep(as.factor("M"), replicats)),8),length(id_sp))
df_ggplot_mvt=cbind(df_ggplot_mvt,param2=param2,comp=comp)

#####plot###############
load(file="/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/df_ggplot_mvt_2023")

plot_coeff_mvt<-ggplot()+ 
  #geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_ggplot_mvt,width=0.8,lwd=0.4,
               aes(x=individu, y=yvalues))+ #, fill=enviro,color=enviro))+ 
  facet_wrap( ~ param, ncol=4, scales = "free")+
  
  #scale_color_manual(values=c(""))+
 # scale_alpha_manual(values=c(1,0.4)) +
  # scale_fill_manual(values=c("dodgerblue1", "white", "dodgerblue1green", "grey80"))+
  
  
  labs(y="Movement parameters, posterior distribution", x="")+#ggtitle("p(R->M)")+
  theme_minimal()+ 
  theme(legend.position = "none",axis.text.x = element_text( size=8, angle=90))

pdf("param_mvt_boxplot_2023.pdf", height = 10, width = 10)
plot_coeff_mvt
dev.off()


plot_coeff_mvt2<-ggplot()+ 
  #geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_ggplot_mvt,width=0.4,lwd=0.4,
               aes(x=individu, y=yvalues, color=comp))+ 
  facet_grid(~param2,  scales = "free")+ #ncol=4,
    #scale_color_manual(values=c(""))+
  # scale_alpha_manual(values=c(1,0.4)) +
  # scale_fill_manual(values=c("dodgerblue1", "white", "dodgerblue1green", "grey80"))+
  labs(y="Movement parameters, posterior distribution", x="")+#ggtitle("p(R->M)")+
  theme_minimal()+ 
  theme(legend.position = "none",axis.text.x = element_text( size=8, angle=0))+
  coord_flip()
  
pdf("param_mvt_boxplot_v2_2023.pdf", height = 6, width = 12)
plot_coeff_mvt2
dev.off()

###########plot des intercepts###########

load("~/Bureau/mtot_list_v5")

replicats=400
df_rf_a1=matrix(0,ncol=2,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(17:18)]) #temp[10001:10020,17:30])
}
df_rf_a1=data.frame(df_rf_a1[-1,])
df_rf_a1=cbind(df_rf_a1,individual=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=replicats))))
rm(temp)
df_rf_intercept=df_rf_a1[order(df_rf_a1$individual),]

param=rep(rep(param_names[17:18], each=replicats),length(numpoiss_vect0))
individu=rep(id_sp,each=replicats*2)

yvalues_param=as.numeric()
for (j in 1:length(id_sp)){
  temp=df_rf_intercept[df_rf_intercept$individual==id_sp[j],]
  for (i in 1:2){
    yvalues_param=c(yvalues_param,temp[,i])
  }
}
rm(temp)

df_ggplot_intercept=data.frame(individu=individu, param=param, yvalues=yvalues_param)

taille_chr_param=nchar(df_ggplot_intercept$param)
param2=substr(df_ggplot_intercept$param, start=1, stop=1)
comp=rep(c(rep(as.factor("p(R->M)"), replicats),rep(as.factor("p(M->R)"), replicats)),length(id_sp))
df_ggplot_intercept=cbind(df_ggplot_intercept,param2=param2,comp=comp)

#####plot###############
load(file="/home/lamonica/Documents/poissons/sorties_juillet_23/classif_moinsuncinq_uncinq/df_ggplot_mvt_2023")

plot_coeff_intercept<-ggplot()+ 
  geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_ggplot_intercept,width=0.8,lwd=0.4,
               aes(x=individu, y=yvalues, fill=comp ))+ #color=enviro))+ 
 # facet_wrap( ~ param, ncol=4, scales = "free")+
  
  #scale_color_manual(values=c(""))+
  # scale_alpha_manual(values=c(1,0.4)) +
  # scale_fill_manual(values=c("dodgerblue1", "white", "dodgerblue1green", "grey80"))+
  
  
  labs(y="Intercept, posterior distribution", x="")+ggtitle(expression(paste(alpha)))+
  theme_minimal()+ 
  theme(axis.text.x = element_text( size=8, angle=90))+ 
  guides(fill=guide_legend(""))

pdf("param_intercept_boxplot_2023.pdf", height = 10, width = 10)
plot_coeff_intercept
dev.off()



########################################
#####heatmaps de tous les individus#####
########################################
library(plot3D)

# valeurs extremes rencontres par les individus
varenviro_tous=read.table(file="/home/lamonica/Documents/poissons/sorties_juillet_23/data/min_max_mean_var_enviro_tous_ind.csv"
                          , header=T, sep=",")[,-1]
min_ind=read.table(file="/home/lamonica/Documents/poissons/sorties_juillet_23/data/min_var_enviro_tous_par_ind.csv"
                   , header=T, sep=",")[,-1]
max_ind=read.table(file="/home/lamonica/Documents/poissons/sorties_juillet_23/data/max_var_enviro_tous_par_ind.csv"
                   , header=T, sep=",")[,-1]
mean_ind=read.table(file="/home/lamonica/Documents/poissons/sorties_juillet_23/data/mean_var_enviro_tous_par_ind.csv"
                    , header=T, sep=",")[,-1]

load("~/Bureau/mtot_list_v5")
is.odd <- function(x) x %% 2 != 0
#choix nyct period var nyct_i aube 8, crep 9, jour 10, nuit 11
nyct_i=10

#si on veut la gamme de valeurs rencontrees par le poisson true_env=T
true_env=F

#nombre de cases de cote
size_map=20

#############version un plot par individu##########################

for (i in 1:18){
  temp=mtot_list[[i]]
  vect_a_p12=as.numeric()
  vect_a_p21=as.numeric()
  for (j in 17:length(temp[1,])){
    param_quantile=as.numeric(quantile(temp[,j],probs = 0.5))
    if (is.odd(j)==T) {vect_a_p12=c(vect_a_p12,param_quantile)}else{
      vect_a_p21=c(vect_a_p21,param_quantile)
    }
  }
  mat_coeff=matrix(data=c(vect_a_p12,vect_a_p21), ncol=2, byrow = F)
  rm(temp)
  
  if (true_env==T){
    dftemp=seq(min_ind[i,5],max_ind[i,5],length.out=size_map)
    hmod=seq(min_ind[i,3],max_ind[i,3],length.out=size_map)
    vmod=seq(min_ind[i,1],max_ind[i,1],length.out=size_map)
    
    mu_h=data_pour_standardiser$moy_h
    mu_v=data_pour_standardiser$moy_v
    mu_t=data_pour_standardiser$moy_t
    
    sd_h=data_pour_standardiser$sd_h
    sd_v=data_pour_standardiser$sd_v
    sd_t=data_pour_standardiser$sd_t
    
    }else{
      
      dftemp=seq(-2,2,length.out=size_map)
      hmod=seq(-2,2,length.out=size_map)
      vmod=seq(-2,2,length.out=size_map)
      
      mu_h=0
      mu_v=0
      mu_t=0
      
      sd_h=1
      sd_v=1
      sd_t=1
    }
  

  
  ###########calcul des proba et df################
  mat_ht=mesh(hmod,dftemp)
  p_ht=list()
  df_ht=list()
  for (j in 1:2){
    p_ht[[j]]=with(mat_ht,(exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[4,j] * ((y-mu_t)/sd_t) + mat_coeff[2,j] * ((x-mu_h)/sd_h) + mat_coeff[7,j] * ((y-mu_t)/sd_t)^2 + mat_coeff[5,j] * ((x-mu_h)/sd_h)^2)/
                             (1 +  exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[4,j] * ((y-mu_t)/sd_t) + mat_coeff[2,j] * ((x-mu_h)/sd_h) + mat_coeff[7,j] * ((y-mu_t)/sd_t)^2 + mat_coeff[5,j] * ((x-mu_h)/sd_h)^2)))
                   )
    #x h 2,,5
    #y t 4,,7
    if (j==1){couple_nom="ht_prm"}else{couple_nom="ht_pmr"}
    df_ht[[j]]=data.frame(env_x=as.vector(t(mat_ht[[1]])),env_y=as.vector(t(mat_ht[[2]])),
                          prob=as.vector(t(p_ht[[j]])),couple=as.factor(rep(couple_nom,length(as.vector(t(p_ht[[j]]))))))
  }
  
  
  
  mat_vt=mesh(vmod,dftemp)
  p_vt=list()
  df_vt=list()
  for (j in 1:2){
    p_vt[[j]]=with(mat_vt,(exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[4,j] * ((y-mu_t)/sd_t) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[7,j] * ((y-mu_t)/sd_t)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)/
                             (1 +  exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[4,j] * ((y-mu_t)/sd_t) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[7,j] * ((y-mu_t)/sd_t)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)))
    )
    #x v 3,,6
    #y t 4,,7
    if (j==1){couple_nom="vt_prm"}else{couple_nom="vt_pmr"}
    df_vt[[j]]=data.frame(env_x=as.vector(t(mat_vt[[1]])),env_y=as.vector(t(mat_vt[[2]])),prob=as.vector(t(p_vt[[j]]))
                          ,couple=as.factor(rep(couple_nom,length(as.vector(t(p_vt[[j]]))))))
    
  }
  
  mat_vh=mesh(vmod,hmod)
  p_vh=list()
  df_vh=list()
  for (j in 1:2){
    p_vh[[j]]=with(mat_vh,(exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * ((y-mu_h)/sd_h) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[5,j] * ((y-mu_h)/sd_h)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)/
                             (1 +  exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * ((y-mu_h)/sd_h) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[5,j] * ((y-mu_h)/sd_h)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)))
    )
    #x v 3,,6
    #y h 2,,5
    if (j==1){couple_nom="vh_prm"}else{couple_nom="vh_pmr"}
    df_vh[[j]]=data.frame(env_x=as.vector(t(mat_vh[[1]])),env_y=as.vector(t(mat_vh[[2]])),prob=as.vector(t(p_vh[[j]]))
                          ,couple=as.factor(rep(couple_nom,length(as.vector(t(p_vh[[j]]))))))
  }
  
  #un seul df
  df_heatmap=rbind(df_ht[[1]],df_ht[[2]],df_vt[[1]],df_vt[[2]],df_vh[[1]],df_vh[[2]])
  
  ##plots
  # plot_ht=list()
  # for (j in 1:2){
  # plot_ht[[j]]<-ggplot()+ 
  #   geom_tile(data=df_ht[[j]],aes(x=env_x, y=env_y, fill=zvalues))+ 
  #   theme_minimal()+xlab("Water depth")+ylab("Upstream temperature difference")
  # }
  # plot_vt=list()
  # for (j in 1:2){
  #   plot_vt[[j]]<-ggplot()+ 
  #     geom_tile(data=df_vt[[j]],aes(x=env_x, y=env_y, fill=zvalues))+ 
  #     theme_minimal()+xlab("Flow velocity")+ylab("Upstream temperature difference")
  # }
  # plot_vh=list()
  # for (j in 1:2){
  #   plot_vh[[j]]<-ggplot()+ 
  #     geom_tile(data=df_vh[[j]],aes(x=env_x, y=env_y, fill=zvalues))+ 
  #     theme_minimal()+xlab("Flow velocity")+ylab("Water depth")
  # }
  
  plot_heatmap<-ggplot()+ 
    geom_tile(data=df_heatmap,aes(x=env_x, y=env_y, fill=prob))+
    facet_wrap(~couple, ncol=2)+
    scale_fill_gradient(low="lightgrey", high="black") +
    theme_minimal()
  
  pdf(paste("heatmap_",nyct_i,"_",id_sp[i],".pdf", sep=""), height = 15, width = 10)
  #grid.arrange(plot_ht[[1]],plot_ht[[2]],plot_vt[[1]],plot_vt[[2]],plot_vh[[1]],plot_vh[[2]],ncol=2, nrow = 3)
  print(plot_heatmap)
  dev.off()
  
  
}#i

#############version deux plots prm/pmr que hauteur et prof##########################
df_heatmap=data.frame(individu=as.factor(id_sp[1]), env_x=as.vector(NA),env_y=as.vector(NA),prob=as.vector(NA)
                      ,couple=as.factor("p(R->M)"))

for (i in 1:18){
  temp=mtot_list[[i]]
  vect_a_p12=as.numeric()
  vect_a_p21=as.numeric()
  for (j in 17:length(temp[1,])){
    param_quantile=as.numeric(quantile(temp[,j],probs = 0.5))
    if (is.odd(j)==T) {vect_a_p12=c(vect_a_p12,param_quantile)}else{
      vect_a_p21=c(vect_a_p21,param_quantile)
    }
  }
  mat_coeff=matrix(data=c(vect_a_p12,vect_a_p21), ncol=2, byrow = F)
  rm(temp)
  
  if (true_env==T){
    dftemp=seq(min_ind[i,5],max_ind[i,5],length.out=size_map)
    hmod=seq(min_ind[i,3],max_ind[i,3],length.out=size_map)
    vmod=seq(min_ind[i,1],max_ind[i,1],length.out=size_map)
    
    mu_h=data_pour_standardiser$moy_h
    mu_v=data_pour_standardiser$moy_v
    mu_t=data_pour_standardiser$moy_t
    
    sd_h=data_pour_standardiser$sd_h
    sd_v=data_pour_standardiser$sd_v
    sd_t=data_pour_standardiser$sd_t
    
  }else{
    
    dftemp=seq(-2,2,length.out=size_map)
    hmod=seq(-2,2,length.out=size_map)
    vmod=seq(-2,2,length.out=size_map)
    
    mu_h=0
    mu_v=0
    mu_t=0
    
    sd_h=1
    sd_v=1
    sd_t=1
  }
  
  ###########calcul des proba et df################
  
  mat_vh=mesh(vmod,hmod)
  p_vh=list()
  df_vh=list()
  for (j in 1:2){
    p_vh[[j]]=with(mat_vh,(exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * ((y-mu_h)/sd_h) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[5,j] * ((y-mu_h)/sd_h)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)/
                             (1 +  exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * ((y-mu_h)/sd_h) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[5,j] * ((y-mu_h)/sd_h)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)))
    )
    #x v 3,,6
    #y h 2,,5
    if (j==1){couple_nom="p(R->M)"}else{couple_nom="p(M->R)"}
    df_vh[[j]]=data.frame(individu=as.factor(rep(id_sp[i],length(as.vector(t(p_vh[[j]]))))), env_x=as.vector(t(mat_vh[[1]])),env_y=as.vector(t(mat_vh[[2]])),prob=as.vector(t(p_vh[[j]]))
                          ,couple=as.factor(rep(couple_nom,length(as.vector(t(p_vh[[j]]))))))
  }
  
  #un seul df
  df_heatmap=rbind(df_heatmap,df_vh[[1]],df_vh[[2]])
  #df_heatmap=df_heatmap[-1,]
}#i

if (true_env==F){
  plot_heatmap<-ggplot()+ 
    geom_tile(data=df_heatmap,aes(x=env_x, y=env_y, fill=prob))+
    facet_grid(couple~individu)+
    scale_fill_viridis(discrete=F)+
    labs(y="Water depth", x="Flow velocity")+
    #scale_fill_gradient(low="lightgrey", high="black") +
    scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
    theme_minimal()
  
  pdf(paste("heatmap_vh_valeurs.pdf", sep=""), height = 3, width = 21)
  print(plot_heatmap)
  dev.off()}else{
    
    plot_heatmap=list()
    for (i in 1:18){
      
      if (i==1|i==5|i==9|i==13){plot_heatmap[[i]]<-ggplot()+ 
        geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
        facet_grid(couple~.)+
        scale_fill_viridis(discrete=F,limits=c(0,1))+
        #scale_fill_gradient(low="lightgrey", high="black") +
        scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
        labs(y="Water depth", x="")+ggtitle(id_sp[i])+
        theme_minimal()+theme(legend.position = "none")
      
      }
      
      if (i==15|i==16|i==18){
        plot_heatmap[[i]]<-ggplot()+ 
          geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
          facet_grid(couple~.)+
          scale_fill_viridis(discrete=F,limits=c(0,1))+
          labs(y="", x="Flow velocity")+ggtitle(id_sp[i])+
          #scale_fill_gradient(low="lightgrey", high="black") +
          scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
          theme_minimal()+theme(legend.position = "none")
      }
      if (i==17){
        plot_heatmap[[i]]<-ggplot()+ 
          geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
          facet_grid(couple~.)+
          scale_fill_viridis(discrete=F, limits=c(0,1))+
          labs(y="Water depth", x="Flow velocity")+ggtitle(id_sp[i])+
          #scale_fill_gradient(low="lightgrey", high="black") +
          scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
          theme_minimal()+theme(legend.position = "none")
      }
      
      if (i==2|i==3|i==4|i==6|i==7|i==8|i==10|i==11|i==12|i==14){
        plot_heatmap[[i]]<-ggplot()+ 
          geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
          facet_grid(couple~.)+
          scale_fill_viridis(discrete=F, limits=c(0,1))+
          labs(y="", x="")+ggtitle(id_sp[i])+
          #scale_fill_gradient(low="lightgrey", high="black") +
          scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
          theme_minimal()+theme(legend.position = "none")
      }
    }
    
    plot_heatmap_leg<-ggplot()+ 
      geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
      facet_grid(couple~.)+
      scale_fill_viridis(discrete=F, limits=c(0,1))+
      theme_minimal()
    
    leg <- get_legend(plot_heatmap_leg)
    
    pdf(paste("heatmap_vh_valeurs_rencontrees.pdf", sep=""), height = 15, width = 15)
    grid.arrange(plot_heatmap[[1]],plot_heatmap[[2]],plot_heatmap[[3]],plot_heatmap[[4]],plot_heatmap[[5]],plot_heatmap[[6]], 
                 plot_heatmap[[7]],plot_heatmap[[8]],plot_heatmap[[9]],plot_heatmap[[10]],plot_heatmap[[11]],plot_heatmap[[12]],
                 plot_heatmap[[13]],plot_heatmap[[14]],plot_heatmap[[15]],plot_heatmap[[16]],plot_heatmap[[17]],plot_heatmap[[18]],
                 leg,ncol=4)
    dev.off()
  }


#############version avec que les valeurs rencontrees##########################

data1=read.table(file="/home/lamonica/Documents/poissons/redaction_papier_poisson_deux/leftovers (copie)/data/data_tous_individus_ready_to_use_v4.csv", header=T, sep=",")[,-1]
load("mtot_list_v5")
is.odd <- function(x) x %% 2 != 0
#choix nyct period var nyct_i aube 8, crep 9, jour 10, nuit 11
nyct_i=10

########prepa du df######
df_heatmap=data.frame(individu=as.factor(id_sp[1]), env_x=as.vector(NA),env_y=as.vector(NA),
                      env_x_tv=as.vector(NA),env_y_tv=as.vector(NA), prob=as.vector(NA)
                      ,couple=as.factor("p(R->M)"))
for (i in 1:18){
  
  #les param
  temp=mtot_list[[i]]
  vect_a_p12=as.numeric()
  vect_a_p21=as.numeric()
  for (j in 17:length(temp[1,])){
    param_quantile=as.numeric(quantile(temp[,j],probs = 0.5))
    if (is.odd(j)==T) {vect_a_p12=c(vect_a_p12,param_quantile)}else{
      vect_a_p21=c(vect_a_p21,param_quantile)
    }
  }
  mat_coeff=matrix(data=c(vect_a_p12,vect_a_p21), ncol=2, byrow = F)
  rm(temp)
  
  #l'enviro
  enviro=data1[data1$id_poiss==numpoiss_vect0[i],c(1,9,10)]
  enviro_non_std=data.frame(vmod_tv=enviro$vmod*data_pour_standardiser$sd_v+data_pour_standardiser$moy_v,
                            hmod_tv=enviro$hmod*data_pour_standardiser$sd_h+data_pour_standardiser$moy_h)
  
  ###########calcul des proba et df################
  df_vh=list()
  
  for (j in 1:2){
    
    p_vh1=as.numeric()
    
    for (e in 1:length(enviro[,1])){
      
      p_vh1[e]=exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * enviro[e,3] + mat_coeff[3,j] * enviro[e,2] + mat_coeff[5,j] * enviro[e,3]^2 + mat_coeff[6,j] * enviro[e,2]^2)/
        (1 +  exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * enviro[e,3] + mat_coeff[3,j] * enviro[e,2] + mat_coeff[5,j] * enviro[e,3]^2 + mat_coeff[6,j] * enviro[e,2]^2))
      #x v 3,,6
      #y h 2,,5
    }#e
    
    
    if (j==1){couple_nom="p(R->M)"}else{couple_nom="p(M->R)"}
    df_vh[[j]]=data.frame(individu=as.factor(rep(id_sp[i],length(p_vh1))), env_x=as.vector(enviro$vmod),env_y=as.vector(enviro$hmod),
                          env_x_tv=as.vector(enviro_non_std$vmod_tv),env_y_tv=as.vector(enviro_non_std$hmod_tv),
                          prob=as.vector(p_vh1) ,couple=as.factor(rep(couple_nom,length(p_vh1)))
    )
  }#j
  
  #un seul df
  df_heatmap=rbind(df_heatmap,df_vh[[1]],df_vh[[2]])
  
}#i  


plot_heatmap<-ggplot()+ 
  geom_point(data=df_heatmap,aes(x=env_x_tv, y=env_y_tv, colour=prob), stat="identity")+
  facet_grid(couple~individu)+
  scale_color_viridis(limits=c(0,1))+
  labs(y="Water depth", x="Flow velocity")+
 # scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
  theme_minimal()

pdf(paste("heatmap_vh_valeurs_rencontrees_tv.pdf", sep=""), height = 3, width = 21)
print(plot_heatmap)
dev.off()


