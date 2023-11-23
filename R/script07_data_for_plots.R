#######################################################################################
#######################################################################################
# Script 7 - Generation of dataframes for plots
#######################################################################################
#######################################################################################

make_df_for_plot <- function(){
  
  load("outputs/tables_list")
  load("outputs/gp_list_list")
  load("outputs/nb_gp_list")
  
  load("outputs/matrice_curves_list")
  load("outputs/matrice_proba_curves_list")
  
  load("outputs/mtot_list_v5")
  
####gestion des sd et moy pour naviguer entre varenviro standardisees et vraies valeurs
load("data/raw-data/data_pr_std")
  
data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)


##########################
#####plots medianes ######
##########################
##dataframe pour plot des medianes################
xvalues=seq(-1.5,1.5,0.2)

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

save("outputs/df_ggplot_mediane")



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

save("outputs/df_ggplot_replicats")


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

save("outputs/df_ggplot_coeff")


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

save("outputs/df_ggplot_mvt")


}
