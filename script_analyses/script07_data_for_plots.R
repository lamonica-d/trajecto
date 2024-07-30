#######################################################################################
#######################################################################################
# Script 7 - Generation of dataframes for plots
#######################################################################################
#######################################################################################

## load and set data
load("outputs/tables_list")
load("outputs/clust_list")
load("outputs/gp_list_list")
load("outputs/nb_gp_list")
load("outputs/matrice_curves_list")
load("outputs/matrice_proba_curves_list")
load("outputs/mtot_list")

param_names=colnames(mtot_list[[1]])
index_lin=3:8
index_carre=9:14
replicats=200
nb_classe=c(4,2,4,2,2,3)
xvalues_plot <- xvalues <- seq(-1.5,1.5,0.2)

numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)
id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")
vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)

varenviroxproba1=c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                   "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)")

## load means and sds used to standardize environmental covariables 
## to navigate between standardized and true values
load("data/raw-data/data_pr_std")
data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)

## dataframe to plot figures with median probabilities
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
    truc=which(table2[i,2:(length(nb_gp)+1)]==max(table2[i,2:(length(nb_gp)+1)]))
    if (length(truc)>1) {truc=truc[1]}
    groupes_vect1=c(groupes_vect1,truc)
  }
}
groupes_vect=as.factor(unlist(lapply(X=groupes_vect1, FUN=rep, times=length(xvalues_plot))))


varenviroxproba=as.factor(rep(unlist(lapply(X=varenviroxproba1, FUN=rep, times=length(xvalues_plot))),length(id_sp)))

df_mediane=data.frame(individu=individu, species=species, group=groupes_vect, j=varenviroxproba, 
                      xvalues=xvalues_df, yvalues=yvalues_mediane)

sd_vect2=rep(sd_vect, each=2)
moy_vect2=rep(moy_vect, each=2)

df_mediane=cbind(df_mediane,xtruevalues=rep(0,length(df_mediane$individu)))

for (i in 1:length(varenviroxproba1)){
  df_mediane[df_mediane$j==varenviroxproba1[i],]$xtruevalues= df_mediane[df_mediane$j==varenviroxproba1[i],]$xvalues*sd_vect2[i]+moy_vect2[i]
}

size=as.factor(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3)))
df_mediane2=data.frame(df_mediane, size=size)
size_class=ifelse(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3))>600,3,
                  ifelse(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3))>=400,2,1))
df_mediane3=data.frame(df_mediane, size_class=as.factor(size_class))
levels(df_mediane3$size_class)=c("< 40 cm", "40 - 60 cm", "60 cm <")

save(df_mediane3, file = "outputs/df_ggplot_mediane")

## dataframe to plot all curves (probability functions and derivatives)
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


varenviroxproba=as.factor(unlist(lapply(X=varenviroxproba1, FUN=rep, times=length(xvalues_plot)*replicats*length(id_sp))))

df_replicats=data.frame(individu=individu, species=species, replicats=replicats_vect, group=groupes_vect, j=varenviroxproba, 
                        xvalues=xvalues_df, yvalues_proba=yvalues_replicats_proba, yvalues_derivee=yvalues_replicats_derivee)


df_replicats=cbind(df_replicats,xtruevalues=rep(0,length(df_replicats$individu)))

for (i in 1:length(varenviroxproba1)){
  df_replicats[df_replicats$j==varenviroxproba1[i],]$xtruevalues= df_replicats[df_replicats$j==varenviroxproba1[i],]$xvalues*sd_vect2[i]+moy_vect2[i]
}

save(df_replicats,file = "outputs/df_ggplot_replicats")

## dataframe with credibility intervals of all curves
xvalues=unique(df_replicats$xvalues)
xtruevalues=unique(df_replicats$xtruevalues)

df_ic=data.frame(df_replicats[1,c(1,5,6,9)],ic_inf=0, ic_sup=0) 

for (i in 1:length(id_sp)){
  
  temp1=df_replicats[df_replicats$individu==id_sp[i],]
  
  for (k in 1:6){
    
    temp2=temp1[temp1$j==varenviroxproba1[k],]
    
    for (x in 1:length(xvalues)){
      
      temp3=temp2[temp2$xvalues==xvalues[x],]
      
      ic_inf=quantile(temp3$yvalues_proba,probs=0.025)
      ic_sup=quantile(temp3$yvalues_proba,probs=0.975)
      
      df_ic=rbind(df_ic,data.frame(individu=id_sp[i],j=as.factor(varenviroxproba1[k]),xvalues=xvalues[x],xtruevalues=xtruevalues[x],ic_inf=ic_inf, ic_sup=ic_sup))
      
    }
  }
}
df_ic=df_ic[-1,]
save(df_ic, file = "outputs/df_ggplot_ic")

## dataframe of nycthemeral coefficients
replicats=400
df_rf_a1=matrix(0,ncol=8,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  #aube 31:32
  #crepuscule 33:34
  #jour 35:36
  #nuit 37:38
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(31:38)])
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

df_coeff_nyct=rbind(df1,df2)  
save(df_coeff_nyct, file = "outputs/df_ggplot_coeff_nyct")

## dataframe of movement coefficients
df_rf_a1=matrix(0,ncol=16,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(1:16)])
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

save(df_ggplot_mvt, file = "outputs/df_ggplot_coeff_mvt")

## dataframe of environmental covariables coefficients
replicats=400
df_rf_a1=matrix(0,ncol=12,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(19:30)])
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

df_coeff_enviro=rbind(df1,df2)  

save(df_coeff_enviro, file = "outputs/df_ggplot_coeff_enviro")

## dataframe of intercepts
replicats=400
df_rf_a1=matrix(0,ncol=2,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(17:18)]) 
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

save(df_ggplot_intercept, file = "outputs/df_ggplot_intercept")

