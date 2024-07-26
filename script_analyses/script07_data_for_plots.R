#######################################################################################
#######################################################################################
# Script 7 - Generation of dataframes for plots
#######################################################################################
#######################################################################################

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

####gestion des sd et moy pour naviguer entre varenviro standardisees et vraies valeurs
load("data/raw-data/data_pr_std")

data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)

numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)
nb_classe=c(4,2,4,2,2,3)
xvalues=seq(-1.5,1.5,0.2)

##########################
#####plots medianes ######
##########################
##dataframe pour plot des medianes################

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


size=as.factor(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3)))
df_mediane2=data.frame(df_mediane, size=size)
size_class=ifelse(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3))>600,3,
                  ifelse(unlist(lapply(X=vect_tailles, FUN=rep, times=length(xvalues_plot)*2*3))>=400,2,1))
df_mediane3=data.frame(df_mediane, size_class=as.factor(size_class))
levels(df_mediane3$size_class)=c("< 40 cm", "40 - 60 cm", "60 cm <")

save(df_mediane3, file = "outputs/df_ggplot_mediane")

########dataframe pour plot des replicats###############
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

save(df_replicats,file = "outputs/df_ggplot_replicats")


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

save(df_ic, file = "outputs/df_ggplot_ic")


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

df_coeff_nyct=rbind(df1,df2)  

save(df_coeff_nyct, file = "outputs/df_ggplot_coeff_nyct")


#replicats=400
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

save(df_ggplot_mvt, file = "outputs/df_ggplot_coeff_mvt")


replicats=400
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

df_coeff_enviro=rbind(df1,df2)  

save(df_coeff_enviro, file = "outputs/df_ggplot_coeff_enviro")

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

save(df_ggplot_intercept, file = "outputs/df_ggplot_intercept")


# valeurs extremes rencontres par les individus
varenviro_tous=read.table(file="data/raw-data/min_max_mean_var_enviro_tous_ind.csv"
                          , header=T, sep=",")[,-1]
min_ind=read.table(file="data/raw-data/min_var_enviro_tous_par_ind.csv"
                   , header=T, sep=",")[,-1]
max_ind=read.table(file="data/raw-data/max_var_enviro_tous_par_ind.csv"
                   , header=T, sep=",")[,-1]
mean_ind=read.table(file="data/raw-data/mean_var_enviro_tous_par_ind.csv"
                    , header=T, sep=",")[,-1]

#choix nyct period var nyct_i aube 8, crep 9, jour 10, nuit 11
nyct_i=10

#si on veut la gamme de valeurs rencontrees par le poisson true_env=T
true_env=F

#nombre de cases de cote
size_map=20
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

save(df_heatmap, file = "outputs/df_ggplot_heatmap_all_values")


#############version avec que les valeurs rencontrees##########################

data1=read.table(file="data/derived-data/data_tous_individus_ready_to_use_v4.csv", header=T, sep=",")[,-1]

#choix nyct period var nyct_i aube 8, crep 9, jour 10, nuit 11
nyct_i=10

########prepa du df######
df_heatmap2=data.frame(individu=as.factor(id_sp[1]), env_x=as.vector(NA),env_y=as.vector(NA),
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
  df_heatmap2=rbind(df_heatmap2,df_vh[[1]],df_vh[[2]])
  
}#i  

save(df_heatmap2, file = "outputs/df_ggplot_heatmap_encountered_values")


