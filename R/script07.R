


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
