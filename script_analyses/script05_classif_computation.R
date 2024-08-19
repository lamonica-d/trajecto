#######################################################################################
#######################################################################################
# Script 5 - Computation for classification
#######################################################################################
#######################################################################################

library(cluster)
library(dendextend)
library(geiger)

## set and load data
numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

load("outputs/mtot_list")

param_names=colnames(mtot_list[[1]])

## set seed for repro
set.seed(3456)

## set the sample size
replicats=200

## sample parameter values in posterior distributions for each individual
df_rf_a1=matrix(0,ncol=14,nrow=1)
for (i in 1:length(numpoiss_vect0)){  
  temp=mtot_list[[i]]
  print(numpoiss_vect0[i])
  
  #jour 33:34
  #nuit 35:36
  
  df_rf_a1=rbind(df_rf_a1, temp[runif(replicats,min=1, max=1500),c(17:28,33,34)]) 
}
df_rf_a1=data.frame(df_rf_a1[-1,])
df_rf_a1=cbind(df_rf_a1,individual=as.factor(unlist(lapply(X=id_sp, FUN=rep, times=replicats))))
rm(temp)
df_rf_a=df_rf_a1[order(df_rf_a1$individual),]
rm(df_rf_a1)

## for each probability (RM or MR) x enviro covariable (temp, depth, flow) x individual x parameters sample
## computation of probability function
index_lin=1:6
index_carre=7:12
xvalues=seq(-1.5,1.5,0.2)
mu=0
sd=1

matrice_proba_curves_list=list()
for (j in 1:6){
  
  matrice_proba_curves=matrix(0,ncol=length(xvalues),nrow=replicats*18)
  
  for (i in 1:(replicats*18)){
    
    if (is.odd(j)==T) {calcul_proba <- function (x) {exp(df_rf_a[i,13] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2)/ 
        (1 +  exp(df_rf_a[i,13] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2))}
    }
    
    if (is.odd(j)==F) {calcul_proba <- function (x) {exp(df_rf_a[i,14] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2)/ 
        (1 +  exp(df_rf_a[i,14] + df_rf_a[i,index_lin[j]] * ((x-mu)/sd) + df_rf_a[i,index_carre[j]] * ((x-mu)/sd)^2))}
    }
    
    matrice_proba_curves[i,]=calcul_proba(xvalues)
  }
  
  matrice_proba_curves_list[[j]]=matrice_proba_curves
  
}

## for each probability (RM or MR) x enviro covariable (temp, depth, flow) x individual x parameters sample
## computation of derivative
matrice_curves_list=list()
for (j in 1:6){
  
  matrice_curves=matrix(0,ncol=length(xvalues),nrow=replicats*18)
  
  for (i in 1:(replicats*18)){
    
    if (is.odd(j)==T) {matrice_curves[i,]= f_derivee(a=df_rf_a[i,13], b=df_rf_a[i,index_lin[j]],
                                                     c=df_rf_a[i,index_carre[j]], x=xvalues, mu=mu, sd=sd)
    }
    
    if (is.odd(j)==F) {matrice_curves[i,]= f_derivee(a=df_rf_a[i,14], b=df_rf_a[i,index_lin[j]],
                                                     c=df_rf_a[i,index_carre[j]], x=xvalues, mu=mu, sd=sd)
    }
    
  }
  matrice_curves_list[[j]]=matrice_curves
}

## for each probability (RM or MR) x enviro covariable (temp, depth, flow) x individual x parameters sample
## computation of aera differences of derivative two by two 
diff_surface_list=list()  
for (j in 1:6){
  matrice_curves= matrice_curves_list[[j]]
  diff_surface=matrix(0,ncol=replicats*18,nrow=replicats*18)
  
  for (i in 1:(replicats*18)){
    for (k in 1:(replicats*18)){
      
      diff_surface[i,k]<-geiger:::.area.between.curves(xvalues, pmax(matrice_curves[i,], matrice_curves[k,]), 
                                                       pmin(matrice_curves[i,], matrice_curves[k,])
                                                       ,xrange = c(-1.5,1.5))      
    }
  }
  
  diff_surface_list[[j]]= diff_surface 
}

## classification 
clust_list=list()
for (j in 1:6){
  diff_surface=diff_surface_list[[j]]
  truc_coeff=as.dist(diff_surface)
  clust_coeff=hclust(d=truc_coeff, method="ward.D2")
  clust_list[[j]]=clust_coeff
}

## save all
save(df_rf_a, file = "outputs/df_rf_a")
save(diff_surface_list, file = "outputs/diff_surface_list")
save(matrice_curves_list, file = "outputs/matrice_curves_list")
save(matrice_proba_curves_list, file = "outputs/matrice_proba_curves_list")
save(clust_list, file = "outputs/clust_list")

