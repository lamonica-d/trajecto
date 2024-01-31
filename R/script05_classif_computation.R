
#######################################################################################
#######################################################################################
# Script 5 - Computation for classification
#######################################################################################
#######################################################################################

classif_computation <- function(){
  
  numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)
  
  id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
          "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
          "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")
  
  vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)
  
  load("outputs/mtot_list_v5")
param_names=colnames(mtot_list[[1]])

#############la classif
library(cluster)
library(dendextend)
is.odd <- function(x) x %% 2 != 0
library(geiger)

f_derivee <- function(a,b,c,x, mu, sd){ ((b*sd + 2*c*(x-mu))*exp(a + ((x-mu)*(b*sd + c*(x-mu)))/sd^2))/(sd^2*(exp(a+((x-mu)*(b*sd-c*mu+c*x))/sd^2)+1)^2)}

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

for (j in 1:6){
  diff_surface=diff_surface_list[[j]]
  truc_coeff=as.dist(diff_surface)
  clust_coeff=hclust(d=truc_coeff, method="ward.D2")
  clust_list[[j]]=clust_coeff
}

save("outputs/df_rf_a")
save("outputs/diff_surface_list")
save("outputs/matrice_curves_list")
save("outputs/matrice_proba_curves_list")
save("outputs/clust_list")


}
