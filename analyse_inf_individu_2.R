rm(list=ls())
library(rjags)
library(viridis)
#library(truncnorm)
library(ggplot2)

#c'est le bordel, mais ça genere la list des posteriors


#########pre check#####################
#baf
#numpoiss_vect0=c(3100,3156,3744,3128,3170,3051,3317,3674,3828)
#sil
#numpoiss_vect0=c(3856,3632,3786,3835,3849,3870,3772,3800,3387,3415,3429)
#che
#numpoiss_vect0=c(3183,3212,3240,3352,3464,3730,3758,3121,3394,3408,3422,3506,3562,3590,3625,3079,3303)
#numpoiss_vect0=c(3183,3212,3240,3464,3730,3758,3121,3394,3408,3422,3506,3562,3590,3625,3079,3303)

numpoiss_vect0=c(3100,3744,3128,3170,
                 3856,3632,3786,3835,3849,3870,
                 3183,3212,3240,3352,3464,3730,3758,3121)


####par espece
#m_list=list()
mtot_list=list()
for (j in 1:length(numpoiss_vect0)){
 # load(paste("~/Bureau/decembre2020/test_chevesnes_dec20/image_inf_individual_che_test_",
 #            numpoiss_vect0[j],"_v6.Rdata", sep=""))

  # load(paste("~/Bureau/decembre2020/test_silures_dec20/image_inf_individual_sil_test_",
  #            numpoiss_vect0[j],"_v6.Rdata", sep=""))
  # 
  #load(paste("~/Bureau/decembre2020/test_barbeaux_dec20/image_inf_individual_baf_test_",
   #          numpoiss_vect0[j],"_v6.Rdata", sep=""))
  
 # load(paste("/home/lamonica/Documents/poissons/redaction_papier_poisson_deux/leftovers (copie)/inference/image/image_inf_individual_",numpoiss_vect0[j],"_v9.Rdata", sep=""))
  
  load(paste("/home/lamonica/Documents/poissons/sorties_juillet_23/image_inf_individual_",numpoiss_vect0[j],"_v10.Rdata", sep=""))
  
  # 
#m_list[[j]]=jags_res
 # print(numpoiss_vect0[j])
 # print(gelman.diag(jags_res))
#  print(fin-debut)
  plot(jags_res, trace=F, density=T, main=numpoiss_vect0[j])
  #print(autocorr.diag(jags_res[[1]]))
mtot_list[[j]]=rbind(jags_res[[1]],jags_res[[2]],jags_res[[3]])
}

save(mtot_list, file="mtot_list_v4")
###resume des statistiques
#(M.su=summary(m))

###echantillon et distributions a posteriori
#plot(m, trace=T, density=TRUE)

######tous les individus##########

numpoiss_vect0=c(3100,3156,3744,3128,3170,3051,3317,3674,3828,3856,3632,3786,3835,3849,3870,3772,3800,3387,3415,3429,
                     3183,3212,3240,3352,3464,3730,3758,3121,3394,3408,3422,3506,3562,3590,3625,3079,3303)

vect_tailles=c( 397, 336 ,477 ,311 ,302, 176 ,183 ,480 ,496, #baf
  1001,  720,  620,  496,  367, 1050,  780, 1360,  267,  303,  350, #sil
  486, 313, 325, 248, 
  472, 454, 462 ,455, 169 ,281 ,301 ,435, 437, 346, 182, 207, 152 #che
  )

nbtraj=c(508,337,1095,533,963,83,73,91,75
         ,731,278,210,965,301,514,22,40,14,14,52
         ,284,1296,1462,1089,312,852,339,162,125,84,28,130,52,22,13,16,52)

nbloc=c(16991,5668,43580,18809,28534,1032,1022,3797,1965,
        16565,5986,3937,26129,5636,13923,234,540,223,227,955,
        4440,29530,32066,22852,8909,24146,7334,4617,1526,1952,346,2810,1290,309,239,251,1656)

id_sp_tous=c("B-3100", "B-3156", "B-3744", "B-3128", "B-3170", "B-3051","B-3317","B-3674","B-3828",
             "S-3856", "S-3632", "S-3786", "S-3835", "S-3849", "S-3870", "S-3772","S-3800","S-3387","S-3415","S-3429",
             "C-3183" ,"C-3212" ,"C-3240" ,"C-3352" ,"C-3464" ,"C-3730" ,"C-3758" ,"C-3121" ,"C-3394", "C-3408" ,"C-3422"
             ,"C-3506", "C-3562","C-3590","C-3625","C-3079","C-3303")#

id_sp_bienloc=id_sp_tous[nbloc/60>36]
numpoiss_bienloc=numpoiss_vect0[nbloc/60>36]
nbloc_bienloc=nbloc[nbloc/60>36]
vect_tailles_bienloc=vect_tailles[nbloc/60>36]

data_ind=data.frame(id=id_sp_bienloc, nbloc=nbloc[nbloc/60>36],size=vect_tailles[nbloc/60>36])
data_ind2=data_ind[order(data_ind$id),]

index_numpoiss=c(1,4,2,5,6,3, 20,13,14,15,16,17,21,18,19, 8,9,10,11,7,12)



########### code de la liste des samples ##############
#m_list=list()
mtot_list=list()
for (j in 1:length(numpoiss_vect0)){

  load(paste("~/Bureau/decembre2020/tous_individus_dec20/image_inf_individual_",
             numpoiss_vect0[j],"_v7.Rdata", sep=""))
  
  #m_list[[j]]=jags_res
  #  print(numpoiss_vect0[j])
  ##  print(gelman.diag(jags_res))
  # print(fin-debut)
 # plot(jags_res, trace=T, density=F)
  mtot_list[[j]]=rbind(jags_res[[1]],jags_res[[2]],jags_res[[3]])
}
####################################################
load("~/Bureau/mtot_list")
param_names=colnames(mtot_list[[1]])

machin=mtot_list[[3]]
pdf("pairs_coeff_matrice.pdf", height = 15, width = 15)
ggpairs(data.frame(machin[runif(1000,min=1, max=length(machin[,1])),c(17:36)]))
dev.off()

############################### DATFRAME POUR GGPLOT ######################################

param=unlist(lapply(X=param_names, FUN=rep, times=1000*length(numpoiss_vect0)))
species=rep(c(rep("BAF",9*1000), rep("SIL",11*1000), rep("CHE",16*1000)),length(param_names))
individual=rep(unlist(lapply(X=numpoiss_vect0, FUN=rep, times=1000)),length(param_names))
size=unlist(lapply(X=vect_tailles, FUN=rep, times=1000*length(param_names)))
value=as.numeric()
for (i in 1:length(param_names)){

  for (j in 1:length(numpoiss_vect0)){
  
     temp=mtot_list[[j]]
     value=c(value,temp[(length(temp[,i])-999):length(temp[,i]),i])
 }
 
}

ggdf=data.frame(param=as.factor(param), species=as.factor(species), individual=as.factor(individual), size=size, value=value)

pdf("ggboxplot.pdf", 20, 20)
ggplot()+ #, fill=ref))+
  #geom_abline(slope=0, intercept=intercept[i], colour="grey45", lty=2, lwd=1.2)+
  geom_boxplot(data=ggdf, aes(y=value, x=individual, fill=species))+ 
  facet_wrap(. ~ param, ncol=6, scales='free_x')+#
  labs(y="Parameter value", x="Individual")+ 
  #scale_fill_viridis(discrete=TRUE) +
  #ggtitle(parameters_names[j])+
  theme_minimal() +coord_flip()#+ 
  #theme(legend.position = "none")
dev.off()

# test <- ggplot()+
#   geom_boxplot(data=ggdf[ggdf$param=="a[1,1]",], aes(y=value, x=individual, fill=species))+ 
#  # facet_wrap(. ~ param, ncol=6, scales='free_y')+#
#   labs(y="Parameter value", x="Individual")+ 
#   #scale_fill_viridis(discrete=TRUE) +
#   #ggtitle(parameters_names[j])+
#   theme_minimal() +
#   theme(legend.position = "none")


####################################################################################


couleurs=viridis(length(numpoiss_vect0))
min_vect=as.numeric()
max_vect=as.numeric()
for (i in 1:36){
  max_temp=as.numeric()
  min_temp=as.numeric()
  for (j in 1:length(numpoiss_vect0)){
    max_temp[j]=max(mtot_list[[j]][,i])
    min_temp[j]=min(mtot_list[[j]][,i])
  }
  max_vect[i]=max(max_temp)  
  min_vect[i]=min(min_temp)  
}


######################### FIGURES DENSITES ####################################

pdf("parametres_densites_silures.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  plot(density(temp[,i]), lwd=2, col=couleurs[1],main=colnames(mtot_list[[1]])[i]
       ,xlim=c(min_vect[i],max_vect[i]),xlab="", cex.axis=1.5, ylab=""
  )
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    lty_v=ifelse(j<=7,1,2)
    lines(density(temp[,i]), lwd=2, col=couleurs[j], lty=lty_v,xlab="")
  }
  if (i>=17){lines(density(rnorm(1000, mean=0,sd=1)), lty=1, lwd=1.5, col="darkgrey")}
  #if (i==1|i==2|i==7|i==8|i==11|i==12){lines(density(rgamma(100, 0.01, 1/0.01)), lty=1,lwd=1.5, col="darkgrey")}
  if (i==3|i==4|i==11|i==12){lines(density(rgamma(1000, 0.01, 1/0.01)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==7|i==8){lines(density(rgamma(1000,1, 1)), lty=1, lwd=1.5, col="darkgrey")}

  if (i==2|(i>=13 & i<=16)|i==9){lines(density(rgamma(1000, 3, 1)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==1|i==10){lines(density(rgamma(1000, 0.5, 1/2)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==5){lines(density(rnorm(1000, mean=pi,sd=0.5)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==6){lines(density(rtruncnorm(1000, mean=0,sd=0.5, a=0)), lty=1, lwd=1.5, col="darkgrey")}#lines(density(runif(100, 0,4)), lty=2, lwd=1, col="grey")}

  if (i%%4==0){legend("topright", legend=c(numpoiss_vect0,"prior"), col=c(couleurs,"darkgrey"),
                      lty=c(rep(1,6), rep(2,5),1), lwd=2,bty = "n")}

}

dev.off()

ymax_vect=c(32,18,300,140,18,15,35,35,30,35,90,60,28,18,4,25,
            5,5,8,8,6.5,7,13,12,8,7,18,7,9,13,rep(20,5),22
            #rep(10,20)
            )
pdf("parametres_densites_chevesnes.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  plot(density(temp[,i]), lwd=2, col=couleurs[1],main=colnames(mtot_list[[1]])[i]
       ,xlim=c(min_vect[i],max_vect[i]),xlab="", cex.axis=1.5, ylab="",ylim=c(0,ymax_vect[i])
  )
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    lty_v=ifelse(j<=6,1,2)
    lines(density(temp[,i]), lwd=2, col=couleurs[j], lty=lty_v,xlab="",ylim=c(0,ymax_vect[i]))
  }
  if (i>=17){lines(density(rnorm(1000, mean=0,sd=1)), lty=1, lwd=1.5, col="darkgrey")}
  #if (i==1|i==2|i==7|i==8|i==11|i==12){lines(density(rgamma(100, 0.01, 1/0.01)), lty=1,lwd=1.5, col="darkgrey")}
  if (i==3|i==4|i==11|i==12){lines(density(rgamma(1000, 0.01, 1/0.01)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==7|i==8){lines(density(rgamma(1000,1, 1)), lty=1, lwd=1.5, col="darkgrey")}
  
  if (i==2|(i>=13 & i<=16)|i==9){lines(density(rgamma(1000, 3, 1)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==1|i==10){lines(density(rgamma(1000, 0.5, 1/2)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==5){lines(density(rnorm(1000, mean=pi,sd=0.5)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==6){lines(density(rtruncnorm(1000, mean=0,sd=0.5, a=0)), lty=1, lwd=1.5, col="darkgrey")}#lines(density(runif(100, 0,4)), lty=2, lwd=1, col="grey")}
  
  if (i%%4==0){legend("topright", legend=c(numpoiss_vect0,"prior"), col=c(couleurs,"darkgrey"), 
                      lty=c(rep(1,6), rep(2,10),1), lwd=2,bty = "n")}
  
}

dev.off()

pdf("parametres_densites_barbeaux.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  plot(density(temp[,i]), lwd=2, col=couleurs[1],main=colnames(mtot_list[[1]])[i]
       ,xlim=c(min_vect[i],max_vect[i]),xlab="", cex.axis=1.5, ylab=""#,ylim=c(0,ymax_vect[i])
  )
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    lty_v=ifelse(j<=4,1,2)
    lines(density(temp[,i]), lwd=2, col=couleurs[j], lty=lty_v,xlab="")
  }
  if (i>=17){lines(density(rnorm(1000, mean=0,sd=1)), lty=1, lwd=1.5, col="darkgrey")}
  #if (i==1|i==2|i==7|i==8|i==11|i==12){lines(density(rgamma(100, 0.01, 1/0.01)), lty=1,lwd=1.5, col="darkgrey")}
  if (i==3|i==4|i==11|i==12){lines(density(rgamma(1000, 0.01, 1/0.01)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==7|i==8){lines(density(rgamma(1000,1, 1)), lty=1, lwd=1.5, col="darkgrey")}
  
  if (i==2|(i>=13 & i<=16)|i==9){lines(density(rgamma(1000, 3, 1)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==1|i==10){lines(density(rgamma(1000, 0.5, 1/2)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==5){lines(density(rnorm(1000, mean=pi,sd=0.5)), lty=1, lwd=1.5, col="darkgrey")}
  if (i==6){lines(density(rtruncnorm(1000, mean=0,sd=0.5, a=0)), lty=1, lwd=1.5, col="darkgrey")}#lines(density(runif(100, 0,4)), lty=2, lwd=1, col="grey")}
  
  if (i%%4==0){legend("topright", legend=c(numpoiss_vect0,"prior"), col=c(couleurs,"darkgrey"), 
                      lty=c(rep(1,4), rep(2,5),1), lwd=2,bty = "n")}
  
}

dev.off()

######################### FIGURES BOXPLOTS ####################################

pdf("parametres_boxplot_chevesnes.pdf", 14, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
    boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),
          main=colnames(mtot_list[[1]])[i],xaxt="n", las=1, col=couleurs[1])
  axis(side=1, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=0.8)
  if (i>=17){abline(h=0,col="darkgrey", lwd=2, lty=2 )}
 for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
       boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=couleurs[j])
  }
}

dev.off()

pdf("parametres_boxplot_silures.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),
          main=colnames(mtot_list[[1]])[i],xaxt="n", las=1, col=couleurs[1])
  axis(side=1, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=0.8)
  if (i>=17){abline(h=0,col="darkgrey", lwd=2, lty=2 )}
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=couleurs[j])
  }
}

dev.off()


pdf("parametres_boxplot_barbeaux.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),
          main=colnames(mtot_list[[1]])[i],xaxt="n", las=1, col=couleurs[1])
  axis(side=1, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=0.8)
  if (i>=17){abline(h=0,col="darkgrey", lwd=2, lty=2 )}
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=couleurs[j])
  }
}

dev.off()

###horizontal

pdf("parametres_boxplot_chevesnes.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),horizontal = T,
          main=colnames(mtot_list[[1]])[i],las=1, col=couleurs[1])
  axis(side=2, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=1, las=1)
  if (i>=17){abline(v=0,col="darkgrey", lwd=2, lty=2 )}
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=couleurs[j],horizontal = T)
  }
}

dev.off()

pdf("parametres_boxplot_silures.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),horizontal = T,
          main=colnames(mtot_list[[1]])[i],las=1, col=couleurs[1])
  axis(side=2, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=1, las=1)
  if (i>=17){abline(v=0,col="darkgrey", lwd=2, lty=2 )}
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=couleurs[j],horizontal = T)
  }
}

dev.off()


pdf("parametres_boxplot_barbeaux.pdf", 10, 10)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),horizontal = T,
          main=colnames(mtot_list[[1]])[i],las=1, col=couleurs[1])
  axis(side=2, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=1, las=1)
  if (i>=17){abline(v=0,col="darkgrey", lwd=2, lty=2 )}
  for (j in 2:length(numpoiss_vect0)){
    temp=mtot_list[[j]]
    boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=couleurs[j],horizontal = T)
  }
}

dev.off()

###tous les individus
#col_espece=viridis(3)
col_espece=c("#00BA38", "#F8766D","#619CFF")
titles=c("mean speed R k[1]" , "mean speed M k[2]" ,"speed shape R inv_lambda[1]", "speed shape M inv_lambda[2]",
         "mean turning angle R phi[1]","mean turning angle M phi[2]","turning angle precision R rho[1]","turning angle precision M rho[2]",     
         "mean U2 R mu[1]","mean U2 M mu[2]","precision U2 R tau[1]" ,"precision U2 M tau[2]",
         "U1 R alpha[1]","U1 M alpha[2]","U1 R beta[1]","U1 M beta[2]",   
         "Intercept RM a[1,1]","Intercept MR a[2,1]","Hauteur RM a[1,2]" ,"Hauteur MR a[2,2]","Vitesse RM a[1,3]","Vitesse MR a[2,3]",
         "Température RM a[1,4]","Température MR a[2,4]" ,"Vitesse max RM a[1,5]","Vitesse max MR a[2,5]",
         "Coeff variation vitesse RM a[1,6]","Coeff variation vitesse MR a[2,6]", "Nb heures exondées RM a[1,7]",
         "Nb heures exondées MR a[2,7]","Hauteur carré RM a[1,8]","Hauteur carré MR a[2,8]", "Vitesse carré RM a[1,9]",
         "Vitesse carré MR a[2,9]","Température carré RM a[1,10]","Température carré MR a[2,10]"      
         )

pdf("parametres_boxplot_tous_individus2.pdf", 15, 20)

par(mfrow=c(2,2))
for (i in 1:36){
  temp=mtot_list[[1]]
  boxplot(temp[,i], at = 1, ylim=c(min_vect[i],max_vect[i]), xlim=c(1, length(numpoiss_vect0)),horizontal = T,
          main=titles[i] #colnames(mtot_list[[1]])[i]
          ,las=1, col=col_espece[1]          )
  axis(side=2, labels=paste(numpoiss_vect0), at = 1:length(numpoiss_vect0), cex.axis=1, las=1)
  legend("bottomright", legend=c("Barbeaux", "Silures", "Chevesnes"), col=col_espece, lwd=4,bty = "n")
  if (i>=17){abline(v=0,col="darkgrey", lwd=2, lty=2 )}
  for (j in 2:length(numpoiss_vect0)){
    col_temp<-ifelse(j<=9,col_espece[1], ifelse(j>20, col_espece[3], col_espece[2]))
    temp=mtot_list[[j]]
    boxplot(temp[,i], at=j, add=T, xlab="", ylab="", yaxt="n" ,xaxt="n", axes=F,col=col_temp,horizontal = T)
  }
}

dev.off()


########### medianes des coeffs vs. taille ind
col_espece=c("#00BA38", "#F8766D","#619CFF")  #viridis(3)
vect_main=c('Intercept',NA,'Hauteur',NA,'Vitesse',NA,'Température',NA,'Vitesse max',NA,'Coeff variation vitesse',
            NA,"Nombre d'heures exondées", NA,'Hauteur carré',NA,'Vitesse carré',NA,'Température carré')

pdf("coeff_vs_taille.pdf", 10, 10)

par(mfrow=c(2,1))

for (i in seq(17,35,2)){
 
  temp=mtot_list[[1]]
  plot(vect_tailles[1], median(temp[,i]), ylim=c(-4,4), xlim=c(min(vect_tailles),max(vect_tailles)),
      las=1, col=col_espece[1], pch=1, xlab="Taille (mm)", ylab="", cex=2)
  points(vect_tailles[1], median(temp[,i+1]), col=col_espece[1], pch=16, cex=2)
  title(vect_main[(i%%17)+1])
  if(i==35){title(vect_main[length(vect_main)])}
  abline(h=0,col="darkgrey", lwd=2, lty=2 )
  
  if (i==17|i==21|i==25|i==29|i==33){legend("bottomright", legend=c("Barbeaux", "Silures", "Chevesnes", "R->R", "M->M"), bty = "n",
         col=c(col_espece,"black", "black"), pch=c(rep(NA,3),1,16),lty=c(rep(1,3),NA,NA),lwd=c(rep(3,3),1,1))
  }
  
  for (j in 2:length(numpoiss_vect0)){
    col_temp<-ifelse(j<=9,col_espece[1], ifelse(j>20, col_espece[3], col_espece[2]))
    temp=mtot_list[[j]]
    points(vect_tailles[j],median(temp[,i]), col=col_temp,pch=1, cex=2)
    points(vect_tailles[j],median(temp[,i+1]), col=col_temp,pch=16, cex=2)
  }
  
}

dev.off()



##################### random forest ################################
library(randomForest)
library(cluster)
library(dendextend)

### prepa df et vecteurs ####
#species=c(rep("BAF",9), rep("SIL",11), rep("CHE", 17))

id_sp_bienloc=id_sp_tous[nbloc/60>36]
numpoiss_bienloc=numpoiss_vect0[nbloc/60>36]
nbloc_bienloc=nbloc[nbloc/60>36]

data_ind=data.frame(id=id_sp_bienloc, nbloc=nbloc[nbloc/60>36],size=vect_tailles[nbloc/60>36])
data_ind2=data_ind[order(data_ind$id),]

index_numpoiss=c(1,4,2,5,6,3, 20,13,14,15,16,17,21,18,19, 8,9,10,11,7,12)
  
  # c("B-3100", "B-3156", "B-3744", "B-3128", "B-3170", 
  #               "S-3856", "S-3632", "S-3786", "S-3835", "S-3849", "S-3870"
  #               ,"C-3183" ,"C-3212" ,"C-3240" ,"C-3352" ,"C-3464" ,"C-3730" ,"C-3758" ,"C-3121" ,"C-3394" ,"C-3506")#

#######################
proba=c(0.1,0.5,0.9)  #c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)

# df_rf_a=matrix(0,ncol=14*length(proba),nrow=length(numpoiss_vect0))
# for (i in 1:length(numpoiss_vect0)){
#   
#   temp=mtot_list[[i]]
#   vect_q=as.numeric()
#   for (j in 17:30){  #17:length(temp[1,])){
#   param_quantile=quantile(temp[,j],probs = proba)
#   vect_q=c(vect_q,param_quantile)
#   }
#   
#   df_rf_a[i,]=vect_q
# }
# df_rf_a=data.frame(df_rf_a)
# df_rf_a=cbind(df_rf_a,numpoiss_vect0)

##avec mean et variance
df_rf_a=matrix(0,ncol=14*2,nrow=length(numpoiss_vect0))
for (i in 1:length(numpoiss_vect0)){
  
  temp=mtot_list[[i]]
  vect_q=as.numeric()
  for (j in 17:30){  #17:length(temp[1,])){
    param_quantile=c(mean(temp[,j]), sd(temp[,j]))
    vect_q=c(vect_q,param_quantile)
  }
  
  df_rf_a[i,]=vect_q
}
df_rf_a=data.frame(df_rf_a)
# df_rf_a=cbind(df_rf_a,numpoiss_vect0

####### test individus bien loc ######
df_rf_a=matrix(0,ncol=14*2,nrow=1)
for (i in which(numpoiss_vect0 %in% numpoiss_bienloc)){
  
  temp=mtot_list[[i]]
  vect_q=as.numeric()
  for (j in 17:30){  #17:length(temp[1,])){
    param_quantile=c(mean(temp[,j]), sd(temp[,j]))
    vect_q=c(vect_q,param_quantile)
  }
  
  df_rf_a=rbind(df_rf_a,vect_q)
}
df_rf_a=data.frame(df_rf_a[-1,])

######avec les individus repetes#####
replicats=50
df_rf_a=matrix(0,ncol=14,nrow=1)#000*length(numpoiss_vect0))
for (i in which(numpoiss_vect0 %in% numpoiss_bienloc)){   #length(numpoiss_vect0)){
  
  temp=mtot_list[[i]]
  df_rf_a=rbind(df_rf_a, temp[runif(replicats,min=8000, max=11000),17:30]) #temp[10001:10020,17:30])
}
df_rf_a=data.frame(df_rf_a[-1,])
df_rf_a=cbind(df_rf_a,individual=as.factor(unlist(lapply(X=id_sp_bienloc, FUN=rep, times=replicats))))
                #as.factor(unlist(lapply(X=id_sp_tous, FUN=rep, times=50))))

rf <- randomForest(individual~., data=df_rf_a, proximity=T, ntree=5000,mtry=4)
rf
rf$confusion[,33]

##on va enlever les individus avec des erreurs > 10% B3051, C3079, C3625, S3429
 # df_rf_a2=df_rf_a[-(which(df_rf_a$individual=="B-3051"|df_rf_a$individual=="C-3590"
 #                          # |df_rf_a$individual=="S-3429"
 #                          )),]

urf <- randomForest(x=df_rf_a[,-15], proximity=T, ntree=5000,mtry=4)
urf$importance

prox_coeff <- urf$proximity
truc_coeff=as.dist(1-prox_coeff)
clust_coeff=hclust(d=truc_coeff, method="ward.D2")
#plot(clust_coeff)
inertie <- sort(clust_coeff$height, decreasing = TRUE)
plot(inertie, pch=16, main="Coefficients de la matrice de transition", xlim=c(0,30))
abline(v=21, col="red", lty=2)
abline(v=4, col="forestgreen", lty=2)


dend <- as.dendrogram(clust_coeff)
dend <- dend %>%
  color_branches(k = 3) %>%
  set("branches_lwd", 2) %>%
  set("labels_cex", 0.8)
#set("branches_lty", c(1,2,1))
#plot(dend)
labels(dend) <- df_rf_a$individual[clust_coeff$order] #species[clust_coeff$order] #nbtraj[clust_coeff$order]#
pdf("plot8.pdf", height = 150, width = 15)
plot(dend,horiz=T,main="Coefficients de la matrice de transition")
dev.off()


rownames(prox_coeff)<-df_rf_a$individual
truc=c(rep(1,3*replicats), rep(2,3*replicats), rep(3,(21-6)*replicats)) 
       #,rep(5,2*replicats),rep(6,1*replicats),rep(7,1*replicats),rep(8,(29-12)*replicats))
truc2=data.frame(id=clust_coeff$order,truc)
truc3=truc2[order(truc2$id),]

pdf("clustplot3.pdf", height = 20, width = 20)
clusplot(prox_coeff, as.factor(truc3$truc), labels=2, cex.txt=1, cex=0.7)
dev.off()

######### kmeans #############
replicats=200
df_rf_a=matrix(0,ncol=14,nrow=1)#000*length(numpoiss_vect0))
for (i in which(numpoiss_vect0 %in% numpoiss_bienloc)){   #length(numpoiss_vect0)){
    temp=mtot_list[[i]]
  df_rf_a=rbind(df_rf_a, temp[runif(replicats,min=8000, max=11000),17:30]) #temp[10001:10020,17:30])
}
df_rf_a=data.frame(df_rf_a[-1,])
df_rf_a=cbind(df_rf_a,individual=as.factor(unlist(lapply(X=id_sp_bienloc, FUN=rep, times=replicats))))

df_rm=df_rf_a[,seq(1,15,2)]
df_mr=df_rf_a[,c(seq(2,14,2),15)]

####################
library(factoextra)
library(NbClust)
library(cluster)
library(FeatureImpCluster)
library(flexclust)

km <- kmeans(df_rf_a[,-15], 3, nstart = 20)

fviz_nbclust(df_rf_a[,-15], kmeans, method = "wss", nstart=20) +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

nbclust_out <- NbClust(
  data = df_rf_a[,-15],
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 6, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

sil <- silhouette(km$cluster, dist(df_rf_a[,-15]))
fviz_silhouette(sil)

fviz_cluster(km, df_rf_a[,-15], ellipse.type = "norm")

km$betweenss/km$totss

res <- kcca(df_rf_a[,-15],k=3)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(df_rf_a[,-15]))
plot(FeatureImp_res)

km <- kmeans(df_rf_a[,-15], 3, nstart = 50)
pred.km <- cbind(km$cluster, df_rf_a$individual)
table(pred.km[,2], pred.km[,1])
levels(df_rf_a$individual)
truc=table(pred.km[,2], pred.km[,1])
df_results_kmeans=data.frame(id=levels(df_rf_a$individual),X1=as.numeric(truc[,1]),X2=as.numeric(truc[,2]),
                        X3=as.numeric(truc[,3]),X4=as.numeric(truc[,4]),nbloc=data_ind2$nbloc,size=data_ind2$size)

for (i in 1:length(resultats_kmeans$id)){
resultats_kmeans$groupe[i]=which(truc[i,]==max(truc[i,]))
}
resultats_kmeans$index_numpoiss=index_numpoiss

################kmeans proba######################
replicats=20
df_rf_a=matrix(0,ncol=20,nrow=1)#000*length(numpoiss_vect0))
for (i in which(numpoiss_vect0 %in% numpoiss_bienloc)){   #length(numpoiss_vect0)){
  temp=mtot_list[[i]]
  df_rf_a=rbind(df_rf_a, temp[runif(replicats,min=8000, max=11000),17:36]) #temp[10001:10020,17:30])
}
df_rf_a=data.frame(df_rf_a[-1,])
df_rf_a=cbind(df_rf_a,individual=as.factor(unlist(lapply(X=id_sp_bienloc, FUN=rep, times=replicats))))


var_enviro=c(-2,-1,1,2)
matrice_proba=matrix(0,ncol=50,nrow=replicats*length(numpoiss_bienloc))

for (i in 1:(replicats*length(numpoiss_bienloc))){
  
  p12_h=exp(df_rf_a[i,1] + df_rf_a[i,3] * var_enviro + df_rf_a[i,15] * var_enviro^2)/ 
    (1 +  exp(df_rf_a[i,1] + df_rf_a[i,3] * var_enviro + df_rf_a[i,15] * var_enviro^2))
  
  p21_h=exp(df_rf_a[i,2] + df_rf_a[i,4] * var_enviro + df_rf_a[i,16] * var_enviro^2)/ 
    (1 +  exp(df_rf_a[i,2] + df_rf_a[i,4] * var_enviro + df_rf_a[i,16] * var_enviro^2))
  
  p12_v=exp(df_rf_a[i,1] + df_rf_a[i,5] * var_enviro + df_rf_a[i,17] * var_enviro^2)/ 
    (1 +  exp(df_rf_a[i,1] + df_rf_a[i,5] * var_enviro + df_rf_a[i,17] * var_enviro^2))
  p21_v=exp(df_rf_a[i,2] + df_rf_a[i,6] * var_enviro + df_rf_a[i,18] * var_enviro^2)/ 
    (1 +  exp(df_rf_a[i,2] + df_rf_a[i,6] * var_enviro + df_rf_a[i,18] * var_enviro^2))
  
  p12_t=exp(df_rf_a[i,1] + df_rf_a[i,7] * var_enviro + df_rf_a[i,19] * var_enviro^2)/ 
    (1 +  exp(df_rf_a[i,1] + df_rf_a[i,7] * var_enviro + df_rf_a[i,19] * var_enviro^2))
  p21_t=exp(df_rf_a[i,2] + df_rf_a[i,8] * var_enviro + df_rf_a[i,20] * var_enviro^2)/ 
    (1 +  exp(df_rf_a[i,2] + df_rf_a[i,8] * var_enviro + df_rf_a[i,20] * var_enviro^2))
  
  p12_vmax=exp(df_rf_a[i,1] + df_rf_a[i,11] * var_enviro)/(1 +  exp(df_rf_a[i,1] + df_rf_a[i,11] * var_enviro))
  p21_vmax=exp(df_rf_a[i,2] + df_rf_a[i,12] * var_enviro)/(1 +  exp(df_rf_a[i,2] + df_rf_a[i,12] * var_enviro))
  
  p12_cvv=exp(df_rf_a[i,1] + df_rf_a[i,9] * var_enviro)/(1 +  exp(df_rf_a[i,1] + df_rf_a[i,9] * var_enviro))
  p21_cvv=exp(df_rf_a[i,2] + df_rf_a[i,10] * var_enviro)/(1 +  exp(df_rf_a[i,2] + df_rf_a[i,10] * var_enviro))
  
  p12_hex=exp(df_rf_a[i,1] + df_rf_a[i,13] * var_enviro)/(1 +  exp(df_rf_a[i,1] + df_rf_a[i,13] * var_enviro))
  p21_hex=exp(df_rf_a[i,2] + df_rf_a[i,14] * var_enviro)/(1 +  exp(df_rf_a[i,2] + df_rf_a[i,14] * var_enviro))
  
  p12=exp(df_rf_a[i,1])/(1 +  exp(df_rf_a[i,1]))
  p21=exp(df_rf_a[i,2])/(1 +  exp(df_rf_a[i,2]))
  
  matrice_proba[i,]=c(p12_h,p21_h,p12_v,p21_v,p12_t,p21_t,p12_vmax,p21_vmax,p12_cvv,p21_cvv,p12_hex,p21_hex,p12,p21)
}

df_proba=data.frame(matrice_proba,individual=as.factor(unlist(lapply(X=id_sp_bienloc, FUN=rep, times=replicats))))

df_rm_proba=df_proba[,c(1,2,5,6,9,10,13,14,17,18,21,22,25,27)]
df_mr_proba=df_proba[,c(c(1,2,5,6,9,10,13,14,17,18,21,22)+2,26,27)]

km <- kmeans(df_proba[,-27],4, nstart = 20)
pred.km <- cbind(km$cluster, df_proba$individual)
table(pred.km[,2], pred.km[,1])

fviz_nbclust(df_proba[,-27], kmeans, method = "wss", nstart=20) +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

nbclust_out <- NbClust(
  data = df_proba[,-27],
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 6, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

res <- kcca(df_proba[,-27],k=4)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(df_proba[,-27]))
plot(FeatureImp_res)

fviz_cluster(km, df_proba[,-27], ellipse.type = "norm")


vect=as.numeric()

for (i in 1:(replicats*length(numpoiss_bienloc))){
  
  calcul_proba_p12_h <- function (x) {exp(df_rf_a[i,1] + df_rf_a[i,3] * x + df_rf_a[i,15] * x^2)/ 
      (1 +  exp(df_rf_a[i,1] + df_rf_a[i,3] * x + df_rf_a[i,15] * x^2))}
  
  vect[i]=integrate(calcul_proba_p12_h,-1,1)[[1]]
}

matrice_p12h=matrix(0,ncol=replicats*length(numpoiss_bienloc),nrow=replicats*length(numpoiss_bienloc))

for (i in 1:(replicats*length(numpoiss_bienloc))){
  
  matrice_p12h[i,]=sqrt((vect-vect[i])^2)
  
}

##################mvt ############
#proba=c(0.1,0.5,0.9)  #c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)

# df_rf_mvt=matrix(0,ncol=4*length(proba),nrow=length(numpoiss_vect0))
# for (i in 1:length(numpoiss_vect0)){
#   
#   temp=mtot_list[[i]]
#   vect_q=as.numeric()
#   for (j in c(1,2,5,6)){
#     param_quantile=as.numeric(quantile(temp[,j],probs = proba))
#     vect_q=c(vect_q,param_quantile)
#   }
#   
#   df_rf_mvt[i,]=vect_q
# }
# df_rf_mvt=data.frame(df_rf_mvt)

df_rf_mvt=matrix(0,ncol=4*2,nrow=length(numpoiss_vect0))
for (i in 1:length(numpoiss_vect0)){
  
  temp=mtot_list[[i]]
  vect_q=as.numeric()
  for (j in c(1,2,5,6)){
    param_quantile=c(mean(temp[,j]), sd(temp[,j]))
    vect_q=c(vect_q,param_quantile)
  }
  
  df_rf_mvt[i,]=vect_q
}
df_rf_mvt=data.frame(df_rf_mvt)
# df_rf_mvt=cbind(df_rf_mvt,numpoiss_vect0)


### calcul rf et plots ###########
urf_coeff <- randomForest(x=df_rf_a2, proximity=T, ntree=10000,mtry=5)
urf_coeff$importance

prox_coeff <- urf_coeff$proximity
truc_coeff=as.dist(1-prox_coeff)
clust_coeff=hclust(d=truc_coeff, method="ward.D2")
#plot(clust_coeff)
inertie <- sort(clust_coeff$height, decreasing = TRUE)
plot(inertie, pch=16, main="Coefficients de la matrice de transition (mean)")


dend <- as.dendrogram(clust_coeff)
dend <- dend %>%
  color_branches(k = 3) %>%
  set("branches_lwd", 2) #%>%
#set("branches_lty", c(1,2,1))
#plot(dend)
labels(dend) <- id_sp_bienloc[clust_coeff$order] #nbloc_bienloc[clust_coeff$order] #species[clust_coeff$order] #nbtraj[clust_coeff$order]#
plot(dend,horiz=T,main="Coefficients de la matrice de transition (mean)")

rownames(prox_coeff)<-id_sp_bienloc
truc=c(rep(1,10), rep(2,3), rep(3,8))   #c(rep(1,8), rep(2,6), rep(3,7), rep(4,3),rep(5,8)) #
truc2=data.frame(id=clust_coeff$order,truc)
truc3=truc2[order(truc2$id),]
clusplot(prox_coeff, as.factor(truc3$truc), labels=2, cex.txt=0.8)

# pam.rf <- pam(prox, 3)
# (cl.rf <- as.factor(pam.rf$cluster))
urf_mvt <- randomForest(x=df_rf_mvt, proximity=T, ntree=5000,mtry=5)

prox_mvt <- urf_mvt$proximity
truc_mvt=as.dist(1-prox_mvt)
clust_mvt=hclust(d=truc_mvt, method="ward.D2")
plot(clust_mvt)
inertie <- sort(clust_mvt$height, decreasing = TRUE)
plot(inertie, pch=16, main="Mouvement - vitesse et angle")

dend <- as.dendrogram(clust_mvt)
dend <- dend %>%
  color_branches(k = 2) %>%
  set("branches_lwd", 2) #%>%
#set("branches_lty", c(1,2,1))
#plot(dend)
labels(dend) <- species[clust_mvt$order]#numpoiss_vect0[clust_coeff$order]
plot(dend,horiz=T,main="Mouvement - vitesse et angle")

rownames(prox_mvt)<-numpoiss_vect0
truc=c(rep(1,11), rep(2,9),rep(3,37-11-9))#truc=c(rep(1,14), rep(2,10), rep(3,13))
truc2=data.frame(id=clust_mvt$order,truc)
truc3=truc2[order(truc2$id),]
clusplot(prox_mvt, as.factor(truc3$truc), labels=2, cex.txt=0.8)

##supervised
df_rf_a=cbind(df_rf_a,species=species)
df_rf_mvt=cbind(df_rf_mvt,species=species)
rf <- randomForest(species~., data=df_rf_mvt, proximity=T, ntree=5000,mtry=3)

#############################boxplot groupe#############

param=unlist(lapply(X=param_names[17:30], FUN=rep, times=500*length(numpoiss_bienloc)))
group=rep(unlist(lapply(X=df_results_urf_mean$groupe, FUN=rep, times=500)),length(param_names[17:30]))
individual=rep(unlist(lapply(X=df_results_kmeans$id, FUN=rep, times=500)),length(param_names[17:30]))
value=as.numeric()
for (i in 17:30){
  
  for (j in as.numeric(df_results_kmeans$index_numpoiss)){
    
    temp=mtot_list[[j]]
    value=c(value,temp[runif(500,min=8000, max=11000),i])
  }
  
}

ggdf=data.frame(param=as.factor(param), individual=as.factor(individual), group=as.factor(group), value=value)

pdf("ggboxplot.pdf", 20, 20)
ggplot()+ #, fill=ref))+
  #geom_abline(slope=0, intercept=intercept[i], colour="grey45", lty=2, lwd=1.2)+
  geom_boxplot(data=ggdf, aes(y=value, x=individual, fill=group))+ 
  facet_wrap(. ~ param, ncol=4, scales='free_x')+#
  labs(y="Parameter value", x="Individual")+ 
  #scale_fill_viridis(discrete=TRUE) +
  #ggtitle(parameters_names[j])+
  theme_minimal() +coord_flip()#+ 
#theme(legend.position = "none")
dev.off()

############# classif avec matrices de dissimilarites via wasserstein metric ###############
library(transport)
nsample=1000
clust_coeff_list=list()
dys_matrices_list=list()

pdf(file="inertie_coeff_wasserstein.pdf", height = 15, width = 15)
par(mfrow=c(3,5))
for (p in 17:30){
  
  a11=matrix(0,ncol=nsample,nrow=1)
  for (i in c(1:15,17,20:34,37)){ 
    
    temp=mtot_list[[i]]
    a11=rbind(a11, temp[runif(nsample,min=5000, max=12000),p])
  }
  a11=a11[-1,]
  
  a11_dys=matrix(0,ncol=32,nrow=32)
  for (i in 1:32){
    for (j in 1:32){
      a11_dys[i,j]=wasserstein1d(a11[i,],a11[j,])
    }}
  
  dys_matrices_list[[p]]<-a11_dys
  clust_coeff=hclust(d=as.dist(a11_dys), method="ward.D2")
  inertie <- sort(clust_coeff$height, decreasing = TRUE)
  plot(inertie, pch=16, main=param_names[p])
  clust_coeff_list[[p]]=clust_coeff
}
dev.off()

nb_gp=c(4,3,4,4,5,3,3,4,3,4,3,3,4,3)
pdf(file="clustering_par_coeff_wasserstein.pdf", height = 15, width = 15)
par(mfrow=c(2,2))
for (p in 17:30){
  clust_coeff=clust_coeff_list[[p]]
  dend <- as.dendrogram(clust_coeff)
  dend <- dend %>%
    color_branches(k = nb_gp[p-16]) %>%
    set("branches_lwd", 2) %>%
    set("labels_cex", 0.8)
  labels(dend) <- id_sp_bienloc[clust_coeff$order]
  plot(dend,horiz=T, main=param_names[p])
 
}
dev.off()

hauteur=c(rep(1.4,4),1.25,rep(1.25,4),0.9,1.9,1.5,1.2,1.2)

pdf("clustplot_coeff_wasserstein.pdf", height = 15, width = 15)
par(mfrow=c(2,2))
for (p in 17:30){

  rownames(dys_matrices_list[[p]])<- id_sp_bienloc
  clust_coeff=clust_coeff_list[[p]]
  
  vect_times=as.numeric()
  cut(as.dendrogram(clust_coeff),h=hauteur[p-16])$lower->machin
  for (i in 1:nb_gp[p-16]){
    vect_times=c(vect_times,length(unlist(machin[[i]])))
  }
  
  # bidule <- cutree(clust_coeff, k = nb_gp[p-16])
  # vect_times1=unlist(lapply(X=1:nb_gp[p-16], FUN=function(x) length(which(bidule==x))))
  # if (nb_gp[p-16]==3){vect_times=vect_times1[c(2,1,3)]}
  # if (nb_gp[p-16]==4){vect_times=vect_times1[c(2,4,3,1)]}
  # if (nb_gp[p-16]==5){vect_times=vect_times1[c(4,2,5,1,3)]}
  
  truc=as.numeric()
  for (i in 1:nb_gp[p-16]){
  truc=c(truc,rep(i,vect_times[i]))
  }
  truc2=data.frame(id=clust_coeff$order,truc)
  truc3=truc2[order(truc2$id),]

  clusplot(dys_matrices_list[[p]], as.factor(truc3$truc), labels=2, cex.txt=1, cex=1,
           main=param_names[p])
}
dev.off()

################## plot 2D #######
library(plot3D)
varenviro_tous=read.table(file="/home/dominique.lamonica/Bureau/decembre2020/data/min_max_mean_var_enviro_tous_ind.csv"
                          , header=T, sep=",")[,-1]
min_ind=read.table(file="/home/dominique.lamonica/Bureau/decembre2020/data/min_var_enviro_tous_par_ind.csv"
                          , header=T, sep=",")[,-1]
max_ind=read.table(file="/home/dominique.lamonica/Bureau/decembre2020/data/max_var_enviro_tous_par_ind.csv"
                   , header=T, sep=",")[,-1]
mean_ind=read.table(file="/home/dominique.lamonica/Bureau/decembre2020/data/mean_var_enviro_tous_par_ind.csv"
                   , header=T, sep=",")[,-1]


####choix de l'individu #####
i=37
id_sp_tous[i]
temp=mtot_list[[i]]
vect_a=as.numeric()
for (j in 17:length(temp[1,])){
  param_quantile=as.numeric(quantile(temp[,j],probs = 0.5))
  vect_a=c(vect_a,param_quantile)
}

dftemp=c(seq(min_ind[i,5],max_ind[i,5],length.out=70))
hmod=c(seq(min_ind[i,3],max_ind[i,3],length.out=70))
vmod=seq(min_ind[i,1],max_ind[i,1],length.out=70)
vmax=seq(min_ind[i,7],max_ind[i,7],length.out=70)
cvv=seq(min_ind[i,8],max_ind[i,8],length.out=70)
nbhex=seq(min_ind[i,9],max_ind[i,9],length.out=70)

###########calcul des proba ################
#####temp + hauteur + vitesse
mat_ht=mesh(hmod,dftemp)
p12_ht=with(mat_ht,(exp(vect_a[1] + vect_a[7] * y + vect_a[3] * x + vect_a[19] * y^2 + vect_a[15] * x^2)/
                      (1 +  exp(vect_a[1] + vect_a[7] * y + vect_a[3] * x + vect_a[19] * y^2 + vect_a[15] * x^2))))
p21_ht=with(mat_ht,(exp(vect_a[2] + vect_a[8] * y + vect_a[4] * x + vect_a[20] * y^2 + vect_a[16] * x^2)/
                      (1 +  exp(vect_a[2] + vect_a[8] * y + vect_a[4] * x + vect_a[20] * y^2 + vect_a[16] * x^2))))

mat_vt=mesh(vmod,dftemp)
p12_vt=with(mat_vt,(exp(vect_a[1] + vect_a[7] * y + vect_a[5] * x + vect_a[19] * y^2 + vect_a[17] * x^2)/
                      (1 +  exp(vect_a[1] + vect_a[7] * y + vect_a[5] * x + vect_a[19] * y^2 + vect_a[17] * x^2))))
p21_vt=with(mat_vt,(exp(vect_a[2] + vect_a[8] * y + vect_a[6] * x + vect_a[20] * y^2 + vect_a[18] * x^2)/
                      (1 +  exp(vect_a[2] + vect_a[8] * y + vect_a[6] * x + vect_a[20] * y^2 + vect_a[18] * x^2))))

mat_vh=mesh(vmod,hmod)
p12_vh=with(mat_vh,(exp(vect_a[1] + vect_a[3] * y + vect_a[5] * x + vect_a[15] * y^2 + vect_a[17] * x^2)/
                      (1 +  exp(vect_a[1] + vect_a[3] * y + vect_a[5] * x + vect_a[15] * y^2 + vect_a[17] * x^2))))
p21_vh=with(mat_vh,(exp(vect_a[2] + vect_a[4] * y + vect_a[6] * x + vect_a[16] * y^2 + vect_a[18] * x^2)/
                      (1 +  exp(vect_a[2] + vect_a[4] * y + vect_a[6] * x + vect_a[16] * y^2 + vect_a[18] * x^2))))

##hauteur + histo
mat_hcvv=mesh(hmod,cvv)
p12_hcvv=with(mat_hcvv,(exp(vect_a[1] + vect_a[11] * y + vect_a[3] * x + vect_a[15] * x^2)/
                      (1 +  exp(vect_a[1] + vect_a[11] * y + vect_a[3] * x + vect_a[15] * x^2))))
p21_hcvv=with(mat_hcvv,(exp(vect_a[2] + vect_a[12] * y + vect_a[4] * x + vect_a[16] * x^2)/
                      (1 +  exp(vect_a[2] + vect_a[12] * y + vect_a[4] * x + vect_a[16] * x^2))))

mat_hvmax=mesh(hmod,vmax)
p12_hvmax=with(mat_hvmax,(exp(vect_a[1] + vect_a[9] * y + vect_a[3] * x + vect_a[15] * x^2)/
                          (1 +  exp(vect_a[1] + vect_a[9] * y + vect_a[3] * x + vect_a[15] * x^2))))
p21_hvmax=with(mat_hvmax,(exp(vect_a[2] + vect_a[10] * y + vect_a[4] * x + vect_a[16] * x^2)/
                          (1 +  exp(vect_a[2] + vect_a[10] * y + vect_a[4] * x + vect_a[16] * x^2))))

mat_hnbhex=mesh(hmod,nbhex)
p12_hnbhex=with(mat_hnbhex,(exp(vect_a[1] + vect_a[13] * y + vect_a[3] * x + vect_a[15] * x^2)/
                          (1 +  exp(vect_a[1] + vect_a[13] * y + vect_a[3] * x + vect_a[15] * x^2))))
p21_hnbhex=with(mat_hnbhex,(exp(vect_a[2] + vect_a[14] * y + vect_a[4] * x + vect_a[16] * x^2)/
                          (1 +  exp(vect_a[2] + vect_a[14] * y + vect_a[4] * x + vect_a[16] * x^2))))

##vitesse + histo
mat_vcvv=mesh(vmod,cvv)
p12_vcvv=with(mat_vcvv,(exp(vect_a[1] + vect_a[11] * y + vect_a[5] * x + vect_a[17] * x^2)/
                          (1 +  exp(vect_a[1] + vect_a[11] * y + vect_a[5] * x + vect_a[17] * x^2))))
p21_vcvv=with(mat_vcvv,(exp(vect_a[2] + vect_a[12] * y + vect_a[6] * x + vect_a[18] * x^2)/
                          (1 +  exp(vect_a[2] + vect_a[12] * y + vect_a[6] * x + vect_a[18] * x^2))))

mat_vvmax=mesh(vmod,vmax)
p12_vvmax=with(mat_vvmax,(exp(vect_a[1] + vect_a[9] * y + vect_a[5] * x + vect_a[17] * x^2)/
                            (1 +  exp(vect_a[1] + vect_a[9] * y + vect_a[5] * x + vect_a[17] * x^2))))
p21_vvmax=with(mat_vvmax,(exp(vect_a[2] + vect_a[10] * y + vect_a[6] * x + vect_a[18] * x^2)/
                            (1 +  exp(vect_a[2] + vect_a[10] * y + vect_a[6] * x + vect_a[18] * x^2))))

mat_vnbhex=mesh(vmod,nbhex)
p12_vnbhex=with(mat_vnbhex,(exp(vect_a[1] + vect_a[13] * y + vect_a[5] * x + vect_a[17] * x^2)/
                              (1 +  exp(vect_a[1] + vect_a[13] * y + vect_a[5] * x + vect_a[17] * x^2))))
p21_vnbhex=with(mat_vnbhex,(exp(vect_a[2] + vect_a[14] * y + vect_a[6] * x + vect_a[18] * x^2)/
                              (1 +  exp(vect_a[2] + vect_a[14] * y + vect_a[6] * x + vect_a[18] * x^2))))

##tempe + histo
mat_tcvv=mesh(dftemp,cvv)
p12_tcvv=with(mat_tcvv,(exp(vect_a[1] + vect_a[11] * y + vect_a[7] * x + vect_a[19] * x^2)/
                          (1 +  exp(vect_a[1] + vect_a[11] * y + vect_a[7] * x + vect_a[19] * x^2))))
p21_tcvv=with(mat_tcvv,(exp(vect_a[2] + vect_a[12] * y + vect_a[8] * x + vect_a[20] * x^2)/
                          (1 +  exp(vect_a[2] + vect_a[12] * y + vect_a[8] * x + vect_a[20] * x^2))))

mat_tvmax=mesh(dftemp,vmax)
p12_tvmax=with(mat_tvmax,(exp(vect_a[1] + vect_a[9] * y + vect_a[7] * x + vect_a[19] * x^2)/
                            (1 +  exp(vect_a[1] + vect_a[9] * y + vect_a[7] * x + vect_a[19] * x^2))))
p21_tvmax=with(mat_tvmax,(exp(vect_a[2] + vect_a[10] * y + vect_a[8] * x + vect_a[20] * x^2)/
                            (1 +  exp(vect_a[2] + vect_a[10] * y + vect_a[8] * x + vect_a[20] * x^2))))

mat_tnbhex=mesh(dftemp,nbhex)
p12_tnbhex=with(mat_tnbhex,(exp(vect_a[1] + vect_a[13] * y + vect_a[7] * x + vect_a[19] * x^2)/
                              (1 +  exp(vect_a[1] + vect_a[13] * y + vect_a[7] * x + vect_a[19] * x^2))))
p21_tnbhex=with(mat_tnbhex,(exp(vect_a[2] + vect_a[14] * y + vect_a[8] * x + vect_a[20] * x^2)/
                              (1 +  exp(vect_a[2] + vect_a[14] * y + vect_a[8] * x + vect_a[20] * x^2))))

##histo
mat_vmaxcvv=mesh(vmax,cvv)
p12_vmaxcvv=with(mat_vmaxcvv,(exp(vect_a[1] + vect_a[11] * y + vect_a[9] * x)/
                          (1 +  exp(vect_a[1] + vect_a[11] * y + vect_a[9] * x))))
p21_vmaxcvv=with(mat_vmaxcvv,(exp(vect_a[2] + vect_a[12] * y + vect_a[10] * x)/
                          (1 +  exp(vect_a[2] + vect_a[12] * y + vect_a[10] * x))))

mat_nbhexcvv=mesh(nbhex,cvv)
p12_nbhexcvv=with(mat_nbhexcvv,(exp(vect_a[1] + vect_a[11] * y + vect_a[13] * x)/
                                (1 +  exp(vect_a[1] + vect_a[11] * y + vect_a[13] * x))))
p21_nbhexcvv=with(mat_nbhexcvv,(exp(vect_a[2] + vect_a[12] * y + vect_a[14] * x)/
                                (1 +  exp(vect_a[2] + vect_a[12] * y + vect_a[14] * x))))

mat_nbhexvmax=mesh(nbhex,vmax)
p12_nbhexvmax=with(mat_nbhexvmax,(exp(vect_a[1] + vect_a[9] * y + vect_a[13] * x)/
                                  (1 +  exp(vect_a[1] + vect_a[9] * y + vect_a[13] * x))))
p21_nbhexvmax=with(mat_nbhexvmax,(exp(vect_a[2] + vect_a[10] * y + vect_a[14] * x)/
                                  (1 +  exp(vect_a[2] + vect_a[10] * y + vect_a[14] * x))))



##########PLOTS ####
palette=cividis(n = 20)


####hauteur/tempe/vitesse####

pdf(paste("maps_",id_sp_tous[i],".pdf",sep=""), width=10, height =10)
par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

image2D(z=p12_ht, x=hmod, y=dftemp,ylab="Upstream temperature difference", xlab="Water depth", 
        col=palette[round(20*min(p12_ht)):round(20*max(p12_ht))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1, 
        ylim=c(varenviro_tous[1,14],varenviro_tous[2,14]), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,5],lty=2,lwd=2)

image2D(z=p21_ht, x=hmod, y=dftemp,ylab="Upstream temperature difference", xlab="Water depth",
        col=palette[round(20*min(p21_ht)):round(20*max(p21_ht))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,14],varenviro_tous[2,14]), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,5],lty=2,lwd=2)


image2D(z=p12_vt, x=vmod, y=dftemp,ylab="Upstream temperature difference", xlab="Flow velocity",
        col=palette[round(20*min(p12_vt)):round(20*max(p12_vt))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,14],varenviro_tous[2,14]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,5],lty=2,lwd=2)

image2D(z=p21_vt, x=vmod, y=dftemp,ylab="Upstream temperature difference", xlab="Flow velocity",
        col=palette[round(20*min(p21_vt)):round(20*max(p21_vt))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,14],varenviro_tous[2,14]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,5],lty=2,lwd=2)


image2D(z=p12_vh, x=vmod, y=hmod,ylab="Water depth", xlab="Flow velocity",
        col=palette[round(20*min(p12_vh)):round(20*max(p12_vh))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,10],varenviro_tous[2,10]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,3],lty=2,lwd=2)

image2D(z=p21_vh, x=vmod, y=hmod,ylab="Water depth", xlab="Flow velocity",
        col=palette[round(20*min(p21_vh)):round(20*max(p21_vh))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,10],varenviro_tous[2,10]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,3],lty=2,lwd=2)

#dev.off()

####hauteur/histo####

# pdf(paste("maps_",id_sp_tous[i],"_2.pdf",sep=""), width=10, height =10)
# par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

image2D(z=p12_hcvv, x=hmod, y=cvv ,ylab="Speed coefficient of variation", xlab="Water depth", 
        col=palette[round(20*min(p12_hcvv)):round(20*max(p12_hcvv))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,12],max_ind[i,8]+2), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p21_hcvv, x=hmod, y=cvv ,ylab="Speed coefficient of variation", xlab="Water depth", 
        col=palette[round(20*min(p21_hcvv)):round(20*max(p21_hcvv))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,12],max_ind[i,8]+2), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p12_hvmax, x=hmod, y=vmax ,ylab="Maximum flow speed", xlab="Water depth",  
        col=palette[round(20*min(p12_hvmax)):round(20*max(p12_hvmax))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)

image2D(z=p21_hvmax, x=hmod, y=vmax ,ylab="Maximum flow speed", xlab="Water depth",  
        col=palette[round(20*min(p21_hvmax)):round(20*max(p21_hvmax))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)


image2D(z=p12_hnbhex, x=hmod, y=nbhex ,ylab="Exondation hours", xlab="Water depth",   
        col=palette[round(20*min(p12_hnbhex)):round(20*max(p12_hnbhex))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,18],varenviro_tous[2,18]), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,9],lty=2,lwd=2)

image2D(z=p21_hnbhex, x=hmod, y=nbhex ,ylab="Exondation hours", xlab="Water depth",   
        col=palette[round(20*min(p21_hnbhex)):round(20*max(p21_hnbhex))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,18],varenviro_tous[2,18]), xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]))
abline(v=mean_ind[i,3],lty=2,lwd=2)
abline(h=mean_ind[i,9],lty=2,lwd=2)

# dev.off()

####vitesse/histo####

# pdf(paste("maps_",id_sp_tous[i],"_3.pdf",sep=""), width=10, height =10)
# par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

image2D(z=p12_vcvv, x=vmod, y=cvv ,ylab="Speed coefficient of variation", xlab="Flow velocity",  
        col=palette[round(20*min(p12_vcvv)):round(20*max(p12_vcvv))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,12],max_ind[i,8]+2), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p21_vcvv, x=vmod, y=cvv ,ylab="Speed coefficient of variation", xlab="Flow velocity",  
        col=palette[round(20*min(p21_vcvv)):round(20*max(p21_vcvv))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,12],max_ind[i,8]+2), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p12_vvmax, x=vmod, y=vmax ,ylab="Maximum flow speed", xlab="Flow velocity",   
        col=palette[round(20*min(p12_vvmax)):round(20*max(p12_vvmax))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)

image2D(z=p21_vvmax, x=vmod, y=vmax ,ylab="Maximum flow speed", xlab="Flow velocity",  
        col=palette[round(20*min(p21_vvmax)):round(20*max(p21_vvmax))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)


image2D(z=p12_vnbhex, x=vmod, y=nbhex ,ylab="Exondation hours", xlab="Flow velocity",    
        col=palette[round(20*min(p12_vnbhex)):round(20*max(p12_vnbhex))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,18],varenviro_tous[2,18]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,9],lty=2,lwd=2)

image2D(z=p21_vnbhex, x=vmod, y=nbhex ,ylab="Exondation hours", xlab="Flow velocity",    
        col=palette[round(20*min(p21_vnbhex)):round(20*max(p21_vnbhex))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        ylim=c(varenviro_tous[1,18],varenviro_tous[2,18]), xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]))
abline(v=mean_ind[i,1],lty=2,lwd=2)
abline(h=mean_ind[i,9],lty=2,lwd=2)

#dev.off()

#######tempe/histo####

# pdf(paste("maps_",id_sp_tous[i],"_4.pdf",sep=""), width=10, height =10)
# par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

image2D(z=p12_tcvv, x=dftemp, y=cvv ,ylab="Speed coefficient of variation", xlab="Upstream temperature difference",   
        col=palette[round(20*min(p12_tcvv)):round(20*max(p12_tcvv))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(varenviro_tous[1,12],max_ind[i,8]+2))
abline(v=mean_ind[i,5],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p21_tcvv, x=dftemp, y=cvv ,ylab="Speed coefficient of variation", xlab="Upstream temperature difference",   
        col=palette[round(20*min(p21_tcvv)):round(20*max(p21_tcvv))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(varenviro_tous[1,12],max_ind[i,8]+2))
abline(v=mean_ind[i,5],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p12_tvmax, x=dftemp, y=vmax ,ylab="Maximum flow speed", xlab="Upstream temperature difference",    
        col=palette[round(20*min(p12_tvmax)):round(20*max(p12_tvmax))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]))
abline(v=mean_ind[i,5],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)

image2D(z=p21_tvmax, x=dftemp, y=vmax ,ylab="Maximum flow speed", xlab="Upstream temperature difference",    
        col=palette[round(20*min(p21_tvmax)):round(20*max(p21_tvmax))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]))
abline(v=mean_ind[i,5],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)


image2D(z=p12_tnbhex, x=dftemp, y=nbhex ,ylab="Exondation hours", xlab="Upstream temperature difference",    
        col=palette[round(20*min(p12_tnbhex)):round(20*max(p12_tnbhex))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(varenviro_tous[1,18],varenviro_tous[2,18]))
abline(v=mean_ind[i,5],lty=2,lwd=2)
abline(h=mean_ind[i,9],lty=2,lwd=2)

image2D(z=p21_tnbhex, x=dftemp, y=nbhex ,ylab="Exondation hours", xlab="Upstream temperature difference",    
        col=palette[round(20*min(p21_tnbhex)):round(20*max(p21_tnbhex))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(varenviro_tous[1,18],varenviro_tous[2,18]))
abline(v=mean_ind[i,5],lty=2,lwd=2)
abline(h=mean_ind[i,9],lty=2,lwd=2)

# dev.off()

#####histo####

# pdf(paste("maps_",id_sp_tous[i],"_5.pdf",sep=""), width=10, height =10)
# par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

image2D(z=p12_vmaxcvv, x=vmax, y=cvv ,ylab="Speed coefficient of variation", xlab="Maximum flow speed",     
        col=palette[round(20*min(p12_vmaxcvv)):round(20*max(p12_vmaxcvv))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(varenviro_tous[1,12],max_ind[i,8]+2))
abline(v=mean_ind[i,7],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p21_vmaxcvv, x=vmax, y=cvv ,ylab="Speed coefficient of variation", xlab="Maximum flow speed",     
        col=palette[round(20*min(p21_vmaxcvv)):round(20*max(p21_vmaxcvv))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(varenviro_tous[1,12],max_ind[i,8]+2))
abline(v=mean_ind[i,7],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)


image2D(z=p12_nbhexcvv, x=nbhex, y=cvv ,ylab="Speed coefficient of variation", xlab="Exondation hours",     
        col=palette[round(20*min(p12_nbhexcvv)):round(20*max(p12_nbhexcvv))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(varenviro_tous[1,12],max_ind[i,8]+2))
abline(v=mean_ind[i,9],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p21_nbhexcvv, x=nbhex, y=cvv ,ylab="Speed coefficient of variation", xlab="Exondation hours",     
        col=palette[round(20*min(p21_nbhexcvv)):round(20*max(p21_nbhexcvv))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(varenviro_tous[1,12],max_ind[i,8]+2))
abline(v=mean_ind[i,9],lty=2,lwd=2)
abline(h=mean_ind[i,8],lty=2,lwd=2)

image2D(z=p12_nbhexvmax, x=nbhex, y=vmax ,ylab="Maximum flow speed", xlab="Exondation hours",     
        col=palette[round(20*min(p12_nbhexvmax)):round(20*max(p12_nbhexvmax))],
        main="P(R->M), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]))
abline(v=mean_ind[i,9],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)

image2D(z=p21_nbhexvmax, x=nbhex, y=vmax ,ylab="Maximum flow speed", xlab="Exondation hours",     
        col=palette[round(20*min(p21_nbhexvmax)):round(20*max(p21_nbhexvmax))],
        main="P(M->R), other variables=0", cex.lab=1, cex.main=1, las=1 ,
        xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(varenviro_tous[1,13],varenviro_tous[2,13]))
abline(v=mean_ind[i,9],lty=2,lwd=2)
abline(h=mean_ind[i,7],lty=2,lwd=2)

dev.off()



################## plot 1D ###################

dftemp_mat=matrix(0,ncol=70,nrow=1)
hmod_mat=matrix(0,ncol=70,nrow=1)
vmod_mat=matrix(0,ncol=70,nrow=1)
vmax_mat=matrix(0,ncol=70,nrow=1)
cvv_mat=matrix(0,ncol=70,nrow=1)
nbhex_mat=matrix(0,ncol=70,nrow=1)

for (i in 1:37){   #length(numpoiss_vect0)){
  
  dftemp_mat=rbind(dftemp_mat,seq(min_ind[i,5],max_ind[i,5],length.out=70))
  hmod_mat=rbind(hmod_mat,seq(min_ind[i,3],max_ind[i,3],length.out=70))
  vmod_mat=rbind(vmod_mat,seq(min_ind[i,1],max_ind[i,1],length.out=70))
  vmax_mat=rbind(vmax_mat,seq(min_ind[i,7],max_ind[i,7],length.out=70))
  cvv_mat=rbind(cvv_mat,seq(min_ind[i,8],max_ind[i,8],length.out=70))
  nbhex_mat=rbind(nbhex_mat,seq(min_ind[i,9],max_ind[i,9],length.out=70))
}

dftemp_mat=dftemp_mat[-1,]
hmod_mat=hmod_mat[-1,]
vmod_mat=vmod_mat[-1,]
vmax_mat=vmax_mat[-1,]
cvv_mat=cvv_mat[-1,]
nbhex_mat=nbhex_mat[-1,]

col_espece=c("#00BA38", "#F8766D","#619CFF")
###########calcul des proba ################
p12_h=matrix(0,ncol=70,nrow=1)
p21_h=matrix(0,ncol=70,nrow=1)

p12_v=matrix(0,ncol=70,nrow=1)
p21_v=matrix(0,ncol=70,nrow=1)

p12_t=matrix(0,ncol=70,nrow=1)
p21_t=matrix(0,ncol=70,nrow=1)

p12_cvv=matrix(0,ncol=70,nrow=1)
p21_cvv=matrix(0,ncol=70,nrow=1)

p12_vmax=matrix(0,ncol=70,nrow=1)
p21_vmax=matrix(0,ncol=70,nrow=1)

p12_nbhex=matrix(0,ncol=70,nrow=1)
p21_nbhex=matrix(0,ncol=70,nrow=1)

vect_a12=as.numeric()
vect_a21=as.numeric()

for (i in 1:37){   #for (i in which(numpoiss_vect0 %in% numpoiss_bienloc)){   #length(numpoiss_vect0)){
  
  print(i)
  temp=mtot_list[[i]]
  
  for (j in 17:18){
    
  
    if(j==17){vect_a12=c(vect_a12,as.numeric(quantile(temp[,j],probs = 0.5)))}
    if(j==18){vect_a21=c(vect_a21,as.numeric(quantile(temp[,j],probs = 0.5)))}
  }
}

for (i in 1:37){   #for (i in which(numpoiss_vect0 %in% numpoiss_bienloc)){   #length(numpoiss_vect0)){
  
  temp=mtot_list[[i]]
  vect_a=as.numeric()
  for (j in 17:length(temp[1,])){
    param_quantile=as.numeric(quantile(temp[,j],probs = 0.5))
    vect_a=c(vect_a,param_quantile)
   }
  
  
  dftemp=dftemp_mat[i,]
  hmod=hmod_mat[i,]
  vmod=vmod_mat[i,]
  vmax=vmax_mat[i,]
  cvv=cvv_mat[i,]
  nbhex=nbhex_mat[i,]
  
p12_h=rbind(p12_h,exp(vect_a[1] + vect_a[3] * hmod + vect_a[15] * hmod^2)/ 
              (1 +  exp(vect_a[1] + vect_a[3] * hmod + vect_a[15] * hmod^2)))
p21_h=rbind(p21_h,exp(vect_a[2] + vect_a[4] * hmod + vect_a[16] * hmod^2)/ 
              (1 +  exp(vect_a[2] + vect_a[4] * hmod + vect_a[16] * hmod^2)))

p12_v=rbind(p12_v,exp(vect_a[1] + vect_a[5] * vmod + vect_a[17] * vmod^2)/ 
              (1 +  exp(vect_a[1] + vect_a[5] * vmod + vect_a[17] * vmod^2)))
p21_v=rbind(p21_v,exp(vect_a[2] + vect_a[6] * vmod + vect_a[18] * vmod^2)/ 
              (1 +  exp(vect_a[2] + vect_a[6] * vmod + vect_a[18] * vmod^2)))

p12_t=rbind(p12_t,exp(vect_a[1] + vect_a[7] * dftemp + vect_a[19] * dftemp^2)/ 
              (1 +  exp(vect_a[1] + vect_a[7] * dftemp + vect_a[19] * dftemp^2)))
p21_t=rbind(p21_t,exp(vect_a[2] + vect_a[8] * dftemp + vect_a[20] * dftemp^2)/ 
              (1 +  exp(vect_a[2] + vect_a[8] * dftemp + vect_a[20] * dftemp^2)))

p12_cvv=rbind(p12_cvv,exp(vect_a[1] + vect_a[11] * cvv)/(1 +  exp(vect_a[1] + vect_a[11] * cvv)))
p21_cvv=rbind(p21_cvv,exp(vect_a[2] + vect_a[12] * cvv)/(1 +  exp(vect_a[2] + vect_a[12] * cvv)))

p12_vmax=rbind(p12_vmax,exp(vect_a[1] + vect_a[9] * vmax)/(1 +  exp(vect_a[1] + vect_a[9] * vmax)))
p21_vmax=rbind(p21_vmax,exp(vect_a[2] + vect_a[10] * vmax)/(1 +  exp(vect_a[2] + vect_a[10] * vmax)))

p12_nbhex=rbind(p12_nbhex,exp(vect_a[1] + vect_a[13] * nbhex)/(1 +  exp(vect_a[1] + vect_a[13] * nbhex)))
p21_nbhex=rbind(p21_nbhex,exp(vect_a[2] + vect_a[14] * nbhex)/(1 +  exp(vect_a[2] + vect_a[14] * nbhex)))

}

p12_h=p12_h[-1,]
p21_h=p21_h[-1,]
p12_v=p12_v[-1,]
p21_v=p21_v[-1,]
p12_t=p12_t[-1,]
p21_t=p21_t[-1,]
p12_cvv=p12_cvv[-1,]
p21_cvv=p21_cvv[-1,]
p12_vmax=p12_vmax[-1,]
p21_vmax=p21_vmax[-1,]
p12_nbhex=p12_nbhex[-1,]
p21_nbhex=p21_nbhex[-1,]

##### plots couleur espece ####

pdf(paste("proba_vs_vareviro_1d_tous_ind_sp.pdf",sep=""), width=10, height =10)
par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

plot(dftemp_mat[1,], p12_t[1,], xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Upstream temperature difference", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20,  "#619CFF", "#F8766D"))
  lines(dftemp_mat[i,], p12_t[i,], lwd=1.5, col=col_sp)
}
plot(dftemp_mat[1,], p21_t[1,], xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Upstream temperature difference", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){  
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20, "#619CFF", "#F8766D"))
  lines(dftemp_mat[i,], p21_t[i,], lwd=1.5, col=col_sp)
 }

plot(vmod_mat[1,], p12_v[1,], xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Flow velocity", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){  
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20,  "#619CFF", "#F8766D"))
  lines(vmod_mat[i,], p12_v[i,], lwd=1.5, col=col_sp)
}
plot(vmod_mat[1,], p21_v[1,], xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Flow velocity", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20, "#619CFF", "#F8766D"))
  lines(vmod_mat[i,], p21_v[i,], lwd=1.5, col=col_sp)
}

plot(hmod_mat[1,], p12_h[1,], xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Water depth", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20,  "#619CFF", "#F8766D"))
  lines(hmod_mat[i,], p12_h[i,], lwd=1.5, col=col_sp)
}
plot(hmod_mat[1,], p21_h[1,], xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Water depth", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20, "#619CFF", "#F8766D"))
  lines(hmod_mat[i,], p21_h[i,], lwd=1.5, col=col_sp)
}
legend("topleft", legend = c("BAF", "SIL", "CHE"), col = c("#00BA38","#F8766D","#619CFF"), lwd=2)


plot(vmax_mat[1,], p12_vmax[1,], xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Maximum flow speed", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20,  "#619CFF", "#F8766D"))
  lines(vmax_mat[i,], p12_vmax[i,], lwd=1.5, col=col_sp)
}
plot(vmax_mat[1,], p21_vmax[1,], xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Maximum flow speed", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20, "#619CFF", "#F8766D"))
  lines(vmax_mat[i,], p21_vmax[i,], lwd=1.5, col=col_sp)
}

plot(cvv_mat[1,], p12_cvv[1,], xlim=c(varenviro_tous[1,12],varenviro_tous[2,12]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Speed coefficient of variation", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20,  "#619CFF", "#F8766D"))
  lines(cvv_mat[i,], p12_cvv[i,], lwd=1.5, col=col_sp)
}
plot(cvv_mat[1,], p21_cvv[1,], xlim=c(varenviro_tous[1,12],varenviro_tous[2,12]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Speed coefficient of variation", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20, "#619CFF", "#F8766D"))
  lines(cvv_mat[i,], p21_cvv[i,], lwd=1.5, col=col_sp)
}

plot(nbhex_mat[1,], p12_nbhex[1,], xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Exondation hours", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20,  "#619CFF", "#F8766D"))
  lines(nbhex_mat[i,], p12_nbhex[i,], lwd=1.5, col=col_sp)
}
plot(nbhex_mat[1,], p21_nbhex[1,], xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Exondation hours", ylab="", las=1)
for (i in c(1:5,8,10:15,21:28,32)){  #c(1:15,17,20:34,37)){
  col_sp=ifelse(i<10, "#00BA38", ifelse(i>20, "#619CFF", "#F8766D"))
  lines(nbhex_mat[i,], p21_nbhex[i,], lwd=1.5, col=col_sp)
}
legend("topleft", legend = c("BAF", "SIL", "CHE"), col = c("#00BA38","#F8766D","#619CFF"), lwd=2)

dev.off()

############## plot couleurs groupes #######
col_groupe=c("#F8766D","#7CAE00", "#00BFC4","#C77CFF")

pdf(paste("proba_vs_vareviro_1d_tous_ind_gp.pdf",sep=""), width=10, height =10)
par(mfrow=c(3,2),mar = par("mar") + c(0, 1, 0, 0))

plot(dftemp_mat[1,], p12_t[1,], xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Upstream temperature difference", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(dftemp_mat[i,], p12_t[i,], lwd=1.5, col=col_sp)
}
plot(dftemp_mat[1,], p21_t[1,], xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Upstream temperature difference", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(dftemp_mat[i,], p21_t[i,], lwd=1.5, col=col_sp)
}

plot(vmod_mat[1,], p12_v[1,], xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Flow velocity", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(vmod_mat[i,], p12_v[i,], lwd=1.5, col=col_sp)
}
plot(vmod_mat[1,], p21_v[1,], xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Flow velocity", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(vmod_mat[i,], p21_v[i,], lwd=1.5, col=col_sp)
}

plot(hmod_mat[1,], p12_h[1,], xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Water depth", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(hmod_mat[i,], p12_h[i,], lwd=1.5, col=col_sp)
}
plot(hmod_mat[1,], p21_h[1,], xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Water depth", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(hmod_mat[i,], p21_h[i,], lwd=1.5, col=col_sp)
}


plot(vmax_mat[1,], p12_vmax[1,], xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Maximum flow speed", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(vmax_mat[i,], p12_vmax[i,], lwd=1.5, col=col_sp)
}
plot(vmax_mat[1,], p21_vmax[1,], xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Maximum flow speed", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(vmax_mat[i,], p21_vmax[i,], lwd=1.5, col=col_sp)
}

plot(cvv_mat[1,], p12_cvv[1,], xlim=c(varenviro_tous[1,12],varenviro_tous[2,12]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Speed coefficient of variation", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(cvv_mat[i,], p12_cvv[i,], lwd=1.5, col=col_sp)
}
plot(cvv_mat[1,], p21_cvv[1,], xlim=c(varenviro_tous[1,12],varenviro_tous[2,12]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Speed coefficient of variation", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(cvv_mat[i,], p21_cvv[i,], lwd=1.5, col=col_sp)
}

plot(nbhex_mat[1,], p12_nbhex[1,], xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(0,1), type="n",
     main="P(R->M), other variables=0", xlab="Exondation hours", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(nbhex_mat[i,], p12_nbhex[i,], lwd=1.5, col=col_sp)
}
plot(nbhex_mat[1,], p21_nbhex[1,], xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(0,1), type="n",
     main="P(M->R), other variables=0", xlab="Exondation hours", ylab="", las=1)
for (i in c(1:15,17,20:34,37)){
  if (i %in% c(14,29,8,30,37)){col_sp=col_groupe[1]}
  if (i %in% c(31:34,20,17,7,6,9,2)){col_sp=col_groupe[2]}
  if (i %in% c(11,12,21,25,27,28)){col_sp=col_groupe[3]}
  if (i %in% c(1,3:5,23,26,10,13,22,24,15)){col_sp=col_groupe[4]}
  lines(nbhex_mat[i,], p21_nbhex[i,], lwd=1.5, col=col_sp)
}

dev.off()


############# plot groupes separes #####
resultats_kmeans=read.table(file="/home/dominique.lamonica/Bureau/classif_01_02/resultats_kmeans_1000rep.csv"
                              , header=T, sep=",")[,-1]

col_sp=c("#F8766D","#7CAE00", "#00BFC4","#C77CFF")

nb_gp=4
gplist=list()
for (j in 1:nb_gp){
  gplist[[j]]=resultats_kmeans[resultats_kmeans$groupe==j,]$index_numpoiss
}

pdf(paste("proba_vs_vareviro_1d_tous_ind_gp.pdf",sep=""), width=10, height =10)
par(mfrow=c(2,4),mar = par("mar") + c(0, 1, 0, 0))

for (j in 1:nb_gp){
  plot(dftemp_mat[1,], p12_t[1,], xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(0,1), type="n",
       main="P(R->M), other variables=0", xlab="Upstream temperature difference", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(dftemp_mat[i,], p12_t[i,], lwd=1.5, col=col_sp[j])
  }}

for (j in 1:nb_gp){
  plot(dftemp_mat[1,], p21_t[1,], xlim=c(varenviro_tous[1,14],varenviro_tous[2,14]), ylim=c(0,1), type="n",
       main="P(M->R), other variables=0", xlab="Upstream temperature difference", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(dftemp_mat[i,], p21_t[i,], lwd=1.5, col=col_sp[j])
  }}

for (j in 1:nb_gp){
  plot(vmod_mat[1,], p12_v[1,], xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]), ylim=c(0,1), type="n",
       main="P(R->M), other variables=0", xlab="Flow velocity", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(vmod_mat[i,], p12_v[i,], lwd=1.5, col=col_sp[j])
  }}

for (j in 1:nb_gp){
  plot(vmod_mat[1,], p21_v[1,], xlim=c(varenviro_tous[1,11],varenviro_tous[2,11]), ylim=c(0,1), type="n",
       main="P(M->R), other variables=0", xlab="Flow velocity", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(vmod_mat[i,], p21_v[i,], lwd=1.5, col=col_sp[j])
  }}

for (j in 1:nb_gp){
  plot(hmod_mat[1,], p12_h[1,], xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]), ylim=c(0,1), type="n",
       main="P(R->M), other variables=0", xlab="Water depth", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(hmod_mat[i,], p12_h[i,], lwd=1.5, col=col_sp[j])
  }}
for (j in 1:nb_gp){
  plot(hmod_mat[1,], p21_h[1,], xlim=c(varenviro_tous[1,10],varenviro_tous[2,10]), ylim=c(0,1), type="n",
       main="P(M->R), other variables=0", xlab="Water depth", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(hmod_mat[i,], p21_h[i,], lwd=1.5, col=col_sp[j])
  }}

for (j in 1:nb_gp){
  plot(vmax_mat[1,], p12_vmax[1,], xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(0,1), type="n",
       main="P(R->M), other variables=0", xlab="Maximum flow speed", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(vmax_mat[i,], p12_vmax[i,], lwd=1.5, col=col_sp[j])
  }}
for (j in 1:nb_gp){
  plot(vmax_mat[1,], p21_vmax[1,], xlim=c(varenviro_tous[1,13],varenviro_tous[2,13]), ylim=c(0,1), type="n",
       main="P(M->R), other variables=0", xlab="Maximum flow speed", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(vmax_mat[i,], p21_vmax[i,], lwd=1.5, col=col_sp[j])
  }}
for (j in 1:nb_gp){
  plot(cvv_mat[1,], p12_cvv[1,], xlim=c(varenviro_tous[1,12],varenviro_tous[2,12]), ylim=c(0,1), type="n",
       main="P(R->M), other variables=0", xlab="Speed coefficient of variation", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(cvv_mat[i,], p12_cvv[i,], lwd=1.5, col=col_sp[j])
  }}
for (j in 1:nb_gp){
  plot(cvv_mat[1,], p21_cvv[1,], xlim=c(varenviro_tous[1,12],varenviro_tous[2,12]), ylim=c(0,1), type="n",
       main="P(M->R), other variables=0", xlab="Speed coefficient of variation", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(cvv_mat[i,], p21_cvv[i,], lwd=1.5, col=col_sp[j])
  }}

for (j in 1:nb_gp){
  plot(nbhex_mat[1,], p12_nbhex[1,], xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(0,1), type="n",
       main="P(R->M), other variables=0", xlab="Exondation hours", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(nbhex_mat[i,], p12_nbhex[i,], lwd=1.5, col=col_sp[j])
  }}
for (j in 1:nb_gp){
  plot(nbhex_mat[1,], p21_nbhex[1,], xlim=c(varenviro_tous[1,18],varenviro_tous[2,18]), ylim=c(0,1), type="n",
       main="P(M->R), other variables=0", xlab="Exondation hours", ylab="", las=1)
  for (i in gplist[[j]]){
    lines(nbhex_mat[i,], p21_nbhex[i,], lwd=1.5, col=col_sp[j])
  }}

dev.off()


############### plot par individu ttes var ##################
col_var=viridis(6)

gplist=list()
for (j in 1:nb_gp){
  gplist[[j]]=resultats_kmeans[resultats_kmeans$groupe==j,]$index_numpoiss
}

for (j in 1:nb_gp){
pdf(paste("proba_vs_vareviro_1d_par_ind_gp",j,"_v2.pdf",sep=""), width=10, height =10)
par(mfrow=c(2,2),mar = par("mar") + c(0, 1, 0, 0))

for (i in gplist[[j]]){
  
  plot(dftemp_mat[i,], p12_t[i,], xlim=c(-5,5), ylim=c(0,1), col=col_var[1], lty=1,type="l", lwd=2,
       main=paste("P(R->M) ",id_sp_bienloc[i]), xlab="Environmental variables", ylab="", las=1)
  lines(vmod_mat[i,], p12_v[i,], lwd=2, col=col_var[2], lty=1)
  lines(hmod_mat[i,], p12_h[i,], lwd=2, col=col_var[3], lty=1)
  lines(vmax_mat[i,], p12_vmax[i,], lwd=2, col=col_var[4], lty=1)
  lines(cvv_mat[i,], p12_cvv[i,], lwd=2, col=col_var[5], lty=1)
  lines(nbhex_mat[i,], p12_nbhex[i,], lwd=2, col=col_var[6], lty=1)
  
  legend("topleft", legend=c("Temp. diff.", "Flow speed", "Depth", "Max speed","Speed coeff var.",
                             "Exondation h.")  #, "P(R->M)", "P(M->R)")
         # , col=c(col_var, "grey", "grey"),lty=c(rep(1,7), 2), 
         , col=c(col_var),lty=rep(1,6), 
         
         lwd=2,bty = "n")

  plot(dftemp_mat[i,], p21_t[i,], xlim=c(-5,5), ylim=c(0,1), col=col_var[1], lty=1,type="l", lwd=2,
       main=paste("P(M->R) ",id_sp_bienloc[i]), xlab="Environmental variables", ylab="", las=1)
  lines(vmod_mat[i,], p21_v[i,], lwd=2, col=col_var[2], lty=1)
  lines(hmod_mat[i,], p21_h[i,], lwd=2, col=col_var[3], lty=1)
  lines(vmax_mat[i,], p21_vmax[i,], lwd=2, col=col_var[4], lty=1)
  lines(cvv_mat[i,], p21_cvv[i,], lwd=2, col=col_var[5], lty=1)
  lines(nbhex_mat[i,], p21_nbhex[i,], lwd=2, col=col_var[6], lty=1)
  
  #abline(h=exp(vect_a12[i])/(1+exp(vect_a12[i])), lty=1, lwd=1.5)
  #abline(h=exp(vect_a21[i])/(1+exp(vect_a21[i])), lty=2, lwd=1.5)
  
}

dev.off()

}


