
#######################################################################################
#######################################################################################
# Script 1 - Preparation de donnees
#######################################################################################
#######################################################################################

prepa1 <- function () {
rm(list=ls())

numpoiss_vect=c(3100,3156,3744,3128,3170,3051,3317,3674,3828,3856,3632,3786,3835,3849,3870,3772,3800,3387,3415,3429,
                3183,3212,3240,3352,3464,3730,3758,3121,3394,3408,3422,3506,3562,3590,3625,3079,3303)


data_comp=read.table(file=paste( numpoiss_vect[1], sep=""), header=T, sep=",")[1,]
data_comp$numpoiss=1

for (i in 1:length(numpoiss_vect)){
  
  temp=read.table(file=paste( numpoiss_vect[i], sep=""), header=T, sep=",") 
  temp$numpoiss=i
  if (i==27){temp=temp[1:8899,]}
  data_comp=rbind(data_comp,temp)
}


data_comp=data_comp[-1,]

#####################gestion des var enviro#############################

##version 2.0######
##1) garder une seule colonne pour la moyenne
data_comp$hmod_moy_ut <- ifelse(is.na(data_comp$hmod_ect)==T, data_comp$hmod.y, data_comp$hmod_moy)
data_comp$vmod_moy_ut <- ifelse(is.na(data_comp$vmod_ect)==T, data_comp$vmod.y, data_comp$vmod_moy)
data_comp$cvv_moy_ut <- ifelse(is.na(data_comp$CVit_ect)==T, data_comp$CVit, data_comp$CVit_moy)
data_comp$vmax_moy_ut <- ifelse(is.na(data_comp$VMax_ect)==T, data_comp$VMax, data_comp$VMax_moy)
data_comp$temp_moy_ut1 <- ifelse(data_comp$Tmoy!=0, data_comp$Tmoy, data_comp$temp)
data_comp$temp_moy_ut <- data_comp$temp_moy_ut1-data_comp$temp_amont

##2)calcul des carres
data_comp$hmod_carre = data_comp$hmod_moy_ut^2
data_comp$vmod_carre = data_comp$vmod_moy_ut^2
data_comp$temp_carre = data_comp$temp_moy_ut^2

##3) standardiser
data_comp$hmod_moy_cr = (data_comp$hmod_moy_ut-mean(data_comp$hmod_moy_ut, na.rm=T))/sd(data_comp$hmod_moy_ut, na.rm=T)
data_comp$vmod_moy_cr = (data_comp$vmod_moy_ut-mean(data_comp$vmod_moy_ut, na.rm=T))/sd(data_comp$vmod_moy_ut, na.rm=T)
data_comp$cvv_moy_cr = (data_comp$cvv_moy_ut-mean(data_comp$cvv_moy_ut, na.rm=T))/sd(data_comp$cvv_moy_ut, na.rm=T)
data_comp$vmax_moy_cr = (data_comp$vmax_moy_ut-mean(data_comp$vmax_moy_ut, na.rm=T))/sd(data_comp$vmax_moy_ut, na.rm=T)
data_comp$temp_moy_cr = (data_comp$temp_moy_ut-mean(data_comp$temp_moy_ut, na.rm=T))/sd(data_comp$temp_moy_ut, na.rm=T)
data_comp$hmod_carre_cr = (data_comp$hmod_carre-mean(data_comp$hmod_carre, na.rm=T))/sd(data_comp$hmod_carre, na.rm=T)
data_comp$vmod_carre_cr = (data_comp$vmod_carre-mean(data_comp$vmod_carre, na.rm=T))/sd(data_comp$vmod_carre, na.rm=T)
data_comp$temp_carre_cr = (data_comp$temp_carre-mean(data_comp$temp_carre, na.rm=T))/sd(data_comp$temp_carre, na.rm=T)

##4) standardiser les vitesses par la taille du poiss
data_comp$vit_taille=data_comp$dist/data_comp$taille


data8=data.frame(id_poiss=data_comp$period,numpoiss=data_comp$numpoiss,sqcont_new=data_comp$sqcont_new,
                 rangle=data_comp$rel.angle, distance=data_comp$vit_taille , 
                 rapport_dist=data_comp$rapport_dist, var_angles_bruts=data_comp$var_angle_3s 
                 ##les var enviro si besoin
                 , vmod=data_comp$vmod_moy_cr, hmod=data_comp$hmod_moy_cr,
                 cvv=data_comp$cvv_moy_cr, vmax=data_comp$vmax_moy_cr,
                 temp=data_comp$temp_moy_cr
                
                 , vmod_carre=data_comp$vmod_carre_cr, hmod_carre=data_comp$hmod_carre_cr,temp_carre=data_comp$temp_carre_cr
                 ,nb_ex_cr=(data_comp$nbhex-mean(data_comp$nbhex, na.rm=T))/sd(data_comp$nbhex, na.rm=T)
                 
)



pdt=60
data8$distance[which(data8$distance==0)]=0.01 ## pour enlever les zeros qui chient la weibull et la gamma
data8$rapport_dist[which(data8$rapport_dist==0)]=0.01
data8$rapport_dist[which(data8$rapport_dist==1)]=0.99
#vitesse en m/
data8$vitesse=data8$distance/pdt
#convertir en radian positif
data8$rangle[data8$rangle<0]=data8$rangle[data8$rangle<0]+2*pi

#enlever des trucs qui chient
data8=data8[-which(data8$sqcont_new==90 & data8$numpoiss==28),]

nbpoiss=length(unique(data8$numpoiss))
poiss=unique(data8$numpoiss)
debut_poiss=sapply(poiss,function(s) min(which(data8$numpoiss==s)))
fin_poiss=sapply(poiss,function(s) max(which(data8$numpoiss==s)))

nbsection1=c(length(poiss))
for (i in 1:length(poiss)){
  temp1=data8[data8$numpoiss==i,]
  nbsection1[i]=length(unique(temp1$sqcont_new))
}

section1=list()
debut_section1=list()
fin_section1=list()
for (i in 1:length(poiss)){
  temp1=data8[data8$numpoiss==i,]
  section1[[i]]=unique(temp1$sqcont_new)
  debut_section1[[i]]=sapply(section1[[i]],function(s) min(which(temp1$sqcont_new==s)))
  fin_section1[[i]]=sapply(section1[[i]],function(s) max(which(temp1$sqcont_new==s)))
}
#enlever les traj a deux points

taille=list()
for (i in 1:length(poiss)){
  taille[[i]]=fin_section1[[i]]-debut_section1[[i]]+1
}

section_sup2=list()
for (i in 1:length(poiss)){
  section_sup2[[i]]=subset(section1[[i]],taille[[i]]>5) #5 normalement, test avec 3
}


##prendre chaque numpoiss et ne garder que les section >5 et les remettre ensemble apres
temp2=data8[data8$numpoiss==1,]
data8_sup2=temp2[temp2$sqcont_new %in% section_sup2[[1]],]

for (i in 2:length(poiss)){
  temp2=data8[data8$numpoiss==i,]
  data8_sup2=rbind(data8_sup2,temp2[temp2$sqcont_new %in% section_sup2[[i]],])
}

write.csv(data8_sup2, file="outputs/data_tous_individus_ready_to_use.csv")

}
