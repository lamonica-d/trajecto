#######################################################################################
#######################################################################################
# Script 8_1 - Figures 2, 3a,b,c,d
#######################################################################################
#######################################################################################


numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)


####gestion des sd et moy pour naviguer entre varenviro standardisees et vraies valeurs
load("data/raw-data/data_pr_std")
data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)

########################################
#####Figure 2 : heatmaps de tous les individus#####
########################################
#load("outputs/df_ggplot_heatmap_encountered_values") #df_heatmap2

# plot_heatmap2<-ggplot()+ 
#   geom_point(data=df_heatmap2,aes(x=env_x_tv, y=env_y_tv, colour=prob), stat="identity")+
#   facet_grid(couple~individu)+
#   scale_color_viridis(limits=c(0,1))+
#   labs(y="Water depth", x="Flow velocity")+
#   # scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
#   theme_minimal()
# 
# pdf(paste("heatmap_vh_valeurs_rencontrees_tv.pdf", sep=""), height = 3, width = 21)
# print(plot_heatmap)
# dev.off()


########################################
#####Figures 3a,b,c,d : 95%IC + barplots###################
#######################################

load("outputs/df_ggplot_replicats")
load("outputs/df_ggplot_ic")

xvalues=unique(df_replicats$xvalues)
varenviroxproba1=as.factor(c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                             "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)"))


#les picto species
library(png)
library(patchwork)

#poiss <- readPNG("/home/dominique.lamonica/Bureau/juin2021_ef_nyct/poissons.png")
baf  <- readPNG("data/raw-data/barbeau.png", native=T)
che  <- readPNG("data/raw-data/chevesne.png", native=T)
sil  <- readPNG("data/raw-data/silure.png", native=T)

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
load("outputs/tables_list")

for (k in 1:6){
  
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
  pdf(paste("ic_and_percent",table_names[k],".pdf", sep=""),width=15,height=15)
  print(plot_ic[[k]])
  for (i in 1:18){
    subvp=viewport(width=0.04,height=0.04,x=x_pos[i],y=y_pos[i])
    subvp2=viewport(width=0.055,height=0.055,x=x_pos_picto[i],y=y_pos_picto[i])
    print(barplot_list[i],vp=subvp)
    print(picto_list[[i]],vp=subvp2)
    
  }
  graphics.off()
  
}





