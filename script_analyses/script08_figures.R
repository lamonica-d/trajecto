#######################################################################################
#######################################################################################
# Script 8 - Figures 3 + supplementary + divers
#######################################################################################
#######################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(patchwork)
library(grid)

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
xvalues_plot=seq(-1.5,1.5,0.2)
varenviroxproba1=c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                   "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)")
varenviroxproba=as.factor(rep(unlist(lapply(X=varenviroxproba1, FUN=rep, times=length(xvalues_plot))),length(id_sp)))


##Figure 3
load("outputs/df_ggplot_replicats")

grouped_df <- df_replicats %>%
  group_by(j,xvalues,xtruevalues,group) %>%
  summarize(q50=median(yvalues_proba),
            q2.5=quantile(yvalues_proba,probs=0.025),
            q97.5=quantile(yvalues_proba,probs=0.975)) %>%
  ungroup() %>%
  mutate(xtitle=sapply(as.character(j), function(x) {
    strsplit(x, ", ")[[1]][1]}),
    ytitle=sapply(as.character(j), function(x) {
      strsplit(x, ", ")[[1]][2]}))

panel_title_y <- as_labeller(
  c(`Flow velocity` = "(a) Flow velocity (m/s)", 
    `Upstream temperature diff` = "(b) Temperature difference (°C)",
    `Water depth` = "(c) Water depth (m)",
    `p(M->R)` = "Switching probability Moving to Resting",
    `p(R->M)` = "Switching probability Resting to Moving")
)

fig3 <- ggplot(grouped_df, aes(x=xtruevalues,y=q50,col=as.factor(group), fill=as.factor(group))) +
  geom_line() + 
  geom_ribbon(aes(ymin=q2.5, ymax = q97.5),col=NA, alpha =.3)+
  facet_grid(ytitle~xtitle, scales="free_x",
             labeller = panel_title_y)+
  xlab('')+ylab('')+
  theme_bw()+
  scale_fill_viridis_d("cluster")+
  scale_color_viridis_d("cluster")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

fig3 

pdf(paste("figures/figure3.pdf", sep=""),width=8,height=8)
fig3
graphics.off()


#########################################
###      boxplots des coeff mvt Figure S1   ###
#########################################
#####plot###############
load(file="outputs/df_ggplot_mvt")

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

pdf("figures/figS1.pdf", height = 10, width = 10)
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

pdf("figures/figS1_v2.pdf", height = 10, width = 10)
plot_coeff_mvt2
dev.off()

###########plot des intercepts###########
#####plot###############
load(file="outputs/df_ggplot_intercept")

plot_coeff_intercept<-ggplot()+ 
  geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_ggplot_intercept,width=0.8,lwd=0.4,
               aes(x=individu, y=yvalues, fill=comp ))+ #color=enviro))+ 
  # facet_wrap( ~ param, ncol=4, scales = "free")+
  
  
  labs(y="Intercept, posterior distribution", x="")+ggtitle(expression(paste(alpha)))+
  theme_minimal()+ 
  theme(axis.text.x = element_text( size=8, angle=90))+ 
  guides(fill=guide_legend(""))

pdf("figures/fig_param_intercept.pdf", height = 10, width = 10)
plot_coeff_intercept
dev.off()



########################################
#####boxplots des coeff effets fixes Figure S2####
########################################
load(file="outputs/df_ggplot_coeff_nyct")
##################anova test################
# library(rstatix)
# #pour prm
# for (i in 1:length(id_sp)){
#   temp=df_coeff_nyct[df_coeff_nyct$individu==id_sp[i] & df_coeff_nyct$proba=="p(R->M)",]
#   print(id_sp[i])
#   print(anova_test(temp, yvalues_coeff~ nyct))
#   print(tukey_hsd(temp, yvalues_coeff~ nyct))
# }
# 
# #pour pmr
# for (i in 1:length(id_sp)){
#   temp=df_coeff_nyct[df_coeff_nyct$individu==id_sp[i]  & df_coeff_nyct$proba=="p(M->R)",]
#   print(id_sp[i])
#   print(anova_test(temp, yvalues_coeff~ nyct))
#   print(tukey_hsd(temp, yvalues_coeff~ nyct))
# }

#####plot###############
plot_coeff_prm<-ggplot()+ 
  geom_boxplot(data=df_coeff_nyct[df_coeff_nyct$proba=="p(R->M)",],width=0.8,lwd=1, #1.8
               aes(x=nyct, y=yvalues_coeff, color=nyct))+ 
  facet_wrap( ~ individu, ncol=4)+

  
  labs(y="Fixed effect coefficient, posterior distribution", x="")+ggtitle("p(R->M)")+
  theme_minimal()+  theme(legend.position = "none")

plot_coeff_pmr<-ggplot()+ 
  geom_boxplot(data=df_coeff_nyct[df_coeff_nyct$proba=="p(M->R)",],width=0.8,lwd=1, #1.8
               aes(x=nyct, y=yvalues_coeff, color=nyct))+ 
  facet_wrap( ~ individu, ncol=4)+
  
  labs(y="Fixed effect coefficient, posterior distribution", x="Nycthemeral period")+ggtitle("p(M->R)")+
  theme_minimal()+  theme(legend.position = "none")

pdf("figures/figS2.pdf", height = 14, width = 6)
grid.arrange(plot_coeff_prm,plot_coeff_pmr,ncol=1, nrow = 2)
dev.off()


########################################
#####boxplots des coeff effet enviro Figure S3####
########################################
#####plot###############
load(file="outputs/df_ggplot_coeff_enviro")

plot_coeff_prm<-ggplot()+ 
  geom_abline(slope=0, intercept=0, colour="grey45", lty=2, lwd=0.8)+
  geom_boxplot(data=df_coeff_enviro[df_coeff_enviro$proba=="p(R->M)",],width=0.8,lwd=0.4,
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
  geom_boxplot(data=df_coeff_enviro[df_coeff_enviro$proba=="p(M->R)",],width=0.8,lwd=0.4,
               aes(x=enviro, y=yvalues_coeff, fill=enviro,color=enviro,alpha=type_coeff))+ 
  facet_wrap( ~ individu, ncol=4)+
  
  #scale_color_manual(values=c(""))+
  scale_alpha_manual(values=c(1,0.4)) +
  labs(y="Environmental effect coefficient, posterior distribution", x="")+ggtitle("p(M->R)")+
  theme_minimal()+ 
  theme(legend.position = "none",axis.text.x = element_text( size=8, angle=90))


pdf("figures/figS3.pdf", height = 20, width = 5)
grid.arrange(plot_coeff_prm,plot_coeff_pmr,ncol=1, nrow = 2)
dev.off()


##############################################
#####plots replicats fig S6 ######
##############################################
load("outputs/df_ggplot_replicats")

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

pdf("figures/figS6.pdf", height = 15, width = 15)
grid.arrange(plot_derivee[[4]],plot_derivee[[3]],plot_derivee[[6]],
             plot_derivee[[5]],plot_derivee[[2]],plot_derivee[[1]], 
             ncol=2, nrow = 3)
dev.off()


##############################################
#####plots replicats fig S6 bis (une version vraies valeurs + une standardisees) ######
##############################################

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


pdf("figures/figS6_v2.pdf", height = 15, width = 15)
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


pdf("figures/figS6_v3.pdf", height = 15, width = 15)
grid.arrange(plot_proba_std[[4]],plot_proba_std[[3]],plot_proba_std[[6]],
             plot_proba_std[[5]],plot_proba_std[[2]],plot_proba_std[[1]], ncol=2, nrow = 3)
dev.off()



########################################
#####Figures S5a,b,c,d : 95%IC + barplots###################
#######################################

load("outputs/df_ggplot_replicats")
load("outputs/df_ggplot_ic")

xvalues=unique(df_replicats$xvalues)
varenviroxproba1=as.factor(c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                             "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)"))


#les picto species
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
  pdf(paste("figures/figS5",table_names[k],".pdf", sep=""),width=15,height=15)
  print(plot_ic[[k]])
  for (i in 1:18){
    subvp=viewport(width=0.04,height=0.04,x=x_pos[i],y=y_pos[i])
    subvp2=viewport(width=0.055,height=0.055,x=x_pos_picto[i],y=y_pos_picto[i])
    print(barplot_list[i],vp=subvp)
    print(picto_list[[i]],vp=subvp2)
    
  }
  graphics.off()
  
}


##########################
#####  figures S7   ######
##########################

############le plot#################################
load("outputs/df_ggplot_mediane")

plots <- ggplot(data=df_mediane3, aes(x=xvalues, y=yvalues , color =group#color = species, linetype=groupe 
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


pdf("figures/figS7_v2.pdf", height = 20, width = 8)
plots
dev.off()


####le meme plot mais avec des classes de tailles
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


pdf("figures/figS7.pdf", height = 20, width = 8)
plots
dev.off()


