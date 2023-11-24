#######################################################################################
#######################################################################################
# Script 8_2 - Figures S1,3,4ab,5,6
#######################################################################################
#######################################################################################

numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)

####gestion des sd et moy pour naviguer entre varenviro standardisees et vraies valeurs
load("/home/lamonica/Documents/poissons/redaction_papier_poisson_deux/leftovers (copie)/analyse/data_pr_std")
data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)


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

pdf("param_mvt_boxplot_2023.pdf", height = 10, width = 10)
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

pdf("param_mvt_boxplot_v2_2023.pdf", height = 6, width = 12)
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

pdf("param_intercept_boxplot_2023.pdf", height = 10, width = 10)
plot_coeff_intercept
dev.off()



########################################
#####boxplots des coeff effets fixes Figure S2####
########################################
load(file="outputs/df_ggplot_coeff_nyct")
##################anova test################
library(rstatix)
#pour prm
for (i in 1:length(id_sp)){
  temp=df_coeff_nyct[df_coeff_nyct$individu==id_sp[i] & df_coeff_nyct$proba=="p(R->M)",]
  print(id_sp[i])
  print(anova_test(temp, yvalues_coeff~ nyct))
  print(tukey_hsd(temp, yvalues_coeff~ nyct))
}

#pour pmr
for (i in 1:length(id_sp)){
  temp=df_coeff[df_coeff_nyct$individu==id_sp[i]  & df_coeff_nyct$proba=="p(M->R)",]
  print(id_sp[i])
  print(anova_test(temp, yvalues_coeff~ nyct))
  print(tukey_hsd(temp, yvalues_coeff~ nyct))
}

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

pdf("effets_fixes_coeff_boxplot_2023.pdf", height = 14, width = 6)
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


pdf("effets_enviro_coeff_boxplot_2023.pdf", height = 20, width = 5)
grid.arrange(plot_coeff_prm,plot_coeff_pmr,ncol=1, nrow = 2)
dev.off()




##########################
#####plots S4a,b ######
##########################

############le plot#################################
load("outputs/df_ggplot_mediane")

plots <- ggplot(data=df_mediane, aes(x=xvalues, y=yvalues , color =group#color = species, linetype=groupe 
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


pdf("figure3_jour_2023.pdf", height = 20, width = 8)
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


pdf("figure3bis_jour_2023.pdf", height = 20, width = 8)
plots
dev.off()

#######################################################################

load("outputs/df_ggplot_replicats")

##############################################
#####plots replicats fig S6 ######
##############################################

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

pdf("figure2_derivee_jour_2023.pdf", height = 15, width = 15)
grid.arrange(plot_derivee[[4]],plot_derivee[[3]],plot_derivee[[6]],
             plot_derivee[[5]],plot_derivee[[2]],plot_derivee[[1]], 
             ncol=2, nrow = 3)
dev.off()


##############################################
#####plots replicats fig S5 (une version vraies valeurs + une standardisees) ######
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


pdf("figure2_proba_jour_truevalues_2023.pdf", height = 15, width = 15)
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


pdf("figure2_proba_jour_std_2023.pdf", height = 15, width = 15)
grid.arrange(plot_proba_std[[4]],plot_proba_std[[3]],plot_proba_std[[6]],
             plot_proba_std[[5]],plot_proba_std[[2]],plot_proba_std[[1]], ncol=2, nrow = 3)
dev.off()
