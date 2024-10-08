#######################################################################################
#######################################################################################
#  Generation of dataframes for heatmaps + plot heatmaps
#######################################################################################
#######################################################################################

library(plot3D)
library(cowplot)

## load and set data
load("outputs/tables_list")
load("outputs/clust_list")
load("outputs/gp_list_list")
load("outputs/nb_gp_list")
load("outputs/matrice_curves_list")
load("outputs/matrice_proba_curves_list")
load("outputs/mtot_list")

param_names=colnames(mtot_list[[1]])
index_lin=1:6
index_carre=7:12
replicats=200
nb_classe=sapply(tables_list, FUN = ncol)-1  
xvalues_plot <- xvalues <- seq(-1.5,1.5,0.2)

numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)
id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")
vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)

varenviroxproba1=c("Water depth, p(R->M)","Water depth, p(M->R)","Flow velocity, p(R->M)","Flow velocity, p(M->R)",
                   "Upstream temperature diff, p(R->M)","Upstream temperature diff, p(M->R)")

## load mean and sd used for standardisation of environmental covariables 
## to navigate between standardised and true values
load("data/raw-data/data_pr_std")
data_pour_standardiser=data.frame(data_pour_standardiser)
moy_vect=c(data_pour_standardiser$moy_h,data_pour_standardiser$moy_v,data_pour_standardiser$moy_t)
sd_vect=c(data_pour_standardiser$sd_h,data_pour_standardiser$sd_v,data_pour_standardiser$sd_t)
mu=0
sd=1

## valeurs extremes rencontres par les individus
varenviro_tous=read.table(file="data/raw-data/min_max_mean_var_enviro_tous_ind.csv", header=T, sep=",")[,-1]
min_ind=read.table(file="data/raw-data/min_var_enviro_tous_par_ind.csv", header=T, sep=",")[,-1]
max_ind=read.table(file="data/raw-data/max_var_enviro_tous_par_ind.csv", header=T, sep=",")[,-1]
mean_ind=read.table(file="data/raw-data/mean_var_enviro_tous_par_ind.csv", header=T, sep=",")[,-1]

## choix nyct period var nyct_i aube 7, crep 8, jour 9, nuit 10
nyct_i=9
## si on veut la gamme de valeurs rencontrees par le poisson true_env=T
true_env=T
## nombre de cases de cote
size_map=20


## 1) generate dataframes
# df_heatmap=data.frame(individu=as.factor(id_sp[1]), env_x=as.vector(NA),env_y=as.vector(NA),prob=as.vector(NA)
#                       ,couple=as.factor("p(R->M)"))
# 
# for (i in 1:18){
#   temp=mtot_list[[i]]
#   vect_a_p12=as.numeric()
#   vect_a_p21=as.numeric()
#   for (j in 17:length(temp[1,])){
#     param_quantile=as.numeric(quantile(temp[,j],probs = 0.5))
#     if (is.odd(j)==T) {vect_a_p12=c(vect_a_p12,param_quantile)}else{
#       vect_a_p21=c(vect_a_p21,param_quantile)
#     }
#   }
#   mat_coeff=matrix(data=c(vect_a_p12,vect_a_p21), ncol=2, byrow = F)
#   rm(temp)
#   
#   if (true_env==T){
#     dftemp=seq(min_ind[i,5],max_ind[i,5],length.out=size_map)
#     hmod=seq(min_ind[i,3],max_ind[i,3],length.out=size_map)
#     vmod=seq(min_ind[i,1],max_ind[i,1],length.out=size_map)
#     
#     mu_h=data_pour_standardiser$moy_h
#     mu_v=data_pour_standardiser$moy_v
#     mu_t=data_pour_standardiser$moy_t
#     
#     sd_h=data_pour_standardiser$sd_h
#     sd_v=data_pour_standardiser$sd_v
#     sd_t=data_pour_standardiser$sd_t
#     
#   }else{
#     
#     dftemp=seq(-1.5,1.5,length.out=size_map)
#     hmod=seq(-1.5,1.5,length.out=size_map)
#     vmod=seq(-1.5,1.5,length.out=size_map)
#     
#     mu_h=0
#     mu_v=0
#     mu_t=0
#     
#     sd_h=1
#     sd_v=1
#     sd_t=1
#   }
#   
#   mat_vh=mesh(vmod,hmod)
#   p_vh=list()
#   df_vh=list()
#   for (j in 1:2){
#     p_vh[[j]]=with(mat_vh,(exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * ((y-mu_h)/sd_h) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[5,j] * ((y-mu_h)/sd_h)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)/
#                              (1 +  exp(mat_coeff[1,j] +mat_coeff[nyct_i,j] + mat_coeff[2,j] * ((y-mu_h)/sd_h) + mat_coeff[3,j] * ((x-mu_v)/sd_v) + mat_coeff[5,j] * ((y-mu_h)/sd_h)^2 + mat_coeff[6,j] * ((x-mu_v)/sd_v)^2)))
#     )
#     #x v 3,,6
#     #y h 2,,5
#     if (j==1){couple_nom="p(R->M)"}else{couple_nom="p(M->R)"}
#     df_vh[[j]]=data.frame(individu=as.factor(rep(id_sp[i],length(as.vector(t(p_vh[[j]]))))), env_x=as.vector(t(mat_vh[[1]])),env_y=as.vector(t(mat_vh[[2]])),prob=as.vector(t(p_vh[[j]]))
#                           ,couple=as.factor(rep(couple_nom,length(as.vector(t(p_vh[[j]]))))))
#   }
#   
#   #un seul df
#   df_heatmap=rbind(df_heatmap,df_vh[[1]],df_vh[[2]])
# }#i
# 
# save(df_heatmap, file = "outputs/df_ggplot_heatmap_all_values")

## with only environemental conditions encountered by each fish
data1=read.table(file="data/derived-data/data_tous_individus_ready_to_use.csv", header=T, sep=",")[,-1]

df_heatmap=data.frame(individu=as.factor(id_sp[1]), env_x=as.vector(NA),env_y=as.vector(NA),
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
  enviro=data1[data1$id_poiss==numpoiss_vect0[i],c(1,8,9,10)]
  enviro_non_std=data.frame(vmod_tv=enviro$vmod*data_pour_standardiser$sd_v+data_pour_standardiser$moy_v,
                            hmod_tv=enviro$hmod*data_pour_standardiser$sd_h+data_pour_standardiser$moy_h)
  
  ###########calcul des proba et df################
  df_vh=list()
  
  for (j in 1:2){
    
    p_vh1=as.numeric()
    
    for (e in 1:length(enviro[,1])){
      
      p_vh1[e]=exp(mat_coeff[nyct_i,j] + mat_coeff[2,j] * enviro[e,3] + mat_coeff[3,j] * enviro[e,2] + mat_coeff[5,j] * enviro[e,3]^2 + mat_coeff[6,j] * enviro[e,2]^2)/
        (1 +  exp(mat_coeff[nyct_i,j] + mat_coeff[2,j] * enviro[e,3] + mat_coeff[3,j] * enviro[e,2] + mat_coeff[5,j] * enviro[e,3]^2 + mat_coeff[6,j] * enviro[e,2]^2))
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
  df_heatmap=rbind(df_heatmap,df_vh[[1]],df_vh[[2]])
}#i  
df_heatmap <- df_heatmap[-1,]
save(df_heatmap, file = "outputs/df_ggplot_heatmap_encountered_values")

## load dataframe
load("outputs/df_ggplot_heatmap_encountered_values")

#if (true_env==T){
plot_heatmap<-ggplot()+ 
  geom_point(data=df_heatmap,aes(x=env_x_tv, y=env_y_tv, colour=prob), stat="identity")+
  facet_grid(couple~individu)+
  scale_color_viridis(limits=c(0,1))+
  labs(y="Water depth", x="Flow velocity")+
  theme_minimal()

pdf(paste("figures/heatmap_vh_valeurs_rencontrees.pdf", sep=""), height = 3, width = 21)
print(plot_heatmap)
dev.off()

# }else{

#    plot_heatmap=list()
#    for (i in 1:18){
#      
#      if (i==1|i==5|i==9|i==13){plot_heatmap[[i]]<-ggplot()+ 
#        geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
#        facet_grid(couple~.)+
#        scale_fill_viridis(discrete=F,limits=c(0,1))+
#        #scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
#        labs(y="Water depth", x="")+ggtitle(id_sp[i])+
#        theme_minimal()+theme(legend.position = "none")
#      
#      }
#      
#      if (i==15|i==16|i==18){
#        plot_heatmap[[i]]<-ggplot()+ 
#          geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
#          facet_grid(couple~.)+
#          scale_fill_viridis(discrete=F,limits=c(0,1))+
#          labs(y="", x="Flow velocity")+ggtitle(id_sp[i])+
#          scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
#          theme_minimal()+theme(legend.position = "none")
#      }
#      if (i==17){
#        plot_heatmap[[i]]<-ggplot()+ 
#          geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
#          facet_grid(couple~.)+
#          scale_fill_viridis(discrete=F, limits=c(0,1))+
#          labs(y="Water depth", x="Flow velocity")+ggtitle(id_sp[i])+
#          scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
#          theme_minimal()+theme(legend.position = "none")
#      }
#      
#      if (i==2|i==3|i==4|i==6|i==7|i==8|i==10|i==11|i==12|i==14){
#        plot_heatmap[[i]]<-ggplot()+ 
#          geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
#          facet_grid(couple~.)+
#          scale_fill_viridis(discrete=F, limits=c(0,1))+
#          labs(y="", x="")+ggtitle(id_sp[i])+
#          scale_y_continuous(limits=c(-2,4))+scale_x_continuous(limits=c(-2,4))+
#          theme_minimal()+theme(legend.position = "none")
#      }
#    }
#    
#    plot_heatmap_leg<-ggplot()+ 
#      geom_tile(data=df_heatmap[df_heatmap$individu==id_sp[i],],aes(x=env_x, y=env_y, fill=prob))+
#      facet_grid(couple~.)+
#      scale_fill_viridis(discrete=F, limits=c(0,1))+
#      theme_minimal()
#    
#    leg <- get_legend(plot_heatmap_leg)
#    
#    pdf(paste("heatmap_vh_valeurs_rencontrees.pdf", sep=""), height = 15, width = 15)
#    grid.arrange(plot_heatmap[[1]],plot_heatmap[[2]],plot_heatmap[[3]],plot_heatmap[[4]],plot_heatmap[[5]],plot_heatmap[[6]], 
#                 plot_heatmap[[7]],plot_heatmap[[8]],plot_heatmap[[9]],plot_heatmap[[10]],plot_heatmap[[11]],plot_heatmap[[12]],
#                 plot_heatmap[[13]],plot_heatmap[[14]],plot_heatmap[[15]],plot_heatmap[[16]],plot_heatmap[[17]],plot_heatmap[[18]],
#                 leg,ncol=4)
#    dev.off()
# # }


#######################################################################################
#######################################################################################
load(file="outputs/df_ggplot_coeff_nyct")
#################anova test################
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
  temp=df_coeff_nyct[df_coeff_nyct$individu==id_sp[i]  & df_coeff_nyct$proba=="p(M->R)",]
  print(id_sp[i])
  print(anova_test(temp, yvalues_coeff~ nyct))
  print(tukey_hsd(temp, yvalues_coeff~ nyct))
}


##################interpolating NAs in environmental covariables#######################
#sil
if (id_poiss_int==3786){data_ind$temp[1315]=data_ind$temp[1314]
}
if (id_poiss_int==3835){data_ind$temp[11712]=data_ind$temp[11711]
}
if (id_poiss_int==3849){data_ind$temp[4043]=data_ind$temp[4042]
}
if (id_poiss_int==3387){data_ind$temp[c(73,74,182:185,207,211:214)]=
  data_ind$temp[c(72,72,rep(181,4),206,rep(210,4))]
}

#baf
if (id_poiss_int==3128){data_ind$temp[7363]=data_ind$temp[7362]
data_ind$hmod[7363]=data_ind$hmod[7362]
data_ind$vmod[7363]=data_ind$vmod[7362]

}

if (id_poiss_int==3170){data_ind$temp[c(1034,1236,1706,1707,9957,9967)]=data_ind$temp[c(1034,1236,1706,1706,9957,9967)-1]
data_ind$hmod[c(9957,9967)]=data_ind$hmod[c(9957,9967)-1]
data_ind$vmod[c(9957,9967)]=data_ind$vmod[c(9957,9967)-1]

}

if (id_poiss_int==3744){data_ind$temp[c(93,96,99,103,104,129,132,142,146,150,151,174,175,184,190:192,195,198,199,218,221,230,233,236,239,245,263,264,272:275,1316,1323,1324,1344,1345,
                                        1349,1406,1442,1454,1484,1770,1774,1791:1793,1808,1809,1826,1827,1848,1849,1860,1870:1872,1895,1907,1913,1919,1931:1933,1937:1940,1943,
                                        1951,1964,1965,1968,1969,1972,1973,4310:4314,4317,4318,4321,4335,4346:4349,4353,4360,4393,4397,4398,4402,4440,4448,4449,4457:4459,4465,4466,
                                        4477,4501,4502,4525,4530,4536,4543:4549,4554:4557,4562,4565,4568,4569,4576:4579,4593:4596,4603:4606,4609,4615,4616,4677,4678,4681:4684,4731,
                                        4765,4768,4793,4811,4826,4843,4848,4853,4868,4871,4892,4898,4905,4912,4913,4929:4931,4952,4966,4973,4974,4978,4992:4995,5002:5006,5162,5171:5174,
                                        5186:5189,5194,5199,5206,5209,5223,5227,5228,5242,5246:5248,5253,5254,5258,5696,5697,5741,5862,5880,6379,6454,6455,6678:6681,6690,6693:6698,6710,
                                        6714,6739,6740,6886,6934,6938:6941,7177,7369,7420,7493,7501,7502,7521,7541,7563,7599,7614,7717:7719,7729,7737,7773,7869,7983,8006,8015,8022,8090,
                                        8155,8185,8200,8317,8322,8366,8391,8394,8395,8399,8494,8508,8512,8929,8930,8985,8998,9689,9729,12694,12695,12698:12700,12923,13053,13318,13382,
                                        13722,16596,17489,17576:17578,17601,17608,20877,20891,20894,21057,21261,21554,23169,23696,24305,24391,24395,24398,24399,24402,24420,24421,24568,
                                        24805,25593,27167,27801,28477,28495:28498,33738,34177,34247,34365,43275,43524,43571,43572,43576:43578)]=
  data_ind$temp[c(c(93,96,99,103)-1,102,c(129,132,142,146,150)-1,149,173,173,183,rep(189,3),194,197,197,c(218,221,230,233,236,239,245,263)-1,262,rep(271,4),1315,1322,1322,1343,1343,
                  c(1349,1406,1442,1454,1484,1770,1774)-1,rep(1790,3),1807,1807,1825,1825,1847,1847,1859,rep(1869,3),c(1895,1907,1913,1919)-1,rep(1930,3),rep(1936,4),c(1943,
                                                                                                                                                                        1951,1964)-1,1963,1967,1967,1971,1971,rep(4309,5),4316,4316,4320,4334,rep(4345,4),c(4353,4360,4393,4397)-1,4396,c(4402,4440,4448)-1,4447,
                  rep(4456,3),4464,4464,4476,4500,4500,c(4525,4530,4536)-1,rep(4542,7),rep(4553,4),4561,4564,4567,4567,rep(4575,4),rep(4592,4),rep(4602,4),4608,4614,4614,4676,4676,rep(4680,4)
                  ,c(4731,4765,4768,4793,4811,4826,4843,4848,4853,4868,4871,4892,4898,4905,4912)-1,4911,rep(4928,3),4951,4965,4972,4972,4977,rep(4991,4),rep(5001,5),5161,rep(5170,4),
                  rep(5185,4),c(5194,5199,5206,5209,5223,5227)-1,5226,5241,rep(5245,3),5252,5252,5257,5695,5695,c(5741,5862,5880,6379,6454)-1,6453,rep(6677,4),6689,rep(6692,6),
                  c(6710,6714,6739)-1,6738,6885,6933,rep(6937,4),c(7177,7369,7420,7493,7501)-1,7500,c(7521,7541,7563,7599,7614)-1,rep(7716,3),c(7729,7737,7773,7869,7983,8006,8015,8022,8090,
                                                                                                                                                8155,8185,8200,8317,8322,8366,8391,8394)-1,8393,c(8399,8494,8508,8512,8929)-1,8928,c(8985,8998,9689,9729,12694)-1,12693,rep(12697,3),c(12923,13053,13318,13382,
                                                                                                                                                                                                                                                                                       13722,16596,17489)-1,rep(17575,3),c(17601,17608,20877,20891,20894,21057,21261,21554,23169,23696,24305,24391,24395,24398)-1,24397,24401,24419,24419,c(24568,
                                                                                                                                                                                                                                                                                                                                                                                                                                            24805,25593,27167,27801,28477)-1,rep(28494,4),c(33738,34177,34247,34365,43275,43524,43571)-1,43570,rep(43575,3))]



data_ind$hmod[c(299,1968,4353,4393,4993,5157,5162,5163,5171:5174,5186:5189,5194,5198,5199,5204:5206,5209:5213,5223,5224,5227,5228,5241,5242,5246:5248,5253:5255,5258,
                5862,5879,5880,6454,6455,6678:6681,6685,6690:6700,6709,6710,6714,6739,6740,6835,6858,6886,6934,6938:6941,6948,7082,7088,7177,7369,7718,7719,7729,7737,
                7749:7751,7754,7773,7869,8022,8366,8494,8508,8512,8924,8929:8933,8985,8998:9000,9232,9729,9973,9974,9979,10121,10330,10709,10742,10743,11991,12052,12056,
                12061,12064,12065,12843:12846,12854,12859,12919:12923,12929,12930,12982,12985,12990,13053,13059,13721,13722,13773,13941,14041,14042,14244,14245,14252,14775,
                14778,15460,15461,16593:16596,16609:16618,17576,17577,17671:17673,17678,17679,17702,17731,17748,17778,17785,17804,17808,17815,17820,17852:17855,17858,17861:17864,
                17898,17899,17920,17924,17942,17952:17955,17961:17963,17969,17970,17979,17982,18994:18996,19025,19026,19039,19051:19053,20869,20877,20891,20894,21057,21497,
                21554,21922,23169,24381,24391,24394,24395,24399,24402,24405,24412,24420,24421,24961,27801,28495,29770,32066,43523,43524)]=
  
  
  data_ind$hmod[c(c(299,1968,4353,4393,4993,5157,5162)-1,5161,rep(5170,4),rep(5185,4),5193,5197,5197,rep(5203,3),rep(5208,5),5222,5222,5226,5226,5240,5240,rep(5245,3),rep(5252,3),5257,
                  5861,5878,5878,6453,6453,rep(6677,4),6684,rep(6689,11),6708,6708,6713,6738,6738,c(6835,6858,6886,6934)-1,rep(6937,4),c(6948,7082,7088,7177,7369,7718)-1,
                  7717,7728,7736,rep(7748,3),c(7754,7773,7869,8022,8366,8494,8508,8512,8924)-1,rep(8928,5),8984,rep(8997,3),c(9232,9729,9973)-1,9972,c(9979,10121,10330,10709,
                                                                                                                                                       10742)-1,10741,c(11991,12052,12056,12061,12064)-1,12063,rep(12842,4),12853,12858,rep(12918,5),12928,12928,c(12982,12985,12990,13053,13059,13721)-1,13720,
                  c(13773,13941,14041)-1,14040,14243,14243,c(14252,14775,14778,15460)-1,15459,rep(16592,4),rep(16608,10),17575,17575,rep(17670,3),17677,17677,c(17702,17731,17748,
                                                                                                                                                                17778,17785,17804,17808,17815,17820)-1,rep(17851,4),17857,rep(17860,4),17897,17897,c(17920,17924,17942)-1,rep(17951,4),rep(17960,3),17968,17968,c(17979,17982)-1,
                  rep(18993,3),19024,19024,19038,rep(19050,3),c(20869,20877,20891,20894,21057,21497,21554,21922,23169,24381,24391,24394)-1,24393,c(24399,24402,24405,24412,24420)-1,24419,c(24961,27801,28495,29770,32066,43523)-1,43522)]


data_ind$vmod[c(4353,4393,4993,5173,5174,5206,5209,5223,5227,5228,5247,5248,5253,5254,5862,5879,5880,6454,6455,6678:6681,6690,6694:6698,6710,6714,6739,6740,6858,6886,6934,
                6939:6941,7082,7088,7177,7369,7737,7749,7750,8022,8366,8494,8929,8930,8998,9729,9974,10742,10743,12064,12065,12844:12846,12859,12919:12924,12982,12990,13053,
                13059,13722,13773,14041,14042,14244,14245,14778,16593:16596,16609,16612:16617,17576,17577,17671,17678,17679,17731,17852:17854,17858,17862,17863,17924,17982,
                18994:18996,19025,19026,19039,20869,20891,21057,21554,24391,24395,24402,27801,29770)]=
  
  data_ind$vmod[c(
    c(4353,4393,4993,5173)-1,5172,c(5206,5209,5223,5227)-1,5226,5246,5246,5252,5252,5861,5878,5878,6453,6453,rep(6677,4),6689,rep(6693,5),c(6710,6714,6739)-1,6738,
    c(6858,6886,6934)-1,rep(6938,3),c(7082,7088,7177,7369,7737,7749)-1,7748,c(8022,8366,8494,8929)-1,8928,c(8998,9729,9974,10742)-1,10741,12063,12063,rep(12843,3),12858,
    rep(12918,6),c(12982,12990,13053,13059,13722)-1,13771,14040,14040,14243,14243,14777,rep(16592,4),16608,rep(16611,6),17575,17575,17670,17677,17677,17730,rep(17851,3),
    17857,17861,17861,17923,17981,rep(18993,3),19024,19024,c(19039,20869,20891,21057,21554,24391,24395,24402,27801,29770)-1)]

}


#che
if (id_poiss_int==3183){data_ind$temp[c(1516,1517,1518,1522)]=data_ind$temp[c(rep(1515,3), 1521)]
data_ind$hmod[c(1516,1517,1518,1521,1522)]=data_ind$hmod[c(rep(1515,3), 1520,1520)]
data_ind$vmod[c(1516,1517,1518,1522)]=data_ind$vmod[c(rep(1515,3), 1521)]
}

if (id_poiss_int==3240){data_ind$temp[c(2505,4779,8117,8317)]=data_ind$temp[c(2505,4779,8117,8317)-1]
data_ind$hmod[c(4779,4780)]=data_ind$hmod[c(4778,4778)]
data_ind$vmod[c(4779,4780)]=data_ind$vmod[c(4778,4778)]
}

if (id_poiss_int==3464){data_ind$temp[c(400,414,436,474,489,490)]=data_ind$temp[c(c(400,414,436,474)-1,488,488)]
data_ind$hmod[c(207:209,218,222:229,235,245,252,257,258,297,300:303,324,386,400:402,
                414:416,426,435,436,455,474,475,484,489,490,580:582,603:605)]=
  data_ind$hmod[c(rep(206,3),217,rep(221,8),c(235,245,252)-1,256,256,296,rep(299,4),323,385,rep(399,3),rep(413,3),
                  425,434,434,454,473,473,483,488,488,rep(579,3),rep(602,3))]
data_ind$vmod[c(218,223:227,245,297,303,400:402,414:416,435,436,474,489,490,605)]=
  data_ind$vmod[c(217,rep(222,5),c(245,297,303)-1,rep(399,3),rep(413,3),434,434,473,488,488,604)]

}

if (id_poiss_int==3730){data_ind$temp[c(264,563,1239,1568,1598,2837,3239,3361,3743,3744,18544,18548,18549,18556,18568,21822,21845,22151,22167,22171,22520,23128,23146,
                                        23149:23151,23590,23596:23598,23605,23859,23860)]=data_ind$temp[c(c(264,563,1239,1568,1598,2837,3239,3361,3743)-1,3742,
                                                                                                          18543,18547,18547,c(18556,18568,21822,21845,22151,22167,22171,22520,23128,23146)-1,rep(23148,3),23589,rep(23595,3),23604,23858,23858)]
data_ind$hmod[c(1608,1609,2837,3239,3361,3743,3744,14522,18556,21845,22151,23859,23860)]=data_ind$hmod[c(1607,1607,2836,3238,3360,3742,3742,c(14522,18556,21845,22151,23859)-1,23858)]
data_ind$vmod[c(1608,1609,2837,3239,3361,14522,18556,21845,23859,23860)]=data_ind$vmod[c(1607,1607,c(2837,3239,3361,14522,18556,21845,23859)-1,23858)]

}

if (id_poiss_int==3758){data_ind$temp[c(2562,2577,2580,2591,2596,7235,7240,7241,7270)]=data_ind$temp[c(c(2562,2577,2580,2591,2596,7235,7240)-1,7239,7269)]
data_ind$hmod[c(41,2562,2577,2580,2584,2589:2591,2596)]=data_ind$hmod[c(c(41,2562,2577,2580,2584)-1,rep(2588,3),2595)]
data_ind$vmod[c(2562,2577,2580,2591,2596)]=data_ind$vmod[c(2562,2577,2580,2591,2596)-1]

}

if (id_poiss_int==3121){data_ind$temp[c(1924:1933,4192,4196,4197,4206:4212,4217,4223:4225,4232:4251,4283:4285,4302,4307,4308,4315,4334:4343,
                                        4358:4362,4365:4368,4371:4375,4378:4391,4394,4399,4400,4439:4442,4445:4447,4566,4571)]=
  data_ind$temp[c(rep(1923,10),4191,4195,4195,rep(4205,7),4216,rep(4222,3),rep(4231,20),rep(4282,3),4301,4306,4306,4314,rep(4333,10),rep(4357,5),
                  rep(4364,4),rep(4370,5),rep(4370,14),4393,4398,4398,rep(4438,4),rep(4444,3),4565,4570)]
data_ind$hmod[c(91:93)]=data_ind$hmod[rep(90,3)]
data_ind$vmod[c(91:93)]=data_ind$vmod[rep(90,3)]

}


for (i in which(is.na(data_ind$temp))){
  data_ind$temp[i]<-(data_ind$temp[i-1]+data_ind$temp[i+1])/2
}
for (i in which(is.na(data_ind$hmod))){
  data_ind$hmod[i]<-(data_ind$hmod[i-1]+data_ind$hmod[i+1])/2
}
for (i in which(is.na(data_ind$vmod))){
  data_ind$vmod[i]<-(data_ind$vmod[i-1]+data_ind$vmod[i+1])/2
}

