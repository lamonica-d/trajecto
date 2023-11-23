
#######################################################################################
#######################################################################################
# Script 4 - Jags posterior treatment
#######################################################################################
#######################################################################################

#library(rjags)
# library(viridis)
# library(ggplot2)
# library(cowplot)
# library(gridExtra)
# library(grid)

treatment_posterior <- function(){
  
  numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

id_sp=c("B-3100", "B-3128", "B-3170", "B-3744" ,
        "C-3121", "C-3183" ,"C-3212" ,"C-3240", "C-3352", "C-3464", "C-3730" ,"C-3758" ,
        "S-3632", "S-3786", "S-3835", "S-3849", "S-3856", "S-3870")

vect_tailles=c(397,311,302,477,455,486,313,325,248,472,454,462,720,620,496,367,1001,1050)

############generer fichier avec les mcmc#############
mtot_list=list()
for (j in 1:length(numpoiss_vect0)){

  load(file.path("outputs",paste("image_inf_individual_",
             numpoiss_vect0[j],"_v10.Rdata", sep="")))


    print(numpoiss_vect0[j])
    print(gelman.diag(jags_res))
  # print(fin-debut)
   plot(jags_res, trace=T, density=F)
  mtot_list[[j]]=rbind(jags_res[[1]],jags_res[[2]],jags_res[[3]])
}
save(mtot_list, file="mtot_list_v5")
}
