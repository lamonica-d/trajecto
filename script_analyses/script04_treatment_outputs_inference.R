#######################################################################################
#######################################################################################
# Script 4 - Jags posterior treatment
#######################################################################################
#######################################################################################

library(rjags)

## set data
numpoiss_vect0=c(3100, 3128, 3170, 3744 ,3121, 3183 ,3212 ,3240, 3352, 3464, 3730 ,3758 ,3632, 3786, 3835, 3849, 3856, 3870)

## load MCMC from individual runs
mtot_list=list()
for (j in 1:length(numpoiss_vect0)){
  
  load(file.path("data", "derived-data","images_jags_inference",paste("image_inf_individual_",
                                                                      numpoiss_vect0[j],"_v10.Rdata", sep="")))
  
  ## some quick checks
  print(numpoiss_vect0[j])
  print(gelman.diag(jags_res))
  print(fin-debut)
  plot(jags_res, trace=T, density=F)
  mtot_list[[j]]=rbind(jags_res[[1]],jags_res[[2]],jags_res[[3]])
}

## save in one file
save(mtot_list, file="outputs/mtot_list")

