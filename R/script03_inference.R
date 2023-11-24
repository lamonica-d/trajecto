#######################################################################################
#######################################################################################
# Script 3 - 
#######################################################################################
#######################################################################################

inferences <- function (){
  
  
  
########################nyct#################################
aube=ifelse(data_ind$nyct==1,1,0)
crep=ifelse(data_ind$nyct==2,1,0)
jour=ifelse(data_ind$nyct==3,1,0)
nuit=ifelse(data_ind$nyct==4,1,0)

#############################################################

mydata <- list(qinit=c(0.5,0.5),  nbsection=nbsection[num_ind],pi=pi,#nbpoiss=nbpoiss,
               debut=mat_debut[num_ind,1:nbsection[num_ind]]-mat_debut[num_ind,1]+1,
               fin=mat_fin[num_ind,1:nbsection[num_ind]]-mat_debut[num_ind,1]+1,
               rangle=as.vector(data_ind$rangle),vit=as.vector(data_ind$vitesse)*1000,
               rapport_dist=as.vector(data_ind$rapport_dist), var_angles_bruts=data_ind$var_angles_bruts
               
               ,vitc_moy=as.vector(data_ind$vmod) #, vmod_carre=as.vector(data_ind$vmod)^2
               ,hmod_moy=as.vector(data_ind$hmod)#, hmod_carre=as.vector(data_ind$hmod)^2
               ,temp_moy=as.vector(data_ind$temp) #,temp_carre=as.vector(data_ind$temp)^2
               
               ,aube=as.vector(aube),crep=as.vector(crep),jour=as.vector(jour),nuit=as.vector(nuit)
               
               #,vmax_moy=as.vector(data_ind$vmax), cvv_moy=as.vector(data_ind$cvv),ex_hours=as.vector(data_ind$nb_ex_cr)
)

gener_inits =function(){
  
  list("a"=matrix(data=rnorm(22,mean=0,sd=1),nrow=2,ncol=11),
       "k"=c(runif(1,0,1),runif(1,2,5)),"inv_lambda"=runif(2,0,1),
       
       "alpha"=c(runif(1,1,2),runif(1,2,5)),"beta"=c(runif(1,3,7),runif(1,1,2)),
       "mu"=c(runif(1,2,5),runif(1,0,1)),"tau"=runif(2,0,2)
       
       ,"phi"=c(rnorm(1,mean=pi, sd=0.1), runif(1,0,1))
       ,"rho"=runif(2,0,2)
       
  )
}


inits=list(gener_inits(),gener_inits(), gener_inits())
print(inits)
##############################################"
#library(Rmpi)
library(doParallel)
library(coda)
library(runjags)

cl <- makeCluster(3)

registerDoParallel(cl)
clusterCall(cl, function () Sys.info () [c ( "nodename", "machine" ) ] )

clusterExport(cl,c("mydata", "inits"))
clusterEvalQ(cl,library(rjags))
clusterEvalQ(cl,library(coda))
clusterEvalQ(cl,library(runjags))
clusterEvalQ(cl,library(doParallel))
clusterEvalQ(cl,library(foreach))
clusterEvalQ(cl,library(iterators))
clusterEvalQ(cl, load.module("vonmises", path="/home/lamonicad/lib/JAGS/modules-4"))

#https://github.com/yeagle/jags-vonmises

lesvariables=c("k", "inv_lambda","phi", "rho", "mu", "tau", "alpha", "beta", "a"
)

debut=Sys.time()
print(debut)

jags_res=foreach(i =1:3,.combine='mcmc.list',.multicombine=TRUE)%dopar%{
  as.mcmc(run.jags("modele_individu_v9",monitor=lesvariables,data=mydata,n.chains=1,inits=dump.format(inits[[i]]),
              burnin=200,sample=500,thin=100,tempdir=FALSE,
                   summarise=FALSE,adapt = 100,keep.jags.files=FALSE))

}



fin=Sys.time()
print(fin)

stopCluster(cl)

save.image(paste("image_inf_individual_",numpoiss_vect[num_ind],"_v10.Rdata", sep=""))
}


