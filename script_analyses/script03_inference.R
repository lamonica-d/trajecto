#######################################################################################
#######################################################################################
# Script 3 - Basis to write inference script for one individual
#######################################################################################
#######################################################################################

library(doParallel)
library(coda)
library(runjags)

## choose which individual
pois_id <- 3100

## load and set data  
numpoiss_vect=c(3100,3156,3744,3128,3170,3051,3317,3674,3828,3856,3632,3786,3835,3849,3870,3772,3800,3387,3415,3429,
                3183,3212,3240,3352,3464,3730,3758,3121,3394,3408,3422,3506,3562,3590,3625,3079,3303)
num_ind=which(numpoiss_vect == pois_id)
poiss=1:length(numpoiss_vect)

load(paste0("data/derived-data/data_ind",numpoiss_vect[num_ind],sep=""))
load("data/derived-data/mat_debut")
load("data/derived-data/mat_fin")
load("data/derived-data/nb_section")

## code for nycthemeral period
aube=ifelse(data_ind$nyct==1,1,0)
crep=ifelse(data_ind$nyct==2,1,0)
jour=ifelse(data_ind$nyct==3,1,0)
nuit=ifelse(data_ind$nyct==4,1,0)

## define data list for JAGS
mydata <- list(qinit=c(0.5,0.5), nbsection=nbsection[num_ind],pi=pi,
               debut=mat_debut[num_ind,1:nbsection[num_ind]]-mat_debut[num_ind,1]+1,
               fin=mat_fin[num_ind,1:nbsection[num_ind]]-mat_debut[num_ind,1]+1,
               
               rangle=as.vector(data_ind$rangle),vit=as.vector(data_ind$vitesse)*1000,
               rapport_dist=as.vector(data_ind$rapport_dist), var_angles_bruts=data_ind$var_angles_bruts
               
               ,vitc_moy=as.vector(data_ind$vmod) 
               ,hmod_moy=as.vector(data_ind$hmod)
               ,temp_moy=as.vector(data_ind$temp) 
               
               ,aube=as.vector(aube),crep=as.vector(crep),jour=as.vector(jour),nuit=as.vector(nuit)
)

## generate and print initial values for parameters
inits=list(gener_inits(),gener_inits(), gener_inits())
print(inits)

## launch cluster for parallel execution of 3 chains
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
# !!!
# path to lib/JAGS/modules-4 to indicate, 
# if you are not root on the computer you are using

## define variables to monitor
lesvariables=c("k", "inv_lambda","phi", "rho", "mu", "tau", "alpha", "beta", "a")

## printing start time
debut=Sys.time()
print(debut)

## run jags on parallel
jags_res=foreach(i =1:3,.combine='mcmc.list',.multicombine=TRUE)%dopar%{
  as.mcmc(run.jags("modele_individu.txt",monitor=lesvariables,data=mydata,n.chains=1,inits=dump.format(inits[[i]]),
                   burnin=200,sample=500,thin=100,tempdir=FALSE,
                   summarise=FALSE,adapt = 100,keep.jags.files=FALSE))
  
}

## printing end time
fin=Sys.time()
print(fin)

stopCluster(cl)

## save image session, I advise you to clean your session before starting the script
save.image(paste("data/derived-data/image_inf_individual_",numpoiss_vect[num_ind],"_v10.Rdata", sep=""))



