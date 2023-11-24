# integration de targets au research compendium 
library(targets)
library(tarchetypes)
library(ggplot2)
library(visNetwork)

#sources all the function to the _target project
tar_source()

list(
  
  #clean data from catfish N 3856 (script01)
  tar_target(prep3856, prepa1(here::here("data", "derived-data",
                                         "3856")))
  
  #clean data from chub N 3183 (script01)
  , tar_target(prep3183, prepa1(here::here("data", "derived-data",
                                         "3183")))
  
  #clean data from barbel N 3100 (script01)
  , tar_target(prep3100, prepa1(here::here("data", "derived-data",
                                           "3100")))
  
  # obtain individual data frame (script02)
  , tar_target(ind3856, prepa2(prep3856)) 
  , tar_target(ind3183, prepa2(prep3183)) 
  , tar_target(ind3100, prepa2(prep3100)) 
  
  # inference: calculate parameters (script03)
  #, tar_target(inf, inferences(ind3856))
  
  # declare the path for input files used by script04
  , tar_target(machin_files, c("outputs/image_inf_individual_3856_v10.Rdata", 
                               "outputs/image_inf_individual_3856_v10.Rdata", 
                               "outputs/image_inf_individual_3100_v10.Rdata"), 
               format = "file")
  
  # concatenate inferences (script04)
  , tar_target(mtot, treatment_posterior(machin_files))
               
  # parameter classification (script05)
  , tar_target(classif1, classif_computation(mtot))
  
  # classification decision (script06)
  , tar_target(classif2, hhfh(classif1))
  
  # plots generation (script06)
  
  #generate quarto file
  , tar_render(report, "trajecto_targets.qmd")
  
)
