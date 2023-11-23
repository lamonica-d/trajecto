# integration de targets au research compendium 
library(targets)
library(tarchetypes)
library(ggplot2)
library(visNetwork)

#sources all the function to the _target project
tar_source()

list(
  #clean data from fish N 3856 (script01)
  tar_target(prep3856, prepa1(here::here("data", "derived-data",
  "3856")))
  
  # obtain individual data frame (script02)
  , tar_target(ind3856, prepa2(prep3856)) 
  
  # inference: calculate parameters (script03)
  #, tar_target(inf, inferences(ind3856))
  
  # concatenate inferences (script04)
  , tar_target(mtot, treatment_posterior(here::here("outputs", 
                                        "image_inf_individual_3856_v10.Rdata")))
               
  # parameter classification (script05)
  , tar_target(classif1, classif_computation(mtot))
  
  # classification decision (script06)
  , tar_target(classif2, hhfh(classif1))
  
  # plots generation (script06)
  
  #generate quarto file
  , tar_render(report, "trajecto_targets.qmd")
  
)
