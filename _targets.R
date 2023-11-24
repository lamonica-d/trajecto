# integration de targets au research compendium 
library(targets)
library(tarchetypes)
library(ggplot2)
library(visNetwork)

#sources all the function to the _target project
tar_source()

list(
  
  # declare the input raw data for script01
  tar_target(input_data, c("data/derived-data/3856", 
                           "data/derived-data/3183",
                           "data/derived-data/3100"), 
            format = "file")
  
  # clean data of catfish N 3856 (script01)
  , tar_target(prep3856, prepa1(here::here("data", "derived-data",
                                         "3856")))
  
  #clean data of chub N 3183 (script01)
  , tar_target(prep3183, prepa1(here::here("data", "derived-data",
                                         "3183")))
  
  #clean data of barbel N 3100 (script01)
  , tar_target(prep3100, prepa1(here::here("data", "derived-data",
                                           "3100")))
  
  # obtain individual data frames (script02)
  , tar_target(data_for_inference, prepa2(prep3856, prep3183, prep3100)) 
  
  # model parameters (script03) should be calculated in an external cluster 
  # this step takes several days. Therefore, for the next step, input files
  # are taken from data generated beforehand>
  #, tar_target(inf, inferences(data_for_inference))

  # declare the input data for script04
  , tar_target(machin_files, c("outputs/image_inf_individual_3856_v10.Rdata", 
                               "outputs/image_inf_individual_3856_v10.Rdata", 
                               "outputs/image_inf_individual_3100_v10.Rdata"), 
               format = "file")
  
  # concatenate inferences (script04)
  , tar_target(mtot, treatment_posterior(machin_files))
               
  # parameter classification (script05)
  , tar_target(classif1, classif_computation(mtot))
  
  # classification decision (script06)
  , tar_target(classif2, classif_decision(classif1))
  
  # data frame generation for plots (script07)
  , tar_target(plot, make_df_for_plot(mtot, classif1, classif2))
  
  # generate quarto file
  , tar_render(report, "trajecto_targets.qmd")
  
)
