#' trajectometry : workflow for data analysis
#' 
#' @description 
#' 
#' @author  \email{dominique.lamonica@gmail.com}
#' 
#' @date 2024/07/26

## 0) Install Dependencies and Load Project Addins
devtools::install_deps(upgrade = "never")
devtools::load_all(here::here())

## 1) Data preparation
source(here::here("script_analyses", "script01_prepa_data.R"))
source(here::here("script_analyses", "script02_indiv_data.R"))

## 2) Inference run !! COMPUTATION TIME !!
# wouldn't advise you to run it on your own laptop/computer,
# but on a server where you copy the entire R project

# need install of JAGS https://mcmc-jags.sourceforge.io/
# and its module "von-mises" https://github.com/yeagle/jags-vonmises

# then use script_analyses/script03_inference.R :
# choose which individual with l.12 : pois_id <- ...
# where you replace ... with the number of the individual you want to run (e.g., 3100)
# you can then run the entire script

# 3) Inference outputs treatment
source(here::here("script_analyses", "script04_treatment_outputs_inference.R"))

# 4) Classification
source(here::here("script_analyses", "script05_classif_computation.R"))
source(here::here("script_analyses", "script06_classif_decision.R"))

# 5) Figures
source(here::here("script_analyses", "script07_data_for_plots.R"))
source(here::here("script_analyses", "script08_figures.R"))




