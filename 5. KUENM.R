#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ecological niche modelling using KUENM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## to be performed for each data subset(i.e. data subset 1:10)

#load required package
library(kuenm)
library(raster)
library(terra)

# setting working directory (CHANGE IT ACCORDING TO YOUR NEEDS)
setwd(".../KUENM")

# reading initial data
## occurrence records
occurrences <- read.csv("sample_1/A_vittatus_country_2.5_01.csv") # species occurrence records
occurrences <- occurrences[, c(2,3,1)]

## environmental variables cropped to M area
vars <- stack(list.files("M_variables/set_1", 
                          pattern = ".asc$",
                          full.names = TRUE))       


# data preparation
data_prep <- prepare_swd(occ = occurrences, species = "Species",
                         longitude = "Longitude", latitude = "Latitude",
                         data.split.method = "random", train.proportion = 0.7,
                         raster.layers = vars,
                         var.sets = "all_comb", save = TRUE,
                         name.occ = "sample_1/sp",
                         back.folder = "sample_1/background")


# model calibration and selection
## define arguments
oj   <- "sample_1/sp_joint.csv"
otr  <- "sample_1/sp_train.csv"
ote  <- "sample_1/sp_test.csv"
back <- "sample_1/background"
btch <- "sample_1/batch_calibration"
odir_calmodels <- "sample_1/Candidate_models"
rg  <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4, 5)
fc  <- c("q", "lq", "lp", "qp", "lqp")
mx  <- "Data/maxent"
sel <- "OR_AICc"
thr <- 1
odir_eval <- "sample_1/Calibration_results"


## run model calibration
cal <- kuenm_cal_swd(occ.joint = oj, occ.tra = otr, occ.test = ote,
                     back.dir = back, batch = btch,
                     out.dir.models = odir_calmodels, reg.mult = rg,
                     f.clas = fc, maxent.path = mx, selection = sel,
                     threshold = thr, out.dir.eval = odir_eval)


# get the final models
cal$selected_models

## get names of predictors variables (these variables are moved into the G_variables folder)
## use variables representing area of model calibration if transferring the models to the calibration area (M _area) or
## use variables representing area of model transfer if transferring the models to areas of potential invasion 

colnames(read.csv("sample_1/background/Set_23.csv"))


#PC prediction in G
gvar <- "sample_1/G_variables"

## set the arguments for final models
btch_final <- "sample_1/bacth_final"
rn <- 10
rt <- "Bootstrap"
jk <- TRUE
of <- "cloglog"
pr <- TRUE
ety <- "all"
mod_dir <- "sample_1/Final_models"


## run final models
kuenm_mod_swd(occ.joint = oj, back.dir = back, out.eval = odir_eval,
              batch = btch_final, rep.n = rn, rep.type = rt, maxent.path = mx,
              jackknife = jk, out.format = of, project = pr,
              G.var.dir = gvar, ext.type = ety, out.dir = mod_dir, wait = TRUE)

# ------------------------------------------------------------------------------
# Post modeling analysis--------------------------------------------------------
# model consensus
## define stat arguments
sp <- read.csv("sample_1/sp_joint.csv")[1, 1]
scen <- c("current")
ext.type = c("E", "EC", "NE")
stats <- c("med", "range")
statsdir <- "sample_1/Final_model_stats"

## run model stats
kuenm_modstats_swd(sp.name = sp, fmod.dir = mod_dir_cal, statistics = stats,
                   proj.scenarios = scen, ext.type = ext.type, 
                   out.dir = statsdir)
# ------------------------------------------------------------------------------