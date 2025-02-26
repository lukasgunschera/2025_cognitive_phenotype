## ======================================================================================================================= ##
## Script:       MAKEFILE
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2025-02-10
## ======================================================================================================================= ##
##
## This script contains detailed instructions on how to execute the code of the project folder. Please ensure to keep to the
## below instructions to reproduce the results of the project.
##
## Unfortunately, we do not have the right to share the data acccompanying this work. The data can be obtained from the
## Leiden Consortium on Individual Development team (https://www.developmentmatters.nl/).
##
## ======================================================================================================================= ##

## DEPENDENCIES =============================================================================================================
# [please note that executing the below code will install software on your computer]
# [please ensure to have the necessary rights to install software on your computer]

install.packages("renv")
renv::init() # (1) restore the project from the lockfile


install.packages("extrafont")
library(extrafont)
extrafont::font_import()

# install cmdstanr to for model fitting in stan
remotes::install_github("stan-dev/cmdstanr")

# install hBayesDM for model fitting in stan
install.packages("hBayesDM", dependencies = TRUE)

## CLUSTER ==================================================================================================================
# Many of the project scripts contain computationally intensive tasks. To manage the computational load we have executed
# the relevant scripts on a local computing cluster. Fitting the scripts on the cluster is managed via the 'FIT_CLUSTER'
# global parameter. Please note, the present code will not work across clusters and may require substantial modifications.


## CODE STRUCTURE ===========================================================================================================

# (1) code/lcid_preprocessing.R
# This script contains all preprocessing steps applied to the raw questionnaire and demographic data.

# (2) code/lcid_parameter_recovery.R
# This script contains all steps to perform the parameter recovery for the hyperbolic model in stan.


