## ============================================================================================================================ ##
## Script:    Dependencies Installation
## ============================================================================================================================ ##
## Authors:   Lukas J. Gunschera
## Date:      Wed Apr  3 12:08:10 2024
## ============================================================================================================================ ##
## Install all dependencies of scripts contained in lcid_cognitive_phenotype
## [script needs to be run only once]
## ============================================================================================================================ ##

## Note: Before running command scripts, the current working directory must be set to the top-level
## folder containing the dmc, code, and data subfolders

## ============================================================================================================================ ##
## Dependencies
## ============================================================================================================================ ##

# Depedencies
install.packages("backports")
install.packages("expm")
install.packages("evd")
install.packages("contfrac")
install.packages("deSolve")
install.packages("Rcpp")
install.packages("gsl")
install.packages("ps")
install.packages("processx")
install.packages("colorspace")
install.packages("scales")
install.packages("glue")
install.packages("tibble")
install.packages("stringi")

# Standard packages from CRAN
install.packages("truncdist") # truncated t etc.
install.packages("msm")  # For truncated normal priors
install.packages("loo") # For WAIC and looaic calculation
install.packages("hypergeo") # For population plausible values
install.packages("statmod") # Wald model
install.packages("rtdists") # For standard model distribution functions
install.packages("pracma")  # For gng and stop signal robust integration
install.packages("snowfall") # Parallel processing
install.packages("rlecuyer") # Parallel processing
install.packages("numDeriv") # Prior transformations
install.packages("vioplot") # Stop signal graphs
install.packages("ggplot2") # For fancy graphs
install.packages("gridExtra") # For fancy graphs
install.packages("mvtnorm") # For Bayes Factors
install.packages("Matrix") # For Bayes Factors
install.packages("Brobdingnag") # For Bayes Factors
install.packages("stringr") # For Bayes Factors
install.packages("LaplacesDemon") # For multivariate Cauchy
install.packages("Brobdingnag")

# This modificaiton of coda allows for plotting of priors with plot.dmc
install.packages("dmc/packages/coda_0.19-3.tar.gz",repos=NULL,type="source")

# Run the below to ensure availability of font types for visual outputs
install.packages("extrafont")
library(extrafont)
extrafont::font_import()


