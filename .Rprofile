# Set MAKE core count for parallel compilation
Sys.setenv(MAKEFLAGS = sprintf("-j%d", parallel::detectCores()))
# Set current core count for parallel installation
options(Ncpus = parallel::detectCores())
# Activate environment
source("renv/activate.R")
