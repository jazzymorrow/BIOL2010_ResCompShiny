#-----------------------------------------
# Script to test rescomp codes 
# Created 8/02/2023
# Author: Jasmine Fowler-Morrow
#-----------------------------------------

library(rescomp)

# One consumer, One continuously supplied resource 
pars <- spec_rescomp(
  spnum = 1, 
  resnum = 1,
  funcresp = "type2",
  mumatrix = matrix(0.12),
  resspeed = 0.03,
  resconc = 2,
  totaltime = 300
)
m1 <- sim_rescomp(pars)
plot_rescomp(m1)


pars <- spec_rescomp(
  spnum = 2, 
  resnum = 1,
  funcresp = "type2",
  mumatrix = matrix(c(0.7,0.05), 
                    nrow = 2, 
                    ncol = 1,
                    byrow = TRUE),  
  resspeed = 0.03, 
  resconc = 0.2,
  totaltime = 500)

m1 <- sim_rescomp(pars)
plot_rescomp(m1)  
