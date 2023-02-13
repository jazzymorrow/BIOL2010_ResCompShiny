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

# Two consumers, one continuously supplied resource 
pars <- spec_rescomp(
  spnum = 2, 
  resnum = 1,
  funcresp = "type2",
  mumatrix = matrix(c(0.7,0.05), 
                    nrow = 2, 
                    ncol = 1,
                    byrow = TRUE),  
  resspeed = 0.03, 
  resconc = 0.5,
  totaltime = 500)

m1 <- sim_rescomp(pars)
plot_rescomp(m1)  


# Two consumers, one pulsed resource 
pars <- spec_rescomp(
  spnum = 2, 
  resnum = 1,
  funcresp = "type2",
  mumatrix = matrix(c(0.7,0.05), 
                    nrow = 2, 
                    ncol = 1,
                    byrow = TRUE),
  kmatrix = matrix(c(2, 0.015), 
                   nrow = 2, 
                   ncol = 1, 
                   byrow = TRUE),  
  resspeed = 0, # set to zero for no additional resource supply 
  resconc = 0.2,
  respulse = 0.3,
  pulsefreq = 100, # resource pulse size
  totaltime = 500
)
m1 <- sim_rescomp(pars)
plot_rescomp(m1) + theme(text=element_text(size=20),
                         axis.title = element_text(size = 10),
                         axis.text = element_text(size = 10),
                         legend.text = element_text(size = 10),
                         legend.position = "bottom")

# Two consumers, two resources 
pars <- spec_rescomp(
  spnum = 2, 
  resnum = 2,
  funcresp = "type2",
  mumatrix = matrix(c(0.09,0.04,
                      0.05,0.08), 
                    nrow = 2, 
                    ncol = 2,
                    byrow = TRUE),
  resspeed = 0.03,
  resconc = 1,
  mort = 0.03,
  essential = FALSE,
  totaltime = 1000
)

plot_funcresp(pars, maxx = 0.2)

m1 <- sim_rescomp(pars)
plot_rescomp(m1)
