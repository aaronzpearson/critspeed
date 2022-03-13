## first package iteration is up and running!

= = =

Current workflow:

devtools::install_github("critspeed")

library(readr)
library(critspeed)

df <- read_csv("...")

max.mean.speed.df(df$max_speed)    
max.median.speed.df(df$duration)    
cs.results.model(df$max_speed)    
cs.results.fitted(df$max_speed)    
cs.results.plot(df$max_speed, log.dur = FALSE)

= = =

To do:

* *&plotting functions (complete)*    
* versatility of cs.results.observed.list(): *1. global vs session data (complete)*, 2. feed into crit.speed.results(), 3. feed into plotting functions, 4. *return as data.frame with residuals and raw max mean vel data (complete)*        
* consistent function naming conventions (same concept as fvp package)    
* butterworth filter for roeker (in place of Kalman filter - Kalman filter in R is nearly impossible)    
* look into stats::KalmanSmooth (small chance it will work)    
* function documentation *Eli*    
* explicit package dependency calls *nearly complete*    
* double-check all package dependencies are documented as imports    

Current dependencies:

* data.table    
* Rfast    
* minpack.lm    
* signal (not yet implemented - for butterworth filter)    

These package have (almost) no dependencies of their own    

= = =

# critspeed
An R package to accompany Eli Mizelman's PhD dissertation


This package provides practitioners with the ability to model an athlete's critical speed/ aerobic velocity from GPS data. The models include:    
* 2 parameter model    
* 3 parameter model    
* 5 parameter model    
* OmVD    
* Exponential model *needs fixing*    

And those suggested by Roeker et al., 2017: *none of these are complete*    
* Richard's logistic 5 parameter    
* Generalized logistic 4 parameter    
* Logistic 3 parameter    
* Quintic polynomial    
* Bi-exponential with 5 parameters    
* Gompertz' logistic 4 parameter    
* Decaying exponential 3 parameter    


The values returned to the user include:    
* Critical speed/ aerobic velocity estimates    
* Aerobic capacity (D') estimates    
* Model fits    
* Predicted values    
    
* D' balance can be modelled using the *stamina* package (currently beta version)
