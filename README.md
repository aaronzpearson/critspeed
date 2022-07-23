## first package iteration is up and running!

### Note

nls starting values:    
* d': 150 m    
* Vmax: 9 m/s    
* CV: 4 m/s    

nls bounds:    
* d': 10, 800    
* Vmax: 4, 12 m/s    
* CV: 1, 5.8 m/s    

= = =

How to:    

**install package**    
devtools::install_github("critspeed")

**load package**    
library(readr)    
library(critspeed)

**read data**    
df <- read_csv("...")

**model MMV**    
critspeed::critspeed()    

**model fits**    
critspeed::model.results()    

**basic plots**    
critspeed::model.plots()    

**compare model fits**    
critspeed::model.results.compare()    

= = =

To do:

* versatility of cs.results.observed.list():    
    *1. global vs session data (complete)*,    
     2. feed into crit.speed.results(),     
     3. feed into plotting functions,            
* double-check all package dependencies are documented as imports    
* remove exponential function?    
* add all roecker algorithms?    
* add arithmetic algorithms?    
* add t[inv] algorithm?    
* improve fit (marginal) w/ optim fctn?    

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
