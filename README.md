To do:

* plotting functions    
* versatility of cs.results.observed.list(): 1. global vs session data, 2. feed into crit.speed.results(), 3. feed into plotting functions, 4. return as data.frame with residuals and raw max mean vel data        
* consistent function naming conventions (same concept as fvp package)    
* butterworth filter for roeker (in place of Kalman filter - Kalman filter in R is nearly impossible)    
* look into stats::KalmanSmooth (small chance it will work)    

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
* Exponential model    

And those suggested by Roeker et al., 2017:    
* Richard's logistic 5 parameter    
* Generalized logistic 4 parameter    
* Logistic 3 parameter    
* Quintic polynomial    
* Bi-exponential with 5 parameters    
* Gompertz' logistic 4 parameter    
* Decaying exponential 3 parameter    


The values returned to the user include:    
* Critical selocity/ aerobic velocity estimates    
* Aerobic capacity (D') estimates    
* Model fits    
* Predicted values    
