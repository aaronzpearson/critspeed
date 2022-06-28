## critspeed V1 

Current workflow:

*Note* ... indicates that there are other arguments that should be filled-in    
*Note* functions can continuously take on the raw/ GPS data - make sure you set raw.data = TRUE    
*Note* if you are only working with GPS data, you can skip the critspeed() step    

devtools::install_github("critspeed")

library(readr)    
library(critspeed)

df <- read_csv("...")

df_clean <- clean.data(player.speed = df$max_speed, ...)    
df_critspeed <- critspeed(data = list(df_clean), speed.col = "player.speed.metric", ...)    
critspeed_model_results <- model.results(player.speed = df_critspeed$max.mean.speed, raw.data = FALSE, ...)     
critspeed_model_fits <- model.fits(player.speed = df_critspeed$max.mean.speed, raw.data = FALSE, ...)    

critspeed_plots <- model.plots(player.speed = df_critspeed$max.mean.speed, raw.data = FALSE, plots = c("fit", "resid", "qq"), ...)    

= = =

To do:

* butterworth filter for roeker (in place of Kalman filter - Kalman filter in R is nearly impossible)    
* look into stats::KalmanSmooth (small chance it will work)      
* complete documentation with examples    
* fix data issues - currently can't compile the package with data provided for the user

Current dependencies:

* data.table    
* Rfast    
* minpack.lm    
* signal (not yet implemented - for butterworth filter)    

These package have (almost) no dependencies of their own    

# critspeed
An R package to accompany Eli Mizelman's PhD dissertation


This package provides practitioners with the ability to model an athlete's critical speed/ aerobic velocity from GPS data. The models include:    
* 2 parameter model    
* 3 parameter model    
* 5 parameter model    
* OmVD    
* Exponential model  

And those suggested by Roeker et al., 2017: *none of these are complete*    
* Richard's logistic 5 parameter    
* Generalized logistic 4 parameter    
* Logistic 3 parameter    
* Quintic polynomial    
* Bi-exponential with 5 parameters    
* Gompertz' logistic 4 parameter    
* Decaying exponential 3 parameter    


The parameter values returned to the user include:    
* Critical speed/ aerobic velocity estimates    
* Aerobic capacity (D') estimates    
* Model goodness-of-fits    
* Predicted values    
