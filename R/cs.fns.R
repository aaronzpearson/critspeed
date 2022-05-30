#' Critical Speed Modelling Functions
#' 
#' A model output that includes parameter estimates and model goodness-of-fit.
#' 
#' The critical speed model functions are helper functions that feed into `model.results` and `model.fits` to 
#' return summarized model fits and fitted data, respectively. 
#' 
#' Using nonlinear regression, `minpack.lm::nlsLM`, you can fit the six mathematical models (CV2, CV3, OmVD, 5-PL, exp, exp decay). 
#' The initial values for the regressions are `crit.speed.estim = 3.5` in m/s, `d.prime.estim = 150` in m, and 
#' `max.speed.estim = 12` in  m/s. These values were chosen after nonlinear regressions with an array of starting values 
#' (1, 3.5, and 5 for CV; 80, 150, and 1500 for D’; and 7, 12 and 15 for Vmax) converged to the same final values.  
#' The regression optimized parameters using the Levenberg-Marquardt algorithm to minimize the residual sum-of-squares (RSS). 
#' 
#' Individual model functions can be called. Please note that there is no explicit documentation for individual model functions. 
#' 
#' The `data` argument requires the data to be processed via `critspeed::critspeed`, `max.mean.speed.df`, or 
#' `max.median.speed.df`.
#' 
#' For more details, please see `model.results` and `model.fits`.
#' 
#' @seealso model.results, model.fits
#'
#' @param data a data frame of the form critspeed
#' @param d.prime.estim D' estimate, default set to 100 m
#' @param crit.speed.estim critical speed estimate, default set to 3.5 m/s
#' @param max.speed.estim max speed, default set to 12 m/s
#'
#' @export
cs.fns <- function(data, 
                      d.prime.estim = 100,
                      crit.speed.estim = 3.5,
                      max.speed.estim = 12) 
  
  { }

#' @describeIn cs.fns OmVD Model
cs.extended.model <- function(data,
                              d.prime.estim = 100,
                              crit.speed.estim = 3.5,
                              max.speed.estim = 12) {

  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                  crit.speed + d.prime * (1 - exp(-1 * duration * (max.speed - crit.speed)/ d.prime))/duration,
                data = data,
                start = list(
                  d.prime = d.prime.estim,
                  crit.speed = crit.speed.estim,
                  max.speed = max.speed.estim
                  ),
                control = nls.control(maxiter = 1000),
                lower = c(10, 1, 5.8),
                upper = c(800, 5.8, 12)
                )

  cs.nls

}

#' @describeIn cs.fns Five Parameter Model
cs.five.param <- function(data,
                          crit.speed.estim = 3.5,
                          max.speed.estim = 12) {

  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                  crit.speed + (max.speed - crit.speed)/ ((1 + exp(-a * log(duration) - b))^f),
                data = data,
                start = list(a = -2.71,
                             b = 2.195,
                             f = 0.2,
                             crit.speed = crit.speed.estim,
                             max.speed = max.speed.estim),
                control = nls.control(maxiter = 1000),
                lower = c(-20, -20, -5, 1, 5.8), # should a, b, f have upper and lower of NA or -Inf and Inf ?
                upper = c(20, 20, 5, 5.8, 12)
                  )

  cs.nls

}

#' @describeIn cs.fns Three Parameter Model
cs.three.param <- function(data,
                           d.prime.estim = 150,
                           crit.speed.estim = 3.5,
                           max.speed.estim = 12) {

  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                  crit.speed + d.prime / (duration + d.prime / (max.speed - crit.speed)),
                data = data,
                start = list(d.prime = d.prime.estim,
                             crit.speed = crit.speed.estim,
                             max.speed = max.speed.estim),
                control = nls.control(maxiter = 1000),
                lower = c(10, 1, 5.8),
                upper = c(800, 5.8, 12)
  )

  cs.nls

}

#' @describeIn cs.fns Two Parameter Model
cs.two.param <- function(data,
                         d.prime.estim = 150,
                         crit.speed.estim = 3.5) {

  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                  d.prime/ duration + crit.speed,
                data = data,
                start = list(d.prime = d.prime.estim,
                             crit.speed = crit.speed.estim),
                control = nls.control(maxiter = 1000),
                lower = c(10, 1),
                upper = c(800, 5.8)
  )

  cs.nls

}

#' @describeIn cs.fns Exponential Model
cs.exponential <- function(data,
                           crit.speed.estim = 3.5,
                           max.speed.estim = 12) {

  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                  crit.speed + (max.speed - crit.speed) * exp(-duration/ tau),
                data = data,
                start = list(
                  crit.speed = crit.speed.estim,
                  max.speed = max.speed.estim,
                  tau = 1
                  ),
                control = nls.control(maxiter = 1000),
                lower = c(1, 5.8, 0),
                upper = c(5.8, 12, 5)
  )

  cs.nls
}

#' @describeIn cs.fns Exponential Decay Model
cs.exponential.decay <- function(data,
                                 crit.speed.estim = 3.5,
                                 max.speed.estim = 12) {


  theta.0 <- min(data$max.mean.speed) * 0.5
  model.0 <- lm(log(max.mean.speed - theta.0) ~ duration, data = data)
  alpha.0 <- exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]

  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~ alpha * exp(beta * duration) + theta,
                data = data,
                start = list(alpha = alpha.0,
                            beta = beta.0,
                            theta = theta.0),
                control = nls.control(maxiter = 1000,
                                      warnOnly = TRUE)
                )

  cs.nls

}
