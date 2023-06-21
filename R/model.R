#' Non-Linear Critical Speed Model Regressions
#'
#' A generic modelling function for the \code{critspeed} package.
#' 
#' The \code{model()} function calls on the \code{minpack.lm::nlsLM()} function which minimizes the residual sum-of-squares (RSS) 
#' via the Levenberg-Marquardt algorithm.
#' 
#' This is a helper function that is called upon by \code{model.plots()} and \code{model.parameters()}, see \strong{See Also}. The output
#' reflects those from the \code{nlsLM()} function from the \code{minpack.lm} package. 
#' 
#' @param data data set containing at least two variables (see notes)
#' @param model critical speed model name as either \code{"two.param"}, \code{"three.param"}, \code{"five.param"}, or \code{"omvd"}
#' @param cv.2 minimum duration to fit the two parameter function, default set to 120 s (see notes)
#' @param lower lower bounds for parameter fits (see notes)
#' @param upper upper bounds for parameter fits (see notes)
#' @param start starting values for parameter fits (see notes)
#'
#' @note 
#' 
#' Users must be aware of the following requirements for the following arguments:    
#' 
#' \enumerate{
#' \item \code{data} is a \code{data.frame} must satisfy the following criteria:
#' \itemize{
#' \item The first column is the duration variable
#' \item The second column is the maximal mean speed or maximal median speed variable
#' \item Subsequent columns will be disregarded
#' }
#' 
#' Users can generate appropriate \code{data.frame}s using the \code{critspeed()} function
#' 
#' \item \code{cv.2} is the minimum duration used to fit the two parameter model. The default is set to 120 s as suggested by the prevailing 
#' literature.
#' 
#' \item \code{lower}, \code{upper}, and \code{start} are the lower and upper bounds for the parameter fits, and initial parameter values, respectively 
#' The default values are set based on various considerations including the prevailing literature and trial-and-error. The defaults are 
#' outlined below in the form of \code{parameter: lower bound, start value, upper bound}:
#' \itemize{
#' \item d prime: 10, 150, 800 in meters
#' \item critical speed: 1, 4, 5.8 in meters per second
#' \item maximal speed: 4, 9, 12 in meters per second
#' \item a: NA, -2.71, NA and is unitless
#' \item b: NA, 2.195, NA and is unitless
#' \item f: NA, 0.2, NA and is unitless
#' }
#' 
#' }
#' 
#' @seealso \code{\link{critspeed}}, \code{\link{model.plots}}, \code{\link{model.parameters}}
#'
#' @export
#' 
#' @examples 
#' 
#' data(sessionMaxMeanSpeed)
#' data(sessionMaxMedianSpeed)
#' 
#' # Max Mean Model
#' 
#' max.mean.model <- model(data = sessionMaxMeanSpeed,
#' model = "three.param",
#' cv.2 = 120,
#' lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
#' upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
#' start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2))
#' 
#' max.mean.model
#' 
#' # Max Median Model
#' 
#' max.median.model <- model(data = sessionMaxMedianSpeed,
#' model = "five.param",
#' cv.2 = 120,
#' lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
#' upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
#' start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2))
#' 
#' max.median.model
model <- function(data,
                  model = c("two.param", "three.param", "five.param", "omvd"), 
                  cv.2 = 120,
                  lower = c(dp = 10, cs = 1, v0 = 4, a = NA, b = NA, f = NA),
                  upper = c(dp = 800, cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
                  start = c(dp = 150, cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
  
  callCS("models",
         model = model,
         data = data,
         lower = lower,
         upper = upper,
         start = start)
  
}

## helper functions ##

# called on by \code{fit.model()}
# written separately for development purposes
# extensive critical speed nls fitting function
models <- function(model = c("two.param", "three.param", "five.param", "omvd"), 
                   data,
                   cv.2 = 120,
                   lower = c(dp = 10, cs = 1, v0 = 4, a = NA, b = NA, f = NA),
                   upper = c(dp = 800, cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
                   start = c(dp = 150, cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
  
  if(length(model) > 1) {error("user must select one model to fit. Please see the documentation for more information")}
  
  control <- control(control = list(
    lower = lower,
    upper = upper,
    start = start
  ))
  
  low <- control$low
  high <- control$high
  start <- control$start
  
  dat = data
  colnames(dat) <- c("duration", "max.mean.speed")
  
  dat.cv2 <- subset(dat, duration >= cv.2)
  
  model.fit <- switch(model, 
                      
                      omvd = omvd(data = dat,
                                  lower = c(dp = low$dp.low, 
                                            cs = low$cs.low, 
                                            v0 = low$v0.low),
                                  upper = c(dp = high$dp.high, 
                                            cs = high$cs.high, 
                                            v0 = high$v0.high),
                                  start = c(dp = start$dp.start, 
                                            cs = start$cs.start, 
                                            v0 = start$v0.start)),
                      
                      five.param = five.param(data = dat,
                                              lower = c(cs = low$cs.low, 
                                                        v0 = low$v0.low, 
                                                        a = low$a.low, 
                                                        b = low$b.low, 
                                                        f = low$f.low),
                                              upper = c(cs = high$cs.high, 
                                                        v0 = high$v0.high, 
                                                        a = high$a.high, 
                                                        b = high$b.high, 
                                                        f = high$f.high),
                                              start = c(cs = start$cs.start, 
                                                        v0 = start$v0.start, 
                                                        a = start$a.start, 
                                                        b = start$b.start, 
                                                        f = start$f.start)),
                      
                      three.param = three.param(data = dat,
                                                lower = c(dp = low$dp.low, 
                                                          cs = low$cs.low, 
                                                          v0 = low$v0.low),
                                                upper = c(high$dp.high, 
                                                          high$cs.high, 
                                                          high$v0.high),
                                                start = c(dp = start$dp.start, 
                                                          cs = start$cs.start, 
                                                          v0 = start$v0.start)),
                      
                      two.param = two.param(data = dat.cv2,
                                            lower = c(dp = low$dp.low, 
                                                      cs = low$cs.low),
                                            upper = c(dp = high$dp.high, 
                                                      cs = high$cs.high),
                                            start = c(dp = start$dp.start, 
                                                      cs = start$cs.start))
                      
  )
  
  return(model.fit)
  
}

# omni-domain model
omvd <- function(data,
                 lower = c(dp = 10, cs = 1, v0 = 4),
                 upper = c(dp = 800, cs = 5.8, v0 = 12),
                 start = c(dp = 150, cs = 4, v0 = 9)
) {
  
  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                                crit.speed + d.prime * 
                                (1 - exp(-1 * duration * (max.speed - crit.speed)/ d.prime))/ 
                                duration,
                              data = data,
                              start = list(
                                d.prime = start[["dp"]],
                                crit.speed = start[["cs"]],
                                max.speed = start[["v0"]]
                              ),
                              control = nls.control(maxiter = 1000),
                              lower = lower,
                              upper = upper
  )
  
  cs.nls
  
}

# five parameter model
five.param <- function(data,
                       lower = c(cs = 1, v0 = 4, a = NA, b = NA, f = NA),
                       upper = c(cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
                       start = c(cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
  
  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                                crit.speed + (max.speed - crit.speed)/ ((1 + exp(-a * (log(duration) - b)))^f),
                              data = data,
                              start = list(crit.speed = start[["cs"]],
                                           max.speed = start[["v0"]],
                                           a = start[["a"]],
                                           b = start[["b"]],
                                           f = start[["f"]]),
                              control = nls.control(maxiter = 1000),
                              lower = lower,
                              upper = upper
  )
  
  cs.nls
  
}

# three parameter model
three.param <- function(data,
                        lower = c(dp = 10, cs = 1, v0 = 4),
                        upper = c(dp = 800, cs = 5.8, v0 = 12),
                        start = c(dp = 150, cs = 4, v0 = 9)) {
  
  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                                crit.speed + d.prime / (duration + d.prime / (max.speed - crit.speed)),
                              data = data,
                              start = list(d.prime = start[["dp"]],
                                           crit.speed = start[["cs"]],
                                           max.speed = start[["v0"]]),
                              control = nls.control(maxiter = 1000),
                              lower = lower,
                              upper = upper
  )
  
  cs.nls
  
}

# two parameter model
two.param <- function(data,
                      lower = c(dp = 10, cs = 1),
                      upper = c(dp = 800, cs = 5.8),
                      start = c(dp = 150, cs = 4)) {
  
  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                                d.prime/ duration + crit.speed,
                              data = data,
                              start = list(d.prime = start[["dp"]],
                                           crit.speed = start[["cs"]]),
                              control = nls.control(maxiter = 1000),
                              lower = lower,
                              upper = upper
  )
  
  cs.nls
  
}

# log.5p <- function(data,
#                    lower = c(cs = 1, v0 = 4, a = NA, b = NA, f = NA),
#                    upper = c(cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
#                    start = c(cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
#   
#   cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
#                                 crit.speed + (max.speed - crit.speed)/ ((1 + exp(-a * (log(duration) - b)))^f),
#                               data = data,
#                               start = list(crit.speed = start[["cs"]],
#                                            max.speed = start[["v0"]],
#                                            a = start[["a"]],
#                                            b = start[["b"]],
#                                            f = start[["f"]]),
#                               control = nls.control(maxiter = 1000),
#                               lower = lower,
#                               upper = upper
#   )
#   
#   cs.nls
#   
# }
# 
# log.4p <- function(data,
#                    lower = c(cs = 1, v0 = 4, a = NA, b = NA, f = NA),
#                    upper = c(cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
#                    start = c(cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
#   
#   cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
#                                 crit.speed + (max.speed - crit.speed)/ (1 + exp(-a * (log(duration) - b))),
#                               data = data,
#                               start = list(crit.speed = start[["cs"]],
#                                            max.speed = start[["v0"]],
#                                            a = start[["a"]],
#                                            b = start[["b"]]),
#                               control = nls.control(maxiter = 1000),
#                               lower = lower,
#                               upper = upper
#   )
#   
#   cs.nls
#   
# }
# 
# log.3p <- function(data,
#                    lower = c(cs = 1, v0 = 4, a = NA, b = NA, f = NA),
#                    upper = c(cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
#                    start = c(cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
#   
#   cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
#                                 (max.speed)/ (1 + exp(-a * (log(duration) - b))),
#                               data = data,
#                               start = list(max.speed = start[["v0"]],
#                                            a = start[["a"]],
#                                            b = start[["b"]]),
#                               control = nls.control(maxiter = 1000),
#                               lower = lower,
#                               upper = upper
#   )
#   
#   cs.nls
#   
# }
# 
# # quintic <- function(data,
# #                    lower = c(cs = 1, v0 = 4, a = NA, b = NA, f = NA),
# #                    upper = c(cs = 5.8, v0 = 12, a = NA, b = NA, f = NA),
# #                    start = c(cs = 4, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
# #   
# #   cs.nls <- minpack.lm::nlsLM(a + b*log(duration)^5 + c*log(duration)^4 + d*log(duration)^3 +
# #                                 f*log(duration)^2 + g*log(duration),
# #                               data = data,
# #                               
# #                               start = list(crit.speed = start[["cs"]],
# #                                            max.speed = start[["v0"]],
# #                                            a = start[["a"]],
# #                                            b = start[["b"]],
# #                                            f = start[["f"]],
# #                                            c = 1,
# #                                            g = 1),
# #                               control = nls.control(maxiter = 1000),
# #                               lower = c(lower, c = NA, g ,
# #                               upper = upper
# #   )
# #   
# #   cs.nls
# #   
# # }
# 
# bi.exp.5p <- .
# 
# log.4p.gompertz <- .
# 

# extended omni-domain model
omvd.extend <- function(data,
                        lower = c(dp = 10, cs = 1, v0 = 4, A = 0.1),
                        upper = c(dp = 800, cs = 5.8, v0 = 12, A = 60),
                        start = c(dp = 150, cs = 4, v0 = 9, A = 10),
                        TCVmax = 600
) {
  
  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~
                                crit.speed + d.prime *
                                (1 - exp(-1 * duration * (max.speed - crit.speed)/ d.prime))/
                                duration - A*log(duration/TCVmax)*(duration > TCVmax),
                              data = data,
                              start = list(
                                d.prime = start[["dp"]],
                                crit.speed = start[["cs"]],
                                max.speed = start[["v0"]],
                                A = start[["A"]],
                                TCVmax = 400
                              ),
                              control = nls.control(maxiter = 1000),
                              lower = c(lower, TCVmax = 1),
                              upper = c(upper, TCVmax = 600)
  )
  
  cs.nls
  
}

# power law model
power.model <- function(data,
                        lower = c(v0 = 4),
                        upper = c(v0 = 12),
                        start = c(v0 = 9)) {
  
  cs.nls <- minpack.lm::nlsLM(max.mean.speed ~ max.speed*(duration^(E - 1)),
                              data = data,
                              start = list(
                                max.speed = start[["v0"]],
                                E = 0.9
                              ),
                              control = nls.control(maxiter = 1000),
                              lower = c(lower, 0),
                              upper = c(upper, 1)
  )
  cs.nls
  
}


