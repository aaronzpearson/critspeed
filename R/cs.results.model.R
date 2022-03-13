#' Critical Speed Model Results
#'
#' @param player.speed speed column
#' @param global.data default set to FALSE
#' @param sample.rate in Hz
#' @param dur duration
#' @param d.prime.estim default set to 100
#' @param crit.speed.estim default set to 3.5
#' @param max.speed.estim default set to 12
#' @param cv.2 min duration for CV2. Default set to 120 s
#'
#' @export
crit.speed.results <- function(player.speed,
                               global.data = FALSE,
                               sample.rate = 10,
                               cv.2 = 120, # minimum duration (default = 2 minutes)
                               dur = 600,
                               d.prime.estim = 100,
                               crit.speed.estim = 3.5,
                               max.speed.estim = 12) {

  if(global.data == FALSE) {

    dat = max.mean.speed.df(player.speed = player.speed,
                            sample.rate = sample.rate,
                            dur = dur)

  } else {

    dat <- data.frame(max.mean.speed = player.speed,
                      duration = (1:length(player.speed))/ sample.rate)

  }

  dat.cv2 <- subset(dat, duration >= cv.2)

  extended.model <- cs.extended.model(data = dat,
                                      d.prime.estim = d.prime.estim,
                                      crit.speed.estim = crit.speed.estim,
                                      max.speed.estim = max.speed.estim)

  five.param <- cs.five.param(data = dat,
                              crit.speed.estim = crit.speed.estim,
                              max.speed.estim = max.speed.estim)

  three.param <- cs.three.param(data = dat,
                                d.prime.estim = d.prime.estim,
                                crit.speed.estim = crit.speed.estim,
                                max.speed.estim = max.speed.estim)

  two.param <- cs.two.param(data = dat.cv2,
                            d.prime.estim = d.prime.estim,
                            crit.speed.estim = crit.speed.estim)

  exponential.model <- cs.exponential(data = dat,
                                      crit.speed.estim = crit.speed.estim,
                                      max.speed.estim = max.speed.estim)

  exponential.decay.model <- cs.exponential.decay(data = dat)

  # inverse.model <- cs.inverse(data = dat,
  #                             d.prime.estim = d.prime.estim,
  #                             crit.speed.estim = crit.speed.estim)

  cs.results <- data.frame(model = c("Exponential Decay",
                                     "Exponential Model",
                                     "Two Parameter",
                                     "Three Parameter",
                                     "Five Parameter",
                                     "OmniDomain"),

                           critical.speed = c(
                             coef(exponential.decay.model)[[3]],
                             coef(exponential.model)[[1]],
                             coef(two.param)[[2]],
                             coef(three.param)[[2]],
                             coef(five.param)[[4]],
                             coef(extended.model)[[2]]
                           ),



                           d.prime = c(
                             NA,
                             NA,
                             coef(two.param)[[1]],
                             coef(three.param)[[1]],
                             NA,
                             coef(extended.model)[[1]]
                           ),

                           max.speed = c(
                             coef(exponential.decay.model)[[1]],
                             # predict(exponential.decay.model, newdata = data.frame(duration = 1)),
                             coef(exponential.model)[[2]],
                             NA,
                             coef(three.param)[[3]],
                             coef(five.param)[[5]],
                             coef(extended.model)[[3]]
                           ),

                           rse = c(
                             summary(exponential.decay.model)[[3]],
                             summary(exponential.model)[[3]],
                             summary(two.param)[[3]],
                             summary(three.param)[[3]],
                             summary(five.param)[[3]],
                             summary(extended.model)[[3]]
                           ),

                           rss = c(
                             summary(exponential.decay.model)[[3]]^2 * summary(exponential.decay.model)[[4]][2],
                             summary(exponential.model)[[3]]^2 * summary(exponential.model)[[4]][2],
                             summary(two.param)[[3]]^2 * summary(two.param)[[4]][2],
                             summary(three.param)[[3]]^2 * summary(three.param)[[4]][2],
                             summary(five.param)[[3]]^2 * summary(five.param)[[4]][2],
                             summary(extended.model)[[3]]^2 * summary(extended.model)[[4]][2]
                           ),

                           aic = c(
                             AIC(exponential.decay.model),
                             AIC(exponential.model),
                             AIC(two.param),
                             AIC(three.param),
                             AIC(five.param),
                             AIC(extended.model)
                           ),

                           bic = c(
                             BIC(exponential.decay.model),
                             BIC(exponential.model),
                             BIC(two.param),
                             BIC(three.param),
                             BIC(five.param),
                             BIC(extended.model)
                           )
  )

  cs.results
}
