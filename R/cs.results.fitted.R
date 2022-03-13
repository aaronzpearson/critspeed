#' Fitted Results
#'
#' @param player.speed speed column
#' @param global.data default set to FALSE
#' @param sample.rate in Hz
#' @param cv.2 start duration for CV2
#' @param dur duration
#' @param d.prime.estim default set to 100
#' @param crit.speed.estim default set to 3.5
#' @param max.speed.estim default set to 12
#'
#' @export
cs.results.fitted <- function(player.speed,
                                global.data = FALSE,
                                sample.rate = 10,
                                cv.2 = 120,
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

  dat.fit <- dat
  dat.fit$dur.log = log10(dat.fit$duration)

  dat.fit$omni.pred <- predict(extended.model)
  dat.fit$omni.resid <- as.numeric(residuals(extended.model))

  dat.fit$five.p.pred <- predict(five.param)
  dat.fit$five.p.resid <- as.numeric(residuals(five.param))

  dat.fit$three.p.pred <- predict(three.param)
  dat.fit$three.p.resid <- as.numeric(residuals(three.param))

  dat.fit$exp.pred <- predict(exponential.model)
  dat.fit$exp.resid <- as.numeric(residuals(exponential.model))

  dat.cv2$two.p.pred <- predict(two.param)
  dat.cv2$two.p.resid <- dat.cv2$max.mean.speed - predict(two.param)

  dat.fit <- merge(dat.fit,
                   dat.cv2,
                   by = c("duration", "max.mean.speed"),
                   all = TRUE)

  dat.fit <- dat.fit[order(dat.fit$duration), , drop = FALSE]

  return(dat.fit)

}
