#' Fitted Plots
#'
#' @param player.speed speed column
#' @param global.data default set to FALSE
#' @param sample.rate in Hz
#' @param dur duration
#' @param cv.2 min duration for CV2
#' @param d.prime.estim default set to 100
#' @param crit.speed.estim default set to 3.5
#' @param max.speed.estim default set to 12
#' @param log.dur default set to FALSE. Takes log base 10 of duration
#' @param resid residual plots. Not yet implemented
#'
#' @export
cs.results.plot <- function(player.speed,
                            global.data = FALSE,
                            sample.rate = 10,
                            dur = 600,
                            cv.2 = 120,
                            d.prime.estim = 150,
                            crit.speed.estim = 3.5,
                            max.speed.estim = 12,
                            log.dur = FALSE,
                            resid = FALSE) {

  cs.coef <- cs.results.model(player.speed = player.speed,
                                global.data = global.data,
                                sample.rate = sample.rate,
                                dur = dur,
                                cv.2 = cv.2,
                                d.prime.estim = d.prime.estim,
                                crit.speed.estim = crit.speed.estim,
                                max.speed.estim = max.speed.estim)

  cs.fit <- cs.results.fitted(player.speed = player.speed,
                                global.data = global.data,
                                sample.rate = sample.rate,
                                dur = dur,
                                cv.2 = cv.2,
                                d.prime.estim = d.prime.estim,
                                crit.speed.estim = crit.speed.estim,
                                max.speed.estim = max.speed.estim)



  # observations w/ model fit
  p.obs <- function(log.dur = log.dur) {

    if(log.dur == FALSE) { plot(max.mean.speed ~ duration, cs.fit,
                                ylim = c(0, 12.5),
                                xlim = c(0, dur),
                                col = "grey",
                                ylab = "Max Mean Velocity (m/sec)",
                                xlab = "Duration (s)") }
    if(log.dur == TRUE) {

      ticks <- c(1, 10, 60, 120, 600)
      ticks.log <- log10(ticks)

      plot(max.mean.speed ~ dur.log, cs.fit,
           ylim = c(0, 12.5),
           xlim = c(0, log10(dur)),
           col = "grey",
           ylab = "Max Mean Velocity (m/s)",
           xlab = "Log(dur) (s)",
           xaxt = "n")

      axis(1,
           at = ticks.log,
           labels = ticks)

    }

  }

  p.five.param <- function(log.dur = log.dur) {

    p.obs(log.dur = log.dur)

    if(log.dur == FALSE) {lines(cs.fit$five.p.pred ~ cs.fit$duration, col = "red")
      text(x = dur - 50, y = cs.coef$critical.speed[5] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[5], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$five.p.pred ~ cs.fit$dur.log, col = "red")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[5] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[5], 2))))}

    title(main = "5P")

    abline(h = cs.coef$critical.speed[5])

  }

  p.three.param <- function(log.dur = log.dur) {

    p.obs(log.dur = log.dur)

    if(log.dur == FALSE) {lines(cs.fit$three.p.pred ~ cs.fit$duration, col = "green")
      text(x = dur - 50, y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[4], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$three.p.pred ~ cs.fit$dur.log, col = "green")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[4], 2))))}

    title(main = "CV3")

    abline(h = cs.coef$critical.speed[4])

  }

  p.two.param <- function(log.dur = log.dur) {

    p.obs(log.dur = log.dur)

    if(log.dur == FALSE) {lines(cs.fit$two.p.pred ~ cs.fit$duration, col = "blue")
      text(x = dur - 50, y = cs.coef$critical.speed[3] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[3], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$two.p.pred ~ cs.fit$dur.log, col = "blue")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[3] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[3], 2))))}

    title(main = "CV2")

    abline(h = cs.coef$critical.speed[3])

  }

  p.omni <- function(log.dur = log.dur) {

    p.obs(log.dur = log.dur)

    if(log.dur == FALSE) {lines(cs.fit$omni.pred ~ cs.fit$duration, col = "black")
      text(x = dur - 50, y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[6], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$omni.pred ~ cs.fit$dur.log, col = "black")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[6], 2))))}

    title(main = "OmVd")

    abline(h = cs.coef$critical.speed[6])

  }

  p.fit <- function(log.dur = log.dur) {

    par(mfrow = c(2, 2))

    p.two.param(log.dur = log.dur)
    p.three.param(log.dur = log.dur)
    p.five.param(log.dur = log.dur)
    p.omni(log.dur = log.dur)

  }

  # resid
  # p.five.param.resid <- function(log.dur = log.dur) { # remove redundant axis labels and tic values (keep tics though)
  #
  #   if(log.dur == FALSE) { plot(five.p.resid ~ duration, cs.fit,
  #                               xlim = c(0, dur),
  #                               ylab = "Max Mean Velocity (m/sec)",
  #                               xlab = "Duration (s)") }
  #   if(log.dur == TRUE) { plot(max.mean.speed ~ dur.log, cs.fit,
  #                              ylim = c(0, 12.5),
  #                              xlim = c(0, log(dur)),
  #                              col = "grey",
  #                              ylab = "Max Mean Velocity (m/s)",
  #                              xlab = "Log(dur) (s)") }
  #
  #   title(main = "5P Resid")
  #
  #   abline(h = 0)
  #
  # }




  # output
  if(resid == FALSE) { p.fit(log.dur = log.dur) }
  # if(resid == TRUE) { p.resid(log.dur = log.dur) }

  par(mfrow = c(1,1))

}
