crit.speed.results <- function(player.speed,
                               global.data = FALSE,
                               sample.rate = 10,
                               dur = 600,
                               d.prime.estim = 100,
                               crit.speed.estim = 3.5,
                               max.speed.estim = 12) {

  if(global.data == FALSE) { # make 2 if statements for consistency in the package

    dat = max.mean.speed.df(player.speed = player.speed,
                            sample.rate = sample.rate,
                            dur = dur)

  } else {

    dat <- data.frame(max.mean.speed = player.speed,
                      duration = (1:length(player.speed))/ sample.rate)

  }

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

  two.param <- cs.two.param(data = dat,
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
