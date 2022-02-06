cs.extended.model <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nlsLM(max.mean.speed ~
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

cs.five.param <- function(data, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nlsLM(max.mean.speed ~
                  crit.speed + (max.speed - crit.speed)/ ((1 + exp(-a * log(duration) - b))^f),
                data = data,
                start = list(a = -2.71,
                             b = 2.195,
                             f = 0.2,
                             crit.speed = crit.speed.estim,
                             max.speed = max.speed.estim),
                control = nls.control(maxiter = 1000),
                lower = c(-20, -20, -5, 1, 5.8),
                upper = c(20, 20, 5, 5.8, 12)
                  )

  cs.nls

}

cs.three.param <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nlsLM(max.mean.speed ~
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

cs.two.param <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5) {

  cs.nls <- nlsLM(max.mean.speed ~
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


cs.exponential <- function(data, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nlsLM(max.mean.speed ~
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

cs.exponential.decay <- function(data, crit.speed.estim = 3.5, max.speed.estim = 12) {


  theta.0 <- min(data$max.mean.speed) * 0.5
  model.0 <- lm(log(max.mean.speed - theta.0) ~ duration, data = data)
  alpha.0 <- exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]

  cs.nls <- nlsLM(max.mean.speed ~ alpha * exp(beta * duration) + theta,
                data = data,
                start = list(alpha = alpha.0,
                            beta = beta.0,
                            theta = theta.0),
                control = nls.control(maxiter = 1000)
                )

  cs.nls

}
