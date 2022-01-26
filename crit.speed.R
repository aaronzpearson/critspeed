cs.extended.model <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nls(max.mean.speed ~
                  crit.speed + d.prime * (1 - exp(-1 * duration * (max.speed - crit.speed)/ d.prime))/duration,
                data = data,
                start = list(
                  d.prime = d.prime.estim,
                  crit.speed = crit.speed.estim,
                  max.speed = max.speed.estim
                  ),
                control = nls.control(maxiter = 1000)
                )

  cs.nls

}

cs.five.param <- function(data, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nls(max.mean.speed ~
                  crit.speed + (max.speed - crit.speed)/ ((1 + exp(-a * log(duration) - b))^f),
                data = data,
                start = list(a = -2.71,
                             b = 2.195,
                             f = 0.2,
                             crit.speed = crit.speed.estim,
                             max.speed = max.speed.estim),
                control = nls.control(maxiter = 1000)
                  )

  cs.nls

}

cs.three.param <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nls(max.mean.speed ~
                  crit.speed + d.prime / (duration + d.prime / (max.speed - crit.speed)),
                data = data,
                start = list(d.prime = d.prime.estim,
                             crit.speed = crit.speed.estim,
                             max.speed = max.speed.estim),
                control = nls.control(maxiter = 1000)
  )

  cs.nls

}

cs.two.param <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5) {

  cs.nls <- nls(max.mean.speed ~
                  d.prime/ duration + crit.speed,
                data = data,
                start = list(d.prime = d.prime.estim,
                             crit.speed = crit.speed.estim),
                control = nls.control(maxiter = 1000)
  )

  cs.nls

}


cs.exponential <- function(data, crit.speed.estim = 3.5, max.speed.estim = 12) {

  cs.nls <- nls(max.mean.speed ~
                  crit.speed + (max.speed - crit.speed) * exp(-duration/ tau),
                data = data,
                start = list(
                  crit.speed = crit.speed.estim,
                  max.speed = max.speed.estim,
                  tau = 1
                  ),
                control = nls.control(maxiter = 1000)
  )

  cs.nls
}

cs.inverse <- function(data, d.prime.estim = 150, crit.speed.estim = 3.5) {

  cs.nls <- nls(max.mean.speed ~
                  d.prime * (1/ duration) + crit.speed,
                data = cs.dat,
                start = list(
                  d.prime = d.prime.estim,
                  crit.speed = crit.speed.estim
                ),
                control = nls.control(maxiter = 1000))

  cs.nls

}
