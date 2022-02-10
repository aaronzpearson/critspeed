cs.results.model.list <- function(data = list(),
                                  data2 = list(),
                                  player.speed,
                                  sample.rate = 10,
                                  dur = 600) {

  temp.df <- as.list(data)[[1]] == player.speed
  speed.col.name <- names(which(colSums(temp.df) > 0))

  set.one.max.mean <- do.call(rbind,
                              lapply(data,
                                     function(x) data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                                                            max.mean.speed = max.mean.speed(x[, speed.col.name],
                                                                                            n = dur*sample.rate)))
  )

  set.one.max.mean.global <- do.call(rbind,
                                     lapply(
                                       split(
                                         set.one.max.mean, as.factor(set.one.max.mean$duration)
                                       ),
                                       function(x) {
                                         return(x[which.max(x$max.mean.speed),])
                                       }
                                     )
  )

  set.two.max.mean <- do.call(rbind,
                              lapply(data2,
                                     function(x) data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                                                            max.mean.speed = max.mean.speed(x[, speed.col.name],
                                                                                            n = dur*sample.rate)))
  )

  set.two.max.mean.global <- do.call(rbind,
                                     lapply(
                                       split(
                                         set.two.max.mean, as.factor(set.two.max.mean$duration)
                                       ),
                                       function(x) {
                                         return(x[which.max(x$max.mean.speed),])
                                       }
                                     )
  )

  max.mean.compare <- list(set.one.max.mean.global,
                           set.two.max.mean.global)

  max.mean.compare

}
