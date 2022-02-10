cs.results.observed.list <- function(data = list(),
                                  data2 = list(),
                                  player.speed,
                                  global.data = FALSE,
                                  sample.rate = 10,
                                  dur = 600) {

  temp.df <- as.list(data)[[1]] == player.speed
  speed.col.name <- names(which(colSums(temp.df) > 0))

  if(global.data == TRUE) {


    data.df <- do.call(rbind, data)

    set.one.max.mean <- data.frame(duration =
                                     rep(
                                       seq(0.1,
                                           max(as.double(lapply(data, FUN = nrow)))/sample.rate,
                                           by = 1/sample.rate),
                                       times = length(data)
                                     )[1:nrow(data.df)],
                                   max.mean.speed = data.df[, speed.col.name]
    )

    colnames(set.one.max.mean) <- c("duration", "max.mean.speed")

    data2.df <- do.call(rbind, data2)

    set.two.max.mean <- data.frame(duration =
                                     rep(
                                       seq(0.1,
                                           max(as.double(lapply(data2, FUN = nrow)))/sample.rate,
                                           by = 1/sample.rate),
                                       times = length(data2)
                                     )[1:nrow(data2.df)],
                                   max.mean.speed = data2.df[, speed.col.name]
    )

    colnames(set.two.max.mean) <- c("duration", "max.mean.speed")

    }


  if(global.data == FALSE) {

  set.one.max.mean <- do.call(rbind,
                              lapply(data,
                                     function(x) data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                                                            max.mean.speed = max.mean.speed(x[, speed.col.name],
                                                                                            n = dur*sample.rate)))
  )

  set.two.max.mean <- do.call(rbind,
                              lapply(data2,
                                     function(x) data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                                                            max.mean.speed = max.mean.speed(x[, speed.col.name],
                                                                                            n = dur*sample.rate)))
  )

  }

  set.one.max.mean.global <- do.call(rbind,
                                     lapply(
                                       split(
                                         set.one.max.mean, as.factor(set.one.max.mean$duration)
                                       ),
                                       function(x) {
                                         return(x[which.max(x$max.mean.speed), ])
                                       }
                                     )
  )

  set.two.max.mean.global <- do.call(rbind,
                                     lapply(
                                       split(
                                         set.two.max.mean, as.factor(set.two.max.mean$duration)
                                       ),
                                       function(x) {
                                         return(x[which.max(x$max.mean.speed), ])
                                       }
                                     )
  )

  max.mean.compare <- list(set.one.max.mean.global,
                           set.two.max.mean.global)

  max.mean.compare

}
