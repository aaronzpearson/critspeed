#' Critical Speed List Results
#'
#' @param data group 1 of sessions
#' @param data2 group 2 of sessions
#' @param speed.col column name for player speed
#' @param global.data default set to FALSE
#' @param sample.rate in Hz
#' @param dur duration
#'
#' @export
cs.results.observed.list <- function(data = list(),
                                     data2 = list(),
                                     speed.col = "speed.col.name",
                                     global.data = FALSE,
                                     sample.rate = 10,
                                     dur = 600) {

  if(global.data == TRUE) {


    set.one.max.mean <- do.call(rbind,
                                lapply(
                                  data,
                                  function(x) {
                                    data.frame(duration = 1:nrow(x)/sample.rate,
                                               max.mean.speed = x[, speed.col])
                                  }
                                )
    )

    colnames(set.one.max.mean) <- c("duration", "max.mean.speed")



    set.two.max.mean <- do.call(rbind,
                                lapply(
                                  data2,
                                  function(x) {
                                    data.frame(duration = 1:nrow(x)/sample.rate,
                                               max.mean.speed = x[, speed.col])
                                  }
                                )
    )

    colnames(set.two.max.mean) <- c("duration", "max.mean.speed")

  }


  if(global.data == FALSE) { # this is the bottleneck (by far)

    # set.one.max.mean <- do.call(rbind,
    #                             lapply(data,
    #                                    function(x) data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
    #                                                           max.mean.speed = max.mean.speed(x[, speed.col],
    #                                                                                           n = dur*sample.rate)))

    set.one.max.mean <- data.frame()
    for(i in seq_along(data)) {

      max.mean.speed.vect = max.mean.speed(data[[i]][, speed.col],
                                           n = dur * sample.rate)

      temp.df <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                            max.mean.speed = max.mean.speed.vect)


      set.one.max.mean <- rbind(set.one.max.mean, temp.df)


    }

    set.two.max.mean <- data.frame()
    for(j in seq_along(data2)) {

      max.mean.speed.vect2 = max.mean.speed(data2[[j]][, speed.col],
                                            n = dur * sample.rate)

      temp.df <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                            max.mean.speed = max.mean.speed.vect2)


      set.two.max.mean <- rbind(set.two.max.mean, temp.df)


    }

    # set.two.max.mean <- do.call(rbind,
    #                             lapply(data2,
    #                                    function(x) data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
    #                                                           max.mean.speed = max.mean.speed(x[, speed.col],
    #                                                                                           n = dur*sample.rate)))
    # )

  }

  set.one.max.mean <- data.table(set.one.max.mean)
  set.one.max.mean.global <- set.one.max.mean[, max(max.mean.speed, na.rm = TRUE), by = duration]
  colnames(set.one.max.mean.global) <- c("duration", "max.mean.speed")

  set.two.max.mean <- data.table(set.two.max.mean)
  set.two.max.mean.global <- set.two.max.mean[, max(max.mean.speed, na.rm = TRUE), by = duration]
  colnames(set.two.max.mean.global) <- c("duration", "max.mean.speed")


  max.mean.compare <- list(set.one.max.mean.global,
                           set.two.max.mean.global)

  max.mean.compare

}
