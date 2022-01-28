max.median.speed. <- function(x, n = c("roeker"), sample.rate = 10) {

  if(length(n) == 1 & n != "roeker") {

    message("Error: n must be greater than 1L")
    stop()

  }

  if(n == "roeker") {

    n. = c(0.3, 0.5, 1, 2, 3, 4,
           5, 6.5, 10, 13.5, 18,
           30, 60, 120, 300, 600,
           900, 1200, 1800, 2400,
           2700) * sample.rate

  } else {

    n. = n * sample.rate

  }


  tryCatch(

    expr = {
      do.call(
        rbind,
        lapply(
          data.table::frollapply(x, n = n., FUN = Rfast::med),
          max, na.rm = TRUE
        )
      )
    },

    error = function(error_condition){

      message("Warning: This function is written for 10 Hz data; n was rounded to handle errors.")

    },

    finally = {

      n. = round(n., 0)

      max.median.vel <- do.call(
        rbind,
        lapply(
          data.table::frollapply(x, n = n., FUN = Rfast::med),
          max, na.rm = TRUE)
      )

    }
  )

  invisible(gc)

  max.median.vel

}
