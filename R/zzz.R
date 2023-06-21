.onLoad <- function(libname, pckgname){
  packageStartupMessage("\n",
  "critspeed: Modelling critical speed from player tracking data in R\n\n",
  "A walkthrough can be found by running critspeed_help() or by visiting https://github.com/aaronzpearson/critspeed \n")
}


# opens URL in browser for package walkthrough
#' @export
critspeed_help <- function() {browseURL("https://github.com/aaronzpearson/critspeed")}