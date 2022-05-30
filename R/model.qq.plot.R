#' Model Q-Q Plots
#' 
#' This function is a helper function for `model.plots` when `plots = "qq"`. 
#' 
#' When calling this function explicitly, the argument `model.fits` takes on the data frame that 
#' is returned when calling `model.fits`.
#'
#' @param model.fits data set returned when calling critspeed::model.fits
#'
#' @export
model.qq.plot <- function(model.fits) {
  
  cs.model.normality.plot(model.fits = model.fits)
  
}


cs.model.normality.plot <- function(model.fits) {
  
  cs.fit = model.fits
  
  qq.two.param <- function() {
    
    qqplot(cs.fit$two.p.pred, cs.fit$max.mean.speed,
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    
    qqline(cs.fit$two.p.pred, col = "grey")
    
    title(main = "2P Q-Q Plot")
    
    
  }
  
  qq.three.param <- function() {
    
    qqplot(cs.fit$three.p.pred, cs.fit$max.mean.speed,
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    
    qqline(cs.fit$three.p.pred, col = "grey")
    
    title(main = "3P Q-Q Plot")
    
    
  }
  
  qq.five.param <- function() {
    
    qqplot(cs.fit$five.p.pred, cs.fit$max.mean.speed,
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    
    qqline(cs.fit$five.p.pred, col = "grey")
    
    title(main = "5P Q-Q Plot")
    
    
  }
  
  qq.omni <- function() {
    
    qqplot(cs.fit$omni.pred, cs.fit$max.mean.speed,
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    
    qqline(cs.fit$omni.pred, col = "grey")
    
    title(main = "OmVD Q-Q Plot")
    
    
  }
  
  
  qq.fit <- function() {
    
    par(mfrow = c(2, 2))
    
    qq.two.param()
    qq.three.param()
    qq.five.param()
    qq.omni()
    
  }
  
  qq.fit()
  
  par(mfrow = c(1,1))
  
}