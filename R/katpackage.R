#' @title OR_95CI
#' @description
#' Using coef and SE, specify significance level and number of decimal places to get OR and 95% CI
#' @author Katherine Lu
#'
#' @param coef coefficient from regression model
#' @param se standard error
#' @param siglevel significance level
#' @param roundto number of decimals
#'
#' @return OR, 95% CI
#' @export
#'
#' @examples
#' logregr <- glm(y ~ x1 + x2, data=toydata, family = "binomial")
#' regsummary <- summary(logregr)
#' coef <- regsummary$coefficients[,1]
#' se <- regsummary$coefficients[,2]
#' OR_95CI(coef,se,0.05,2)
OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
                     return(ORresult)
}
