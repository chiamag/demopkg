#' Calculate the logarithm of the odds ofa probability
#'
#' @export
#' @param p A probability (a value > 0 and < 1)
#' @return The logarithm (to base e) of the probability
#' @examples
#' logit(0.5)
#' logit(0.72)
#' logit(0.85)
logit <- function(p){
  log(p/(1-p))
}


#' Calculate the inverse of the logit function
#'
#' @export
#' @param logodds The logarithm (to base e) of some probability
#' @return The probability that is the inverse of the log odds
#' @examples
#' ilogit(3)
#' ilogit(-1)
#' ilogit(0)
ilogit <- function(logodds){
  1/(1 + exp(- logodds))
}


#' Plot a binomial likelihood fuction
#'
#' @export
#' @param n Number of trials
#' @param m Number of successes
#' @return A ggplot of the likelihood function
#' @examples
#' likelihood(75,25)
#' likelihood(100,10)
#' likelihood(20,5)
#' @import ggplot2
likelihood <- function(n= 100,m = 60){
  data_df <- dplyr::tibble(x = seq(0,1, length.out = 1000),
                    y = x ^ m * (1 - x) ^ (n - m))
  ggplot(data_df, aes(x = x, y = y)) + geom_line()
}
