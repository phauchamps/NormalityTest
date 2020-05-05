#' Arrivals
#'
#' @description
#' Arrivals data of men/women in a shop, starting at an arbitrary time
#'
#' @format
#' A `data.frame` with `r nrow(arrivals)` rows and `r ncol(arrivals)` columns:
#'
#' * `gender` is a factor with two levels : *female* and *male*.
#' * `time` is the person arrival time, in minutes.
#' * `weight` is the person weight, as disclosed by her/himself, in kilograms.
#' * `age` is the person age, as disclosed by her/himself, in years.
#'
#' @author Philippe Hauchamps <philippe.hauchamps@student.uclouvain.be>
#'
"arrivals"
# set.seed(20200503)
# nData <- 1000
# arrivals <- data.frame( gender = rbinom(nData, size = 1, prob = 0.5),
#                         time = rpois(n = nData, lambda = 3),
#                         weight = rnorm(n = nData, mean = 60, sd = 5),
#                         age = rnorm(n = nData, mean = 35, sd = 10) )
#
# arrivals$weight <- arrivals$weight + 20*arrivals$gender
# arrivals$gender <- factor(arrivals$gender)
# levels(arrivals$gender) <- c("female", "male")
# head(arrivals,20)

