
#' @title Function test.normality
#' @description Tests the normality of a single variable using Shapiro-Wilk's test.
#' @param x tested variable
#' @param output if "none", nothing is displayed, if *"figure"*, a figure is
#' displayed, if *"message"* a message is displayed, and  if *"all"*, a message and
#' a figure are displayed. Try and see !
#' @param var.name the name to be displayed. The default value is the varname of x.
#' Try and see !
#'
#' @return a list with the following members :
#' * `pval` : p-value of the normality test
#' * `result` : text containing the results from the normality test
#' @export
#'
#' @examples
#' #---------------------------------------------
#' # apply test.normality to 1 variable
#' #---------------------------------------------
#' test.normality(arrivals$time, var.name = "Arrival Time")
#' test.normality(arrivals$weight, var.name = "Person's Weight", output = "figure")
#' test.normality(arrivals$age, var.name = "Person's Age", output = "all")
#'
#' #---------------------------------------------
#' # apply test.normality to multiple variables
#' #---------------------------------------------
#' for (i in 1:ncol(iris)) {
#'   if (is.numeric(iris[, i]) == TRUE) {
#'     test.normality(iris[, i], var.name = names(iris)[i], output = "message")
#'   } else {
#'     cat("The variable", names(iris)[i], "is not numerical\n")
#'   }
#' }

test.normality <- function(x, output = c("none", "figure", "message", "all"), var.name = deparse(substitute(x))) {
  output <- match.arg(output)
  if (!is.numeric(x)) {
    text <- paste0("The variable ", var.name, " is not numerical")
    if (output %in% c("message", "all")) {
      cat(text, "\n \n")
    }
    return(list(pval = NA, result = text))
  }
  if (output %in% c("figure", "all")) {
    graphics::par(mfrow = c(1, 2))
    test <- stats::dnorm(x, mean = mean(x), sd = stats::sd(x))
    histo <- graphics::hist(x, plot = FALSE)
    limiteY <- max(test, histo$density)
    graphics::hist(x, prob = TRUE, col = "lightgrey", main = "", plot = TRUE, ylim = c(0, limiteY), xlab = "", ylab = "Density")
    graphics::curve(stats::dnorm(x, mean = mean(x), sd = stats::sd(x)), col = "blue", add = TRUE)
    stats::qqnorm(x, main = "")
    stats::qqline(x)
    graphics::legend("topleft", legend = paste(c("Mean:", "Standard deviation:"), round(c(mean(x), stats::sd(x)), 2)))
    graphics::mtext(paste("Normality test for the variable", var.name), 3, outer = TRUE, line = -2, cex = 1.5)
  }
  pvalue <- stats::shapiro.test(x)$p.value
  if (pvalue > .05) {
    text <- paste0("The variable ", var.name, " is normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  } else {
    text <- paste0("The variable ", var.name, " is not normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  }
  if (output %in% c("message", "all")) {
    cat(text, "\n \n")
  }
  return(list(pval = pvalue, result = text))
}


#' @title Function normality.df
#' @description Tests the normality of a all numerical variables of a given
#' data.frame using Shapiro-Wilk's test.
#' @param df the data frame to be tested
#' @param output if *"none"*, nothing is displayed, if *"figure"*, a figure is
#' displayed, if *"message"* a message is displayed, and  if *"all"*, a message
#' and a figure are displayed
#'
#' @return a list with the following members :
#' * `pvalues` : p-values of the normality test for the data frame
#' * `results` : text containing the results from the normality test for the
#' data frame
#' @export
#'
#' @examples
#' #---------------------------------------------
#' # apply normality.df to a data.frame
#' #---------------------------------------------
#' normality.df(arrivals, output = "all")

normality.df <- function(df, output = c("none", "figure", "message", "all")) {
  try(df<-data.frame(df), silent = TRUE)
  if (!exists("df", mode = "list")) stop("Please enter a valid data.frame!")
  output <- match.arg(output)
  pvalues <- c()
  results <- c()
  for (i in 1:ncol(df)) {
    res <- test.normality(df[, i], output = output, var.name = names(df)[i])
    pvaluei <- res$pval
    resulti <- res$result
    names(pvaluei) <- names(df)[i]
    names(resulti) <- names(df)[i]
    pvalues <- c(pvalues, pvaluei)
    results <- c(results, resulti)
  }
  return(list(pvalues = pvalues, results = results))
}



