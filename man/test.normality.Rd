% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NormalityTest.R
\name{test.normality}
\alias{test.normality}
\title{Function test.normality}
\usage{
test.normality(
  x,
  output = c("none", "figure", "message", "all"),
  var.name = deparse(substitute(x))
)
}
\arguments{
\item{x}{tested variable}

\item{output}{if "none", nothing is displayed, if \emph{"figure"}, a figure is
displayed, if \emph{"message"} a message is displayed, and  if \emph{"all"}, a message and
a figure are displayed. Try and see !}

\item{var.name}{the name to be displayed. The default value is the varname of x.
Try and see !}
}
\value{
a list with the following members :
\itemize{
\item \code{pval} : p-value of the normality test
\item \code{result} : text containing the results from the normality test
}
}
\description{
Tests the normality of a single variable using Shapiro-Wilk's test.
}
\examples{
#---------------------------------------------
# apply test.normality to 1 variable
#---------------------------------------------
test.normality(arrivals$time, var.name = "Arrival Time")
test.normality(arrivals$weight, var.name = "Person's Weight", output = "figure")
test.normality(arrivals$age, var.name = "Person's Age", output = "all")

#---------------------------------------------
# apply test.normality to multiple variables
#---------------------------------------------
for (i in 1:ncol(iris)) {
  if (is.numeric(iris[, i]) == TRUE) {
    test.normality(iris[, i], var.name = names(iris)[i], output = "message")
  } else {
    cat("The variable", names(iris)[i], "is not numerical\n")
  }
}
}
