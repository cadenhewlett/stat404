% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/section7.R
\name{full_factorial}
\alias{full_factorial}
\title{Full Factorial Design ANOVA}
\usage{
full_factorial(df, alpha = 0.05, plus_first = FALSE)
}
\arguments{
\item{df}{A data frame representing a full factorial experimental design. Each column represents a factor with
two levels (-1 and 1), and each row represents an experimental run.}

\item{alpha}{The significance level (alpha) used for hypothesis testing. The default value is 0.05.}

\item{plus_first}{Logical; if TRUE, uses positive-first ordering in generating the planning matrix for the ANOVA analysis.
This parameter is passed to \code{\link{generate_planning_matrix}}.}
}
\value{
A data frame representing the ANOVA table with columns for source, degrees of freedom (DF), sum of squares (SS),
mean sum of squares (MSS), F-statistics (F), p-values (P), and a significance indicator (signif) for each source.
The table includes sources for each combination of factor levels, error, and total.
}
\description{
This function performs a full factorial design ANOVA on experimental data with a high/low design.
}
\examples{
# Create a data frame with response data for two factors
data <- data.frame(
  High = c(30, 40, 35, 45, 38, 50, 23, 28),
  Low = c(20, 25, 22, 30, 28, 35, 20, 25)
)
# Perform a full factorial ANOVA
full_factorial(data)

}
\seealso{
\code{\link{generate_planning_matrix}} for the function used to generate the planning matrix.
}
