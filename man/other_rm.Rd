% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_rm.R
\name{other_rm}
\alias{other_rm}
\title{Auto change "Other please specify"s into "Other"}
\usage{
other_rm(dataset, var = NULL, remove = FALSE)
}
\arguments{
\item{dataset}{The name of the data frame for the function to modify, usually piped in after running freqs. You almost never need any arguments in this function.}

\item{var}{DEFAULT = NULL; If kept at NULL, it will already remove parenthetical text in 'label', 'variable', and 'group_var' vars. You can add an additional variable by specifying it here.}

\item{remove}{DEFAULT = FALSE; If set to TRUE, will remove categories of "Other" and "None of the above" from the frequencies}
}
\description{
Takes a dataframe (frequencies) and replaces the usual variations of "Other please specify" into Other. Also standardizes "None of the above" and "Prefer not to say" options. Additionally, removes all extra text after parentheses opening. Does this for the 'label', 'variable', and 'group_var' vars.
}
\examples{
frequencies <- tibble::tibble(
  label = c(
  'Brand 1',
  'Brand 2',
  'Brand 3',
  'Other (please specify)',
  'None of the above brands...'
  ),
  result = c(.25, .15, .20, .10, .30),
  other_var = c('x', 'y', 'z', 'z (test)', 'None')
)

frequencies \%>\% other_rm(remove = TRUE)
frequencies \%>\% other_rm(var = other_var)
}
\keyword{extra}
\keyword{none}
\keyword{other}
