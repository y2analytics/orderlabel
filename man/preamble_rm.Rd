% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preamble_rm.R
\name{preamble_rm}
\alias{preamble_rm}
\title{Auto remove question preambles for multiple choice/matrix questions}
\usage{
preamble_rm(dataset, var = prompt, before_symbol = "- ")
}
\arguments{
\item{dataset}{The name of the data frame for the function to modify, usually piped in after running freqs. You almost never need any arguments in this function.}

\item{var}{DEFAULT = prompt; If you use the prompt = TRUE argument in freqs(), this default should be perfect for you.}

\item{before_symbol}{DEFAULT = "- "; preamble_rm will remove everything before this symbol or string of symbols}
}
\description{
Takes a dataframe (frequencies) and removes the usual question preambles so that you are left with only the labels you care about.
For example, "What is your favorite color? - Blue" becomes "Blue"
}
\examples{
frequencies <- tibble::tibble(
  label = c('Selected','Selected','Selected','Selected','Selected'),
  result = c(.25, .15, .20, .10, .30),
  prompt = c(
  'What is your favorite color? - Blue',
  'What is your favorite color? - Green',
  'What is your favorite color? - Yellow',
  'What is your favorite color? - Purple',
  'What is your favorite color? - Orange'
  ),
  prompt2 = c(
  'What is your favorite color? ... Blue',
  'What is your favorite color? ... Green',
  'What is your favorite color? ... Yellow',
  'What is your favorite color? ... Purple',
  'What is your favorite color? ... Orange'
  )
)

preamble_rm(frequencies)
preamble_rm(frequencies, var = prompt2, before_symbol = '\\\\. ')
frequencies \%>\% preamble_rm()
}
