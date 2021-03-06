% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/text_util.R
\name{split_nchars_numeric}
\alias{split_nchars_numeric}
\title{For reading structured text}
\usage{
split_nchars_numeric(string, no_chars)
}
\arguments{
\item{string}{The character string to operate on}

\item{no_chars}{Integer: Split string into pieces with 'no_chars' characters}
}
\value{
A vector with the split string
}
\description{
Take a string, split it into individual characters, and recombine them
into separate pieces of no_chars length. Then convert these to numeric,
and output as a vector
}
\examples{
x='abcdefghijkl'
}

