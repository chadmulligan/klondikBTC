% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBTC.R
\name{getBTC}
\alias{getBTC}
\title{Downloading and flattening ina df all transactions info from a set of BTC addresses}
\usage{
getBTC(addresses = c(), apikey = character())
}
\arguments{
\item{addresses}{a vector of addresses hash}

\item{apikey}{Blockchain.info API key}
}
\value{
Returns a data frame
}
\description{
\code{getBTC} is a wrapper - returns all transactions info of a vector of addresses from blockchain.info API.
}
