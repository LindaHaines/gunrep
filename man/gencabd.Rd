% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basics.R
\name{gencabd}
\alias{gencabd}
\title{Generate a cyclic auxiliary block design from an initial block}
\usage{
gencabd(m, nc, ib)
}
\arguments{
\item{m}{Number of treatments}

\item{ib}{Initial block - vector}

\item{nc.}{Number of controls}
}
\value{
abd Augmented block design. Otherwise
            0 for an invalid entry
}
\description{
Generate a cyclic auxiliary block design from an initial block
}
