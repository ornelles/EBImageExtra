% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROI-class.R
\docType{class}
\name{Roi-class}
\alias{Roi-class}
\alias{Roi}
\title{Roi-constructor}
\arguments{
\item{object}{A \code{Image} object}
}
\value{
\code{Image} object with added "Roi" class and "loc" slot
}
\description{
This class extends the \code{Image} object from \code{EBImage} 
to include a class named "Roi" and a slot named "loc" to hold the
original location of the region of interest. This slot can be
reached by the accessor function \code{loc} or simply by
\code{attr(obj, "loc"} or by \code{obj@loc}. Previously, \code{roxygen2}
seemed to require that the parent (\code{EBImage}) library be loaded
before it can recognize this directive...
}
\details{
The function \code{Roi} is an S4 constructor to create
the extended S4 class named "Roi".
}
\section{Slots}{

\describe{
\item{\code{loc}}{A length-two \code{list} of length-two numeric
vectors for the points defining the region of interest}
}}

