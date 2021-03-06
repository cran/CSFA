% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_structure.R
\name{CSanalysis}
\alias{CSanalysis}
\title{Connectivity Score Analysis.}
\usage{
CSanalysis(querMat, refMat, type, ...)
}
\arguments{
\item{querMat}{Query matrix (Rows = genes and columns = compounds)}

\item{refMat}{Reference matrix}

\item{type}{Type of Factor Analysis or Zhang & Gant ( \code{"CSfabia"}, \code{"CSmfa"}, \code{"CSpca"}, \code{"CSsmfa"} or \code{"CSzhang"})}

\item{...}{Additional parameters for analysis}
}
\value{
An object of the S4 Class \code{\link{CSresult-class}}.
}
\description{
Doing a CS analysis, interactively generating graphs. See specific type for additional parameteres.\cr
Types:\cr
\itemize{
\item \code{\link[=CSanalysis,matrix,matrix,CSzhang-method]{Zhang and Gant}}
\item \code{\link[=CSanalysis,matrix,matrix,CSmfa-method]{MFA}}
\item \code{\link[=CSanalysis,matrix,matrix,CSpca-method]{PCA}}
\item \code{\link[=CSanalysis,matrix,matrix,CSsmfa-method]{Sparse MFA}}
\item \code{\link[=CSanalysis,matrix,matrix,CSfabia-method]{FABIA}}
}
}
\examples{
\dontshow{
data("dataSIM",package="CSFA")
Mat1 <- dataSIM[,c(1:6)]
Mat2 <- dataSIM[,-c(1:6)]
ZHANG_analysis <- CSanalysis(Mat1,Mat2,"CSzhang")
}
 
\donttest{
data("dataSIM",package="CSFA")
Mat1 <- dataSIM[,c(1:6)]
Mat2 <- dataSIM[,-c(1:6)]

MFA_analysis <- CSanalysis(Mat1,Mat2,"CSmfa")
FABIA_analysis <- CSanalysis(Mat1,Mat2,"CSfabia")
ZHANG_analysis <- CSanalysis(Mat1,Mat2,"CSzhang")
}
}
