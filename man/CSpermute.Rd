% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permutation_functions.R
\name{CSpermute}
\alias{CSpermute}
\title{Permute CS results}
\usage{
CSpermute(querMat, refMat, CSresult, B = 500, mfa.factor = NULL,
  method.adjust = "none", verbose = TRUE, which = c(1, 3),
  cmpd.hist = NULL, color.columns = NULL, labels = TRUE,
  plot.type = "device", basefilename = NULL, MultiCores = FALSE,
  MultiCores.number = detectCores(logical = FALSE), MultiCores.seed = NULL,
  save.permutation = TRUE)
}
\arguments{
\item{querMat}{Query matrix (Rows = genes and columns = compounds).}

\item{refMat}{Reference matrix}

\item{CSresult}{A CSresult class object.}

\item{B}{Number of permutations.}

\item{mfa.factor}{If permuting a CSmfa result, mfa.factor will decide of which factor the p-values should be computed. If \code{NULL}, the factor chosen in CSanalysis will be chosen (the factor chosen in the CS slot of the CSresult). NOTE: If the mfa.factor is different from the factor in the CS slot, the CS slot will be overwritten with this new factor.}

\item{method.adjust}{Correction method of multiplicity adjusted p-values: "none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY" or "fdr". (Raw p-values are also always provided)}

\item{verbose}{If \code{TRUE}, progression dots     of the permutation analysis will be printed.}

\item{which}{Choose which plot to draw:
\enumerate{
\item A volcano plot of the -log(p-values) versus the observed connection scores.
\item A histogram of the permuted connection scores under the null hypothesis for a specific compound. A vertical line(s) is added for the observed CS and its p-value. The \code{cmpd.hist} parameter determines which compounds are drawn like this.
\item Analog to \code{which=1}, but for CSRankScores.
\item Analog to \code{which=2}, but for CSRankScores.
}}

\item{cmpd.hist}{Reference index vector which decides which reference compounds are plotted for the histogram distribution under null hypothesis (\code{which=2}). If \code{NULL}, you can select which compounds you want interactively on the volcano plot.}

\item{color.columns}{Option to color the compounds on the volcano plot (\code{which=1}). Should be a vector of colors with the length of number of references.}

\item{labels}{Boolean value (default=TRUE) to use row and/or column text labels in the volcano plots (\code{which=c(1,3)}).}

\item{plot.type}{How should the plots be outputted? \code{"pdf"} to save them in pdf files, \code{device} to draw them in a graphics device (default), \code{sweave} to use them in a sweave or knitr file.}

\item{basefilename}{Directory including filename of the graphs if saved in pdf files}

\item{MultiCores}{Logical value parallelisation should be used for permutation. \code{FALSE} by default. (This option uses \code{\link[snowFT]{clusterApplyFT}} in order to provide load balancing and reproducible results with \code{MultiCores.seed})}

\item{MultiCores.number}{Number of cores to be used for \code{MultiCores=TRUE}. By default total number of physical cores.}

\item{MultiCores.seed}{Seed to be used for \code{MultiCores=TRUE} using see (\code{\link[snowFT]{clusterSetupRNG.FT}})}

\item{save.permutation}{Logical value if the scores (CLoadings, CRankingScores, ZG Scores) of each permuted data set should be saved (default=\code{TRUE}). 
This information is necessary to recalculate the p-values for different components as well as for producing the histograms. However for larger data, disabling this option will reduce the size of the resulting \code{\link{CSresult-class}} object.}
}
\value{
Returns the same \code{\link{CSresult-class}} object with added p-values to the CS slot and added information to the permutation.object slot. This CSresult can be reused in CSpermute to redraw the plots without calculation.
}
\description{
Apply permutation on MFA or Zhang results to obtain p-values of 1 of the components. 
The function asks for a CSresult object which is returned by CSanalysis. The CSpermute function will return the same CSresult object with added information such as p-values.
If asked, the CSpermute function will also draw a volcanoplot and/or histograms of the p-values. If you simply want to redraw these plots, simply use the returned CSresult object by CSpermute again in the CSpermute function.
If the number of permutations was not changed, this will prevent the entire permutation analysis from being redone.
}
\details{
\bold{IMPORTANT!} For MFA, \code{CSpermute} should \emph{only} be used to compute the p-values of the Component in which the structure (loadings) of the queries is the strongest.
This because in each permutation the factor with the highest average query loadings will be chosen. 
The ability to compute p-values of other factors (in which the query set also increased loadings) will be added in a later release.
}
\examples{
\dontshow{
data("dataSIM",package="CSFA")
Mat1 <- dataSIM[1:250,c(1:6)]
Mat2 <- dataSIM[1:250,-c(1:6)]

ZHANG_analysis <- CSanalysis(Mat1,Mat2,"CSzhang",which=c())
CSpermute(Mat1,Mat2,ZHANG_analysis,B=5,verbose=FALSE)
}
\donttest{
data("dataSIM",package="CSFA")
Mat1 <- dataSIM[,c(1:6)]
Mat2 <- dataSIM[,-c(1:6)]

MFA_analysis <- CSanalysis(Mat1,Mat2,"CSmfa")
MFA_analysis <- CSpermute(Mat1,Mat2,MFA_analysis,B=200)
}
}
