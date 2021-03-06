% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_structure.R
\docType{class}
\name{CSresult-class}
\alias{CSresult-class}
\title{An S4 class in which the results of the Connectivity Scores by Factor Analysis are stored.}
\description{
An S4 class in which the results of the Connectivity Scores by Factor Analysis are stored.
}
\section{Slots}{

\describe{
\item{\code{type}}{A character string containing the analysis type.}

\item{\code{CS}}{List of any number of lists (depending on how many components were selected) which contain the connectivity loadings and ranking scores for the reference (and query loadings). If permutation was applied, will also contain p-values.}

\item{\code{GS}}{Dataframe containing the gene scores.}

\item{\code{extra}}{List which contains \code{CSRank_Full} (contains all intermediate values while calculating the CS Ranking Score), \code{Object} (contains the complete original FA or Zhang result) and \code{samplefactorlabels} (contains thresholded labels based on the factor loadings, see plot \code{which=8}).}

\item{\code{permutation.object}}{Contains CS for permuted data (matrix) and a dataframe with the p-values (only for MFA and Zhang).}

\item{\code{call}}{List object containing the original call of \code{CSanalysis} as well as the parameters for the chosen method.}
}}

