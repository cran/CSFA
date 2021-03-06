% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_structure.R
\docType{methods}
\name{CSanalysis,matrix,matrix,CSsmfa-method}
\alias{CSanalysis,matrix,matrix,CSsmfa-method}
\title{"CSsmfa"}
\usage{
\S4method{CSanalysis}{matrix,matrix,CSsmfa}(querMat, refMat, type = "Csmfa",
  K = 15, para, lambda = 1e-06, sparse.dim = 2, sparse = "penalty",
  max.iter = 200, eps.conv = 0.001, which = c(2, 3, 4, 5),
  component.plot = NULL, CSrank.queryplot = FALSE, column.interest = NULL,
  row.interest = NULL, profile.type = "gene", color.columns = NULL,
  gene.highlight = NULL, gene.thresP = 1, gene.thresN = -1,
  thresP.col = "blue", thresN.col = "red", grouploadings.labels = NULL,
  grouploadings.cutoff = NULL, legend.names = NULL, legend.cols = NULL,
  legend.pos = "topright", labels = TRUE, result.available = NULL,
  result.available.update = FALSE, plot.type = "device",
  basefilename = NULL)
}
\arguments{
\item{querMat}{Query matrix (Rows = genes and columns = compounds)}

\item{refMat}{Reference matrix}

\item{type}{\code{"CSsmfa"}}

\item{K}{\emph{sMFA Parameters:} Number of components.}

\item{para}{\emph{sMFA Parameters:} A vector of length K. All elements should be positive. If \code{sparse="varnum"}, the elements integers.}

\item{lambda}{\emph{sMFA Parameters:} Quadratic penalty parameter. Default value is 1e-6. If the target dimension of the sparsness is higher than the other dimension (p > n), it is advised to put \code{lambda} to \code{Inf} which uses the \code{arrayspc} algorithm optimized for this case. For the other case, p < n, a zero or positive \code{lambda} is sufficient and will utilize the normal \code{spca} algorithm.}

\item{sparse.dim}{\emph{sMFA Parameters:} Which dimension should be sparse? 1: Rows, 2: Columns (default) (Note: For Connectivity Scores it is advised to apply sparsity on the compounds/columns)}

\item{sparse}{\emph{sMFA Parameters (\code{lambda < Inf} only):} If \code{sparse="penalty"}, \code{para} is a vector of 1-norm penalty parameters. If \code{sparse="varnum"}, \code{para} defines the number of sparse loadings to be obtained.}

\item{max.iter}{\emph{sMFA Parameters:} Maximum number of iterations.}

\item{eps.conv}{\emph{sMFA Parameters:} Convergence criterion.}

\item{which}{Choose one or more plots to draw: 
\enumerate{
\item Information Content for Bicluster (Only available for "CSfabia")
\item Loadings for query compounds
\item Loadings for Component (Factor/Bicluster) \code{component.plot}
\item Gene Scores for Component (Factor/Bicluster) \code{component.Plot}
\item Connectivity Ranking Scores for Component \code{component.plot}
\item Component \code{component.plot} VS Other Component : Loadings & Genes 
\item Profile plot (see \code{profile.type})
\item Group Loadings Plots for all components (see \code{grouploadings.labels}).
}}

\item{component.plot}{Which components (Factor/Bicluster) should be investigated? Can be a vector of multiple (e.g. \code{c(1,3,5)}). If \code{NULL}, you can choose components of interest interactively from query loadings plot.}

\item{CSrank.queryplot}{Logical value deciding if the CS Rank Scores (\code{which=5}) should also be plotted per query (instead of only the weighted mean).}

\item{column.interest}{Numeric vector of indices of reference columns which should be in the profiles plots (\code{which=7}). If \code{NULL}, you can interactively select genes on the Compound Loadings plot (\code{which=3}).}

\item{row.interest}{Numeric vector of gene indices to be plotted in gene profiles plot (\code{which=7}, \code{profile.type="gene"}). If \code{NULL}, you can interactively select them in the gene scores plot (\code{which=4}).}

\item{profile.type}{Type of \code{which=7} plot:
\itemize{
\item \code{"gene"}: Gene profiles plot of selected genes in \code{row.interest} with the query compounds and those selected in \code{column.interest} ordered first on the x axis. The other compounds are ordered in decreasing CScore. 
\item \code{"cmpd"}: Compound profiles plot of query and selected compounds (\code{column.interest}) and only those genes on the x-axis which beat the thresholds (\code{gene.thresP}, \code{gene.thresN})
}}

\item{color.columns}{Vector of colors for the query and reference columns (compounds). If \code{NULL}, blue will be used for query and black for reference. Use this option to highlight query columns and reference columns of interest.}

\item{gene.highlight}{Single numeric vector or list of maximum 5 numeric vectors. This highlights gene of interest in gene scores plot (\code{which=4}) up to 5 different colors. (e.g. You can use this to highlight genes you know to be differentially expressed)}

\item{gene.thresP}{Threshold for genes with a high score (\code{which=4}).}

\item{gene.thresN}{Threshold for genes with a low score (\code{which=4}).}

\item{thresP.col}{Color of genes above \code{gene.thresP}.}

\item{thresN.col}{Color of genes below \code{gene.thresN}.}

\item{grouploadings.labels}{This parameter used for the Group Loadings Plots (\code{which=8}). In general this plot will contain the loadings of all factors, grouped and colored by the labels given in this parameter. Two types of plot can be created:
\enumerate{
\item If \code{grouploadings.labels!=NULL}:\cr
Provide a vector for all samples (query + ref) containing labels on which the plot will be based on.

\item If \code{grouploadings.labels=NULL}: \cr
If no labels are provided when choosing \code{which=8}, automatic labels ("Top Samples of Component 1, 2....") will be created. These labels are given to the top \code{grouploadings.cutoff}  number of samples based on the absolute values of the loadings. 
}
Plot \code{which=8} can be used to check 2 different situations. The first plot checks if your provided labels coincide with the discovered structures in the analysis. The second plot aims to find new interesting structures (of samples) which strongly appear in one or multiple components. A subsequent step could be to take some strong samples/compounds of these compounds and use them as a new query set in a new CS analysis to check its validity or to find newly connected compounds.

Please note that even when \code{group.loadings.labels!=NULL}, that the labels based on the absolute loadings of all the factors (the top \code{grouploadings.cutoff}) will always be generated and saved in \code{samplefactorlabels} in the \code{extra} slot of the \code{CSresult} object. 
This can then later be used for the \code{\link{CSlabelscompare}} function to compare them with your true labels.}

\item{grouploadings.cutoff}{Parameter used in plot \code{which=8}. An integer for the number of cut-offs. See \code{grouploadings.labels=NULL} for more information. If this parameter is not provided, it will be automatically set to 10\% of the total number of loadings.}

\item{legend.names}{Option to draw a legend of for example colored columns in Compound Loadings plot (\code{which=3}). If \code{NULL}, only "References" will be in the legend.}

\item{legend.cols}{Colors to be used in legends. If \code{NULL}, only blue for "Queries is used".}

\item{legend.pos}{Position of the legend in all requested plots, can be \code{"topright"}, \code{"topleft"}, \code{"bottomleft"}, \code{"bottomright"}, \code{"bottom"}, \code{"top"}, \code{"left"}, \code{"right"}, \code{"center"}.}

\item{labels}{Boolean value (default=TRUE) to use row and/or column text labels in the score plots (\code{which=c(3,4,5,6)}).}

\item{result.available}{You can a previously returned object by \code{CSanalysis} in order to only draw graphs, not recompute the scores.}

\item{result.available.update}{Logical value. If \code{TRUE}, the CS and GS will be overwritten depending on the new \code{component.plot} choice. This would also delete the p-values if \code{permutation.object} was available.}

\item{plot.type}{How should the plots be outputted? \code{"pdf"} to save them in pdf files, \code{device} to draw them in a graphics device (default), \code{sweave} to use them in a sweave or knitr file.}

\item{basefilename}{Directory including filename of the graphs if saved in pdf files}
}
\value{
An object of the S4 Class \code{\link{CSresult-class}}.
}
\description{
Doing interactive CS analysis with sMFA (Sparse Multiple Factor Analysis). Should use multiple queries for this analysis.
Either \code{\link[elasticnet]{spca}} or \code{\link[elasticnet]{arrayspc}} is used.
}
