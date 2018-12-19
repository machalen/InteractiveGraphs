\name{Volcano.scatter}
\alias{Volcano.scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive Volcano plots
}
\description{
This function allows to make interactive plots comparing log10(p-val) in the Y axis with the FC in the x axis.
}
\usage{
Volcano.scatter(num_mat, pval_col, FC_col, pval_th, FC_th, mainName, filePath = NULL, PNG = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num_mat}{
Matrix or data.frame with rownames = GeneID and with columns corresponding to p-val and FC
}
  \item{pval_col}{
Name of the column in the matrix with p-value scores
}
  \item{FC_col}{
Name of the column in the matrix with FC scores
}
  \item{pval_th}{
Name of the p-val threshold to colour the points
}
  \item{FC_th}{
Name of the FC threshold to colour the points
}
  \item{mainName}{
Name of the output file if PNG=TRUE
}
  \item{filePath}{
File path to store the output file if PNG=TRUE
}
  \item{PNG}{
If PNG=TRUE a the plot is also stored in a static format as a .png
}
}
\details{
This function is used to make volcano plots in order to compare p-Val and FC scores. The package "plotly" is required, and there is an option to save the plot in a static file as a .png. For the interactive plot visualization Rstudio is recommended.
}
\value{
A interactive version of the plot is shown in Rstudio. When PNG=TRUE the static plot is created in the filePath.
}
\author{
Magdalena Arnal Segura <marnal@imim.es>
}
