\name{SameScale.scatter}
\alias{SameScale.scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive scatterplot comparing FC between different statistic results
}
\description{
This function allows to plot values of FC using a cut-off in order to see which genes are positive correlated, negative correlated or inverse correlated.
}
\usage{
SameScale.scatter(num_mat1, num_mat2, col1.cutoff, col2.cutoff, FC.Col1, FC.Col2, cutoff, mainName, filePath = NULL, PNG = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num_mat1}{
First data matrix to compare with rownames = GeneID and values of FC and p.Val
}
  \item{num_mat2}{
Second data matrix to compare with rownames = GeneID and values of FC and p.Val
}
  \item{col1.cutoff}{
Column name in the first matrix used to apply the cut-off
}
  \item{col2.cutoff}{
Column name in the second matrix used to apply the cut-off
}
  \item{FC.Col1}{
Column name in the first matrix containing the values of FC
}
  \item{FC.Col2}{
Column name in the second matrix containing the values of FC
}
  \item{cutoff}{
Cut-off used to plot the genes
}
  \item{mainName}{
Name of the output
}
  \item{filePath}{
File path for the results. Used only if PNG=TRUE
}
  \item{PNG}{
If PNG=TRUE a .png file is created
}
}
\details{
This function is used to compare two results with different FC and p-value scores. It uses a cut-off parameter and plots the intersection of the genes in both matrices using the FC values (X and Y axis). In the interactive version the name of each data point can also be accessed. The package plotly is required in order to make the interctive graphs.
}
\value{
A interactive version of the plot is shown in Rstudio. When PNG=TRUE the static plot is created in the filePath.
}

\author{
Magdalena Arnal Segura <marnal@imim.es>
}
