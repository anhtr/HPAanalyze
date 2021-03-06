% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one.R
\name{hpaVis}
\alias{hpaVis}
\title{Visualize data in one function}
\usage{
hpaVis(
  data = NULL,
  targetGene = NULL,
  targetTissue = NULL,
  targetCellType = NULL,
  targetCancer = NULL,
  visType = c("Tissue", "Patho", "Subcell"),
  color = c("#FCFDBF", "#FE9F6D", "#DE4968", "#8C2981"),
  customTheme = FALSE,
  ...
)
}
\arguments{
\item{data}{Input the list object generated by \code{hpa_download()} or
\code{hpa_subset()}. By default this function use the example dataset
bundled with HPAanalyze.}

\item{targetGene}{Vector of strings of HGNC gene symbols. By default it is
set to \code{c('TP53', 'EGFR', 'CD44', 'PTEN', 'IDH1')}. You can also mix
HGNC gene symbols and ensemnl ids (start with ENSG) and they will be
converted to HGNC gene symbols.}

\item{targetTissue}{Vector of strings of normal tissue names. By default it
is set to \code{"breast"}.}

\item{targetCellType}{Vector of strings of normal cell types. By default
inludes all available cell types in the target tissues.}

\item{targetCancer}{Vector of strings of normal tissues. By default it
is set to \code{"breast cancer"}.}

\item{visType}{Vector of strings indicating which plots will be generated.
Currently available values are \code{"all"}, \code{"Tissue"},
\code{"Patho"}, \code{"Cancer"}, \code{"Subcell"}.}

\item{color}{Vector of 4 colors used to depict different expression levels.}

\item{customTheme}{Logical argument. If \code{TRUE}, the function will return
a barebone ggplot2 plot to be customized further.}

\item{...}{Additional arguments to be passed downstream to other hpaVis
functions being called behind the scene. These arguments includes
\code{targetTissue}, \code{targetCellType}, \code{targetCancer}. See
documentation for individual hpaVis functions for more information.}
}
\value{
If multiple visType is chosen, this function will return multiple
  graphs in one panel. If only one visType is chosen, this function will
  return a ggplot2 plot object, which can be further modified if desirable.
  See help file for each of the hpaVis function for more information about
  individual graphs.
}
\description{
This function is an universal visualization function that allow calling other
hpaVis functions via a single function call. By default, this function will
use the dataset bundled with HPAanalyze, and provide a grid of all available
plots. The types of plots in the output can be specified via the
\code{visType} argument. If only one plot type is specified, this function
will return the exact same output as the specific hpaVis function used to
create the plot.
}
\examples{
  hpaVis()

}
\seealso{
\code{\link{hpaDownload}}, \code{\link{hpaSubset}}

Other visualization functions: 
\code{\link{hpaVisPatho}()},
\code{\link{hpaVisSubcell}()},
\code{\link{hpaVisTissue}()}
}
\concept{visualization functions}
