##########################
## Search HPA database ##
##########################

#' Search the Human Protein Atlas
#'
#' Query the Human Protein Atlas search API directly from R, equivalent to
#' searching at \url{https://www.proteinatlas.org/search} and downloading the
#' resulting tsv file. See
#' \url{https://www.proteinatlas.org/about/help/dataaccess} for the full
#' query syntax and list of available column codes.
#'
#' @param search A string with the search term(s) or query, using the same
#'   syntax as \url{https://www.proteinatlas.org/search}. This can be a
#'   simple term (e.g. \code{'TP53'}) or an advanced field query (e.g.
#'   \code{'protein_class:CD markers AND normal_expression:Cerebral
#'   cortex;Any;Not detected,Low'}).
#' @param columns A vector or comma-separated string of column codes to
#'   retrieve, e.g. \code{c('g', 'gs', 'eg')} or \code{'g,gs,eg'}. See
#'   \url{https://www.proteinatlas.org/about/help/dataaccess} for the full
#'   list of available codes. Defaults to gene name, gene synonym, and
#'   Ensembl id.
#' @param exact Logical. The HPA search itself always matches on partial and
#'   related terms (e.g. searching \code{'TP53'} also returns \code{'TP53BP1'}
#'   and other related genes). If \code{TRUE}, the result is filtered down,
#'   after retrieval, to rows whose \code{Gene} column is an exact match for
#'   \code{search}. Requires \code{'g'} (gene name) to be included in
#'   \code{columns}, since that's what the filtering is done on. Only
#'   meaningful when \code{search} is a single gene symbol, not an advanced
#'   field query.
#'
#' @return A tibble with one row per matching protein/gene and one column
#'   per requested field, using the HPA's own column names (e.g.
#'   \code{Gene}, \code{Ensembl}).
#'
#' @family search functions
#'
#' @seealso \code{\link{hpaSubset}} \code{\link{hpaXmlGet}}
#'
#' @examples
#'   hpaSearch(search = 'TP53', columns = c('g', 'gs', 'eg'))
#'   hpaSearch(search = 'TP53', columns = c('g', 'gs', 'eg'), exact = TRUE)
#'
#' @importFrom utils download.file read.delim
#' @importFrom tibble as_tibble
#' @export

hpaSearch <- function(search,
                      columns = c('g', 'gs', 'eg'),
                      exact = FALSE) {

    columns <- .normalize_columns(columns)

    if (exact && !("g" %in% columns)) {
        stop("exact = TRUE requires 'g' (gene name) to be included in columns.")
    }

    url <- .build_search_url(search = search, columns = columns)

    .set_download_timeout(10000)

    temp <- tempfile(fileext = ".tsv")
    on.exit(unlink(temp), add = TRUE)
    download.file(url = url, destfile = temp, mode = "wb")
    dat <- read.delim(
        temp,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        strip.white = TRUE,
        na.strings = c("", " ")
    )

    dat <- as_tibble(dat)

    if (exact) {
        dat <- dat[dat$Gene == search, ]
    }

    return(dat)
}
