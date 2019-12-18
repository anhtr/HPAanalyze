temp <- tempfile("query", fileext=c(".tsv.gz"))
download.file(url="https://www.proteinatlas.org/search?format=tsv", destfile = temp, method = "curl", mode = "wb")
query_df <- readr::read_tsv(temp)
unlink(temp)

#lookup_df <- dplyr::select(query_df, "gene" = Gene, "ensembl" = Ensembl)
lookup_df <- setNames(query_df[,c("Gene", "Ensembl")], c("gene", "ensembl"))

usethis::use_data(lookup_df,  internal = TRUE, overwrite = TRUE)

# gene_ensembl_convert <- function(id, convert_to) {
#     id_c <- id
#     warn <- FALSE
#     
#     for (i in seq_along(id)) {
#         if (convert_to == "ensembl") {
#             if (substr(id[i], 1, 4) != "ENSG") {
#                 id_c[i] <- lookup_df$ensembl[lookup_df$gene == id[i]][1]
#                 if (is.na(id_c[i])) {
#                     id_c[i] <- id[i]
#                     warn <- TRUE
#                 }
#             } else {
#                 id_c[i] <- id[i]
#             }
#         } else if (convert_to == "gene") {
#             if (substr(id[i], 1, 4) == "ENSG") {
#                 id_c[i] <- lookup_df$gene[lookup_df$ensembl == id[i]][1]
#                 if (is.na(id_c[i])) {
#                     id_c[i] <- id[i]
#                     warn <- TRUE
#                 }
#             } else {
#                 id_c[i] <- id[i]
#             }
#         }
#     }
#     
#     if (warn == TRUE)
#         message(
#             "Couldn't find all requested genes in lookup table. There may be typo(s) or your gene(s) may not currently be supported by HPA."
#         )
#     
#     return(id_c)
# }
# 
# 
# ids <- c("GCH1", "ENSG00000180155", "BLAHBLAH", "ENSG00000284505XYZ")
# 
# gene_ensembl_convert(ids, "gene")
