
#' Anonymize some information in the dataframe.
#'
#' @param df The name of the file includes a dataframe.
#' @param cols_to_anon The columns to anonymize. The default is people's names
#' @param algo The algorithms to be used. The available choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64, and murmur32.
#' @export
#' @return Returns a dataframe with an anonymized column (id).
#' @examples
#' DF <- sal_df()
#' anonymize(DF)

load("~/CyChecks/data/sals_dept.rda")
df <- sals_dept

anonymize <- function(df, cols_to_anon = "name", algo = "crc32"){
  #if(!require(digest)) stop("digest package is required")
  assertthat::see_if(is.character(cols_to_anon), msg = "The selected columns are not character!")
  assertthat::see_if(cols_to_anon %in% names(df), msg = "The selected column isn't in the dataframe")
  assertthat::assert_that(is.data.frame(df), msg = "df is not a dataframe!")
  assertthat::assert_that(is.character(algo), msg = "algo is not string")
  to_anon <- dplyr::select(df, cols_to_anon)
  ids <- unname(apply(to_anon, 1, digest, algo = algo))
  df2 <- df %>%
    dplyr::mutate(id = ids)
  return(df2)
}

