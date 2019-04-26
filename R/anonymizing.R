
#' Anonymize some information in the dataframe.
#'
#' @param df The name of the file includes a dataframe.
#' @param cols_to_anon The columns to anonymize. The default is people's names
#' @param algo The algorithms to be used. The available choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64, and murmur32.
#' @importFrom digest digest
#' @export
#' @return Returns a dataframe with an anonymized column named 'id'.
#' @examples
#' df <- anonymize(df=sals18, cols_to_anon = "place_of_residence")

anonymize <- function(df, cols_to_anon = "name", algo = "crc32"){

  assertthat::not_empty(df)
  assertthat::see_if(cols_to_anon %in% names(df), msg = "The selected column isn't in the dataframe")
  assertthat::assert_that(is.data.frame(df),
                          is.character(algo),
                          nrow(df) > 0)

  to_anon <- dplyr::select(df, cols_to_anon)

  ids <- unname(apply(to_anon, 1, digest::digest, algo = algo))

  df2 <- df %>%
    dplyr::mutate(id = ids)

  return(df2)
}

