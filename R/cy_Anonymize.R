#' @name cy_Anonymize
#' @title Anonymize Names
#' @description Anonymize column of your choice to alpha-numeric code (uses digest package)
#' @param df The dataframe you wish to anonymize
#' @param col_to_anon The column to anonymize. The default 'name'.
#' @param algo The algorithms to be used. The available choices are md5 (default), sha1, crc32, sha256, sha512, xxhash32, xxhash64, and murmur32. See the digest R package documentation for details.
#' @importFrom digest digest
#' @export
#' @return Returns the dataframe with a new anonymized column named 'id'.
#' @examples
#' rawdf <- data.frame(name = c("John", "Jon", "Jonathan", "Jon"),
#'       year = c(2010,2010,2011,2011), pay = c(5000,7000,8000,7000))
#' cy_Anonymize(rawdf, "name")
#' anondf <- cy_Anonymize(df=sals18, col_to_anon = "place_of_residence")


cy_Anonymize <- function(df, col_to_anon = "name", algo = "crc32"){

  assertthat::not_empty(df)
  assertthat::see_if(col_to_anon %in% names(df), msg = "The selected column isn't in the dataframe")
  assertthat::assert_that(is.data.frame(df),
                          is.character(algo),
                          nrow(df) > 0)

  to_anon <- dplyr::select(df, col_to_anon)

  ids <- unname(apply(to_anon, 1, digest::digest, algo = algo))

  df2 <- df %>%
    dplyr::mutate(id = ids)

  return(df2)
}

