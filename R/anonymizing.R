
#' Anonymize some information in the dataframe.
#'
#' @param df The name of the file includes a dataframe.
#' @param algo The algorithms to be used. The available choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64, and murmur32.
#' @export
#' @return Returns the dataframe.
#' @examples
#' # Anonymize(data, cols_to_mask = "V")
#'

#df <- load(file = data/all_sals.rda)
df_Cysalary <- all_sals

anonymize <- function(df_Cysalary, algo="crc32"){
  library(data.table)
  library(digest)
  library(assertthat)

  unq_hashes <- vapply(unique(df_Cysalary), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hashes[df_Cysalary])

  assertthat::assert_that(is.string(algo), msg = "The type of algorithm is not string!")
}


#### anonymizing

cols_to_mask <- c("name")
assertthat::see_if(is.character(cols_to_mask), msg = "The selected columns are not character!")

setDT(df_Cysalary)
assertthat::see_if(is.data.table (df_Cysalary), msg = "The dataframe is not converted to a table!")

df_Cysalary_org <- copy(df_Cysalary)
df_Cysalary <- df_Cysalary[,cols_to_mask := lapply(.SD, anonymize),.SDcols=cols_to_mask,with=FALSE]

head(df_Cysalary, 5)
