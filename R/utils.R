
#' @export
dplyr::`%>%`

# Gets the column indices in a data.table of the specified column names
get_col_index <- function(.data, colnames) {
  return(which(colnames(.data) %in% colnames))
}