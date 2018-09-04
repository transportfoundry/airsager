#' Balance matrix
#'
#' This function takes an input matrix in origin/destination form and balances
#' it so that it is symmetric according to the following formula
#' \eqn{(A + A^T) / 2 }
#'
#' @param df A matrix in \code{data_frame} format with a column for origin and a
#'   column for destination.
#' @param from A character string identifying the origin column, defaults to
#'   \code{origin}
#' @param to A character string identifying the destination column, defaults
#'   to \code{destination}
#'
#' @return A \code{data_frame} of the same format as \code{df}, with the matrix
#'   cores balanced.
#' @importFrom magrittr '%>%'
#' @importFrom dplyr left_join
#' @importFrom stats setNames
#'
#' @author Kyle Ward
#'
#'
#' @examples
#' balance_matrix(test_oddf)
#'
#' @export
#'
balance_matrix <- function (df, from = "origin", to = "destination") {
  # Determine the other column names in the data frame (not from and to)
  col_names <- colnames(df)
  col_names <- col_names[!col_names %in% c(from, to)]
  
  # Create the transpose matrix by simply switching the from and to columns
  # Add "_t" to the other column names so the join doesn't append ".x" and ".y"
  # to them.
  temp <- df[, from]
  df_t <- df
  df_t[, from] <- df_t[, to]
  df_t[, to] <- temp
  colnames(df_t) <- c(from, to, paste0(col_names, "_t"))
  
  # Joining the two dataframes
  both <- df %>%
    dplyr::left_join(df_t, by = stats::setNames(c(from, to), c(from, to))) %>%
    dplyr::mutate_all(funs(ifelse(is.na(.), 0, .)))
  
  # For each column other than from and to, add together and divide by 2
  # Afterwards, remove the transposed column
  for (c in col_names) {
    both[, c] <- (both[, c] + both[, paste0(c, "_t")]) / 2
    both[, paste0(c, "_t")] <- NULL
  }
  
  return(both)
}