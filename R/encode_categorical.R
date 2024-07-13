#' Encode Categorical Variables
#'
#' This function encodes categorical variables in a data frame using one-hot encoding.
#' @param df A data frame.
#' @return A data frame with categorical variables encoded.
#' @examples
#' # Create a sample data frame with categorical variables
#' sample_df <- data.frame(
#'   A = factor(c("cat", "dog", "cat", "bird")),
#'   B = c(1, 2, 3, 4)
#' )
#' encode_categorical(sample_df)
#'
#' @importFrom stats model.matrix
#' @export
encode_categorical <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  df <- as.data.frame(lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      model.matrix(~x - 1)
    } else {
      x
    }
  }))
  return(df)
}
