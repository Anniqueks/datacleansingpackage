#' Summary of Missing Values
#'
#' This function provides a summary of missing values in each column of a data frame.
#' @param df A data frame.
#' @return A data frame with columns for the variable name, number of missing values, and percentage of missing values.
#' @examples
#' # Create a sample data frame
#' sample_df <- data.frame(
#'   A = c(1, 2, NA, 4),
#'   B = c(NA, 2, 3, 4),
#'   C = c(1, NA, NA, 4)
#' )
#' missing_summary(sample_df)
#'
#' @export
missing_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  missing_data <- sapply(df, function(x) sum(is.na(x)))
  missing_percentage <- missing_data / nrow(df) * 100
  result <- data.frame(
    Variable = names(missing_data),
    MissingValues = missing_data,
    Percentage = missing_percentage
  )
  return(result)
}

