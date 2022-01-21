#' A function that plot out the given data with outliers marked.
#'
#' @param dataframe A target dataframe where the function is performed.
#' @param columns The target columns where the function needed to be performed. Default is None, the function will check all columns.
#' @param type The method of plotting the distribution.
#'
#' @return A ggplot with data distribution.
#' @export
#'
#' @examples
#' df <- tibble(cola=c(1:5), colb=c(6:10), colc=c(11:15))
#' visualize_outliers(df, columns=c("cola", "colb"), type="violin")
visualize_outliers <- function(dataframe, columns=NA, type="violin") {

}
