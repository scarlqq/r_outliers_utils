#' A function that plot out the given data with outliers marked.
#'
#' @param dataframe A target dataframe where the function is performed.
#' @param columns The target columns where the function needed to be performed. Default is None, the function will check all columns.
#' @param type The method of plotting the distribution.
#'
#' @return A ggplot of data distribution.
#' @export
#'
#' @examples
#' df <- tibble(cola=c(1:5), colb=c(6:10), colc=c(11:15))
#' visualize_outliers(df, columns=c("cola", "colb"), type="violin")

library(tidyverse)
library(ggplot2)

visualize_outliers <- function(dataframe, columns=NA, type='violin') {

  ## Handle dataframe type error (Check if dataframe is of type Pandas DataFrame)
  if (!is.data.frame(dataframe) & !is_tibble(dataframe)) {
    stop("Passed dataframe should be DataFrame or tibble.")
  }

  ## Handle empty dataframe or dataframe with all NAN
  if (nrow(dataframe) == 0) {
    stop("Passed dataframe is None.")
  }

  ## Handle columns type error (Check if columns are None or type character)
  if (!is.na(columns) & !is.character(columns)) {
    stop("Passed columns should be list or NoneType.")
  }

  ## Handle type Value error (Check if type has value 'violin' or 'boxplot')
  if (type != 'violin' & type != 'boxplot') {
    stop("Passed type should have value 'violin' or 'boxplot'.")
  }

  ## Check columns
  if (is.na(columns)){
    columns <- c(colnames(df))
  }

  ## Pivot dataframe
  dataframe <- dataframe |>
    select(columns) |>
    pivot_longer(cols=columns)

  ## Check type and plot
  if (type == 'violin') {
    p <- dataframe |>
      ggplot(aes(name, value)) +
      geom_violin(aes(fill = name)) +
      facet_wrap(facets = ~name, ncol = 4) +
      labs(y = "Value", x = "Variables") +
      ggtitle("Data distribution") +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none")
  } else if (type == 'boxplot') {
    p <- dataframe |>
      ggplot(aes(name, value)) +
      geom_boxplot(aes(fill = name)) +
      facet_wrap(facets = ~name, ncol = 4) +
      labs(y = "Value", x = "Variables") +
      ggtitle("Data distribution") +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none")
  }

  ## Return plot
  p

}
