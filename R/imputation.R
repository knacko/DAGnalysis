
#' Imputes the missing elements of a matrix
#' @details 
#' @param data The data to impute
#' @param m The number of datasets to produce. Should use > fraction of incomplete cases * 100 
#' @param maxit Iterations for each dataset. Should use at least 20 to help determine convergence
#' @param seed Seed value for the randomization
#' @param exclude List of columns names to exclude from the imputation
#' @param complete Returns list of complete data.frames instead of imputation object
#' @param show_stats Flag to display the  summary for before and after imputation and plot the pattern
#' @return The imputed data
#' @import mice
#' @examples
#' @export
impute_data <- function(data, m = 10, maxit = 10, seed = 123, exclude = NULL, complete = FALSE, 
                        show_stats = FALSE) {
  
  imputed <- mice(data, m=m, maxit=maxit, seed=seed, 
                  pred=quickpred(data, method="spearman",exclude = exclude))
  
  if (complete == TRUE) {
    imputed <- lapply(1:m, function(i) complete(imputed),i)
  }
  
  if (show_stats) {
    message("Before imputation:")
    summary(data)
    message("\nAfter imputation:")
    summary(imputed)
    md.pattern(imputed,rotate.names=TRUE)
  }
  
  return(imputed)
}

#' Plots the convergence data
#' @details 
#' @param data The data to impute (from impute_data)
#' @param cols Which columns to show. Default is all columns that were imputed.
#' @param min_imp Cut-off of minimum number of imputed values. 
#' @param m The number of datasets to show on the graph. Recommended value < 10 for visibility 
#' @param maxit Iterations for each dataset. Recommended value > 20 imputations
#' @return The imputed data
#' @examples
#' @export
check_convergence <- function(data, cols = NULL, min_imp = 0, m = 10, maxit = 20) {
  
  if (m > length(data)) {
    m <- length(data)
    warning("m exceeds the number of imputed data sets. Defaulting to ",m)
  }
  
  if (maxit > length(data$imp)) {
    maxit <- length(data$imp) 
    warning("maxit exceeds the number of iterations in data. Defaulting to ",maxit)
  }
  
  if (is.null(cols)) {
    cols <- names(which(lapply(data$imp,nrow) != 0))
  } 
  
  cols <- cols && names(which(lapply(data$imp,nrow) >= min_imp)) #Get common elements
  
  plot(data,y=cols)
}

#' Imputes the missing elements of a matrix
#' @details 
#' @param data The data to impute (from impute_data)
#' @param cols Which columns to show. Default is all columns that were imputed.
#' @param m The number of datasets to display. Recommend < 10 for clarity
#' @return plot showing distribution of the original and imputed data
#' @import mice grid
#' @examples
#' @export
check_distribution <- function(data, cols, m = 10, ...) {
  
  #Same boilerplate as above except for maxit
  if (m > length(data)) {
    m <- length(data)
    warning("m exceeds the number of imputed data sets. Defaulting to ",m)
  }
  
  if (is.null(cols)) {
    cols <- names(which(lapply(data$imp,nrow) != 0))
  }
  
  cols <- cols && names(which(lapply(data$imp,nrow) >= min_imp)) #Get common elements
  
  plots <- cols %>% densityplot(data,~.,ylab="")
  gridExtra::grid.arrange(plots,..., left="Density")
  
}