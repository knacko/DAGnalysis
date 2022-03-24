
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

#' Gets the n (%) for missing values#'
#' @param mids The imputed dataset
#' @param count.char The character to look for. Default = NA
#' @return data.frame
#' @export
#' @examples
getNAstats <- function(mids, count.char = NA) {
  ### Getting missing values table
  tbl <- sort(col_count(mids,count=count.char,append=FALSE))             ### By col
  tbl <- t(tbl)
  tbl <- apply(tbl, 2, rev)
  tbl[,1] <- paste0(tbl," (",round(tbl/nrow(mids)*100, 1),")")
  tbl <- cbind.data.frame("Risk Factor" = row.names(tbl), "n (%)" = tbl[,1])
  
  total <- length(which(row_count(mids,count=count.char,append=FALSE)>0))
  
  tbl <- rbind(tbl,c("Cases with >= 1 incomplete",paste0(total," (",round(total/nrow(mids)*100, 1),")")))
  
  row.names(tbl) <- NULL
  
  return (tbl)
}

#' Plots the convergence data
#' @details 
#' @param mids The imputed dataset
#' @param cols Which columns to show. Default is all columns that were imputed.
#' @param min_imp Cut-off of minimum number of imputed values. 
#' @param m The number of datasets to show on the graph. Recommended value < 10 for visibility 
#' @param maxit Iterations for each dataset. Recommended value > 20 imputations
#' @return The imputed data
#' @examples
#' @export
checkConvergence <- function(mids, cols = NULL, minNA = 0, m = 10, maxit = 20, ...) {
  
  if (m > mids$m) {
    m <- mids$m
    warning("m exceeds the number of imputed data sets. Defaulting to ",m)
  }
  
  if (maxit > mids$iteration) {
    maxit <- mids$iteration
    warning("maxit exceeds the number of iterations in data. Defaulting to ",maxit)
  }
  
  if (is.null(cols)) {
    cols <- names(which(lapply(mids$imp,nrow) != 0))
  } 
  
  cols <- intersect(cols, names(which(lapply(mids$imp,nrow) >= minNA))) #Get common elements
  
  plot.mids2(mids,y=cols,layout=c(3,ceiling(length(cols)/3)))
}

#' Imputes the missing elements of a matrix
#' @details 
#' @param mids The imputed dataset
#' @param cols Which columns to show. Default is all columns that were imputed.
#' @param m The number of datasets to display
#' @return plot showing distribution of the original and imputed data
#' @import mice grid
#' @examples
#' @export
checkDistribution <- function(mids, cols = NULL, m = 10, minNA = 0, maxit = 20,  ...) {

  #Same boilerplate as above except for maxit
  if (m > length(mids)) {
    m <- length(mids)
    warning("m exceeds the number of imputed data sets. Defaulting to ",m)
  }
  
  if (is.null(cols)) {
    cols <- names(which(lapply(mids$imp,nrow) != 0))
  }
  
  if (minNA < 2) {
    minNA <- 2
    warning("minNA must be >= 2.")
  }
  
  cols <- intersect(cols, names(which(lapply(mids$imp,nrow) >= minNA))) #Get common elements

  densityplot(mids,as.formula(paste("~",paste(cols,collapse=" + "))),layout=c(3,ceiling(length(cols)/3)))
  
}

#' Plots an imputation distribution of a mice::mids object, but without the SD plot
#' @inheritParams mice::plot.mids 
#' @return An object of class \code{"trellis"}.
plot.mids2 <- function(x, y = NULL, theme = mice.theme(), layout = c(2, 3),
                      type = "l", col = 1:10, lty = 1, ...) {
  strip.combined <- function(which.given, which.panel, factor.levels, ...) {
    if (which.given == 1) {
      lattice::panel.rect(0, 0, 1, 1,
                          col = theme$strip.background$col, border = 1
      )
      lattice::panel.text(
        x = 0, y = 0.5, pos = 4,
        lab = factor.levels[which.panel[which.given]]
      )
    }
    if (which.given == 2) {
      lattice::panel.text(
        x = 1, y = 0.5, pos = 2,
        lab = factor.levels[which.panel[which.given]]
      )
    }
  }
  
  call <- match.call()
  if (!is.mids(x)) {
    stop("argument 'x' must be a 'mids' object", call. = FALSE)
  }
  if (is.null(x$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }
  
  mn <- x$chainMean
  sm <- sqrt(x$chainVar)
  
  # select subset of nonmissing entries
  obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
  varlist <- names(obs)[obs]
  
  ## create formula if not given in y
  if (missing(y)) {
    formula <- as.formula(paste0(
      paste0(varlist, collapse = "+"),
      "~.it|.ms"
    ))
  } else {
    formula <- NULL
    if (is.null(y)) {
      formula <- as.formula(paste0(
        paste0(varlist, collapse = "+"),
        "~.it|.ms"
      ))
    }
    if (is.character(y)) {
      formula <- if (length(y) == 1) {
        as.formula(paste0(y, "~.it|.ms"))
      } else {
        as.formula(paste0(paste0(y, collapse = "+"), "~.it|.ms"))
      }
    }
    if (is.integer(y) || is.logical(y)) {
      vars <- varlist[y]
      formula <- if (length(vars) == 1) {
        as.formula(paste0(vars, "~.it|.ms"))
      } else {
        as.formula(paste0(paste0(vars, collapse = "+"), "~.it|.ms"))
      }
    }
    if (is.null(formula)) {
      formula <- as.formula(y)
    }
  }
  
  m <- x$m
  it <- x$iteration
  mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
  sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
  
  adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
  data <- cbind(adm, rbind(mn, sm))
  colnames(data) <- c(".it", ".m", ".ms", varlist)
  ## Dummy to trick R CMD check
  .m <- NULL
  rm(.m)
  
  data <- data[data$.ms == "mean",] ### THIS REMOVE THE SD GRAPH
  
  tp <- xyplot(
    x = formula, data = data, groups = .m,
    type = type, lty = lty, col = col, layout = layout,
    scales = list(
      y = list(relation = "free"),
      x = list(alternating = FALSE)
    ),
    as.table = TRUE,
    xlab = "Iteration",
    ylab = "",
    strip = strip.combined,
    par.strip.text = list(lines = 0.5),
    ...
  )
  update(tp, par.settings = theme)
}