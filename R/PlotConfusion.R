# ***** Plot confusion matrix ***** --------------------------------------
#
#' Plot a confusion matrix, \code{PlotConfusion}
#'
#' @param conf - a table, a square matrix.
#' @param Title ... /not implemented yet/ Title.
#' @param subTitle ... /not implemented yet/ Subtitle.
#'
#'
#' @return Plot of confusion matrix. ...
#' @examples
#' Prediction <- sample(c("A","B","C"),1000, replace = T)
#' Reference  <- sample(c("A","B","C"),1000, replace = T)
#' conf <- table(Prediction,Reference)
#'
#' PlotConfusion(conf)
#' PlotConfusion(prop.table(conf,1))
#' PlotConfusion(prop.table(conf,2))
#'
#' @export
#'
#' @import ggplot2
#'
PlotConfusion <- function(conf) {
    conf <- round(conf,2)

    conf.m <- reshape2::melt(conf)
    names(conf.m)[1:2] <- c("Actual","Predicted")
    conf.m$Actual <- factor(conf.m$Actual, levels = rev(levels(conf.m$Actual)))

    # Spalvinam įstrižainę ****************************************
    nRows <- nrow(conf.m)
    nCols <- length(unique(conf.m[,2]))
    ind   <- seq(1,nRows,nCols + 1)

    #     conf.m$ColValue      <- conf.m$value*(-1)
    #     conf.m$ColValue[ind] <- conf.m$ColValue[ind]*(-1)

    conf.m$ColValue      <- -1
    conf.m$ColValue[ind] <- 1
    # *************************************************************

    p <- ggplot(conf.m, aes(Actual, Predicted)) +
         scale_fill_gradient2(high = "#006400", mid = "#f2f6c3",
                              midpoint = 0,
                              low  = "#cd0000") +
         geom_tile(aes(fill = ColValue), colour = "white") +
         geom_text(aes(label = value), size = 6)

    return(p)
}
