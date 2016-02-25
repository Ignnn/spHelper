# ***** Plot a confusion matrix ***** --------------------------------------
#
#' [!] Vizualize a confusion matrix / classification table
#'
#' \code{plot_confusion}
#'
#' @param conf A confusion matrix / A classificattion table (either a table
#'        or a square matrix).
#' @template labels
#' @param subTitle Subtitle - second line of a title.
#' @param shades A way, how cells are shaded.
#'      \describe{
#'          \item{prop}{(default) A shade (intensity) of a color is proportianal
#'                to a value of a cell. Distribution of colors is balanced
#'                according to number of classes, but not balanced according to
#'                number of observations per class.}
#'          \item{max}{Cell with absolute maximum value is represented by the
#'          most intensive color. Other cells are represented by colors which
#'          are diluted proportionaly to their value in accordance with the
#'          maximum value.}
#'          \item{const}{Constant red and green colors.}
#'          \item{none}{All cells are grey.}
#'      }
#'
#' @return A plot of confusion matrix.
#' @examples
#'
#' N <- 1000 # number of observations
#'
#' Prediction <- sample(c("A","B","C","D"),N, replace = TRUE)
#' Reference  <- sample(c("A", "B","C","D"),N, replace = TRUE)
#'
#'
#' # Random guess  =====================
#' conf <- table(Prediction,Reference)
#'
#' plot_confusion(conf)
#'
#' # At least 40% of the cases agree =====================
#' ind <- sample(1:N,round(0.5*N))
#' Reference[ind] <- Prediction[ind]
#' conf2 <- table(Prediction,Reference)
#'
#' plot_confusion(conf2)
#'
#' # Most of the cases agree =============================
#' ind <- sample(1:N,round(N*.8))
#' Reference[ind] <- Prediction[ind]
#' conf3 <- table(Prediction,Reference)
#'
#' plot_confusion(conf3)
#'
#' # Proportions =========================================
#'
#' plot_confusion(conf3)
#' plot_confusion(prop.table(conf3))
#' plot_confusion(prop.table(conf3,1))
#' plot_confusion(prop.table(conf3,2))
#'
#' # Shades: proportional ================================
#'
#' plot_confusion(conf,shades = "prop",  subTitle = "shades: 'prop', correct by chance")
#' plot_confusion(conf,shades = "max",   subTitle = "shades: 'max', correct by chance")
#'
#' plot_confusion(conf2,shades = "prop", subTitle = "shades: 'prop', correct >50%")
#' plot_confusion(conf2,shades = "max",  subTitle = "shades: 'max', correct >50%")
#'
#' plot_confusion(conf3,shades = "prop", subTitle = "shades: 'prop', correct >80%")
#' plot_confusion(conf3,shades = "max",  subTitle = "shades: 'max', correct >80%")
#'
#' # Shades: constant and none ===========================
#'
#' plot_confusion(conf3,shades = "const",subTitle = "shades: constant")
#' plot_confusion(conf3,shades = "none", subTitle = "shades: none")
#'
#'
#' @export
#' @family spHelper plots
#' @import ggplot2


plot_confusion <- function(conf,
                          Title  = "Classification table",
                          xLabel = "Reference group",
                          yLabel = "Predicted group",
                          subTitle = NULL,
                          shades = "prop") {
    if (!is.table(conf)) conf <- as.table(conf)

    conf <- round(conf,2)

    conf.m <- reshape2::melt(conf)
    names(conf.m)[1:2] <- c("Actual","Predicted")
    conf.m$Predicted <- factor(conf.m$Predicted, levels = rev(levels(conf.m$Predicted)))

    # Let's color the diagonal cells ****************************************
    nRows <- nrow(conf.m)
    nCols <- length(unique(conf.m[,2]))
    indx  <- seq(1,nRows,nCols + 1)

    switch(shades,
           prop =  {
               n <- nrow(conf);
               conf.m$ColValue       <- conf.m$value / sum(conf) * n * (n - 1);
               conf.m$ColValue[indx] <- -1/ (n - 1) * conf.m$ColValue[indx]
            },

           max = {
               conf.m$ColValue       <- conf.m$value / max(conf);
               conf.m$ColValue[indx] <- -1 * conf.m$ColValue[indx]
            },

           const = {
               conf.m$ColValue      <-  .8;
               conf.m$ColValue[indx] <- -.9
            },
        # Just constant grey color (no red nor green colors)
           conf.m$ColValue <- 0
    )
    conf.m$ColValue[conf.m$ColValue < -1] <- -1
    conf.m$ColValue[conf.m$ColValue >  1] <-  1

    # *************************************************************
    p <- ggplot(conf.m, aes(Actual, Predicted)) +
         scale_fill_gradient2(high = "#cd0000",
                              mid  = "#eeeeee", #mid = "#f2f6c3",
                              midpoint = 0,
                              low  = "#008000",

                              guide = FALSE,
                              name = " ",
                              limits = c(-1,1),
                              breaks = c(1,0,-1),
                              labels = c("Incorrectly identified", "0", "Correctly identified")) +
         geom_tile(aes(fill = ColValue), colour = "white") +
         geom_text(aes(label = value), size = 6) +

         labs(title = subt(Title, subTitle),
             x = xLabel,
             y = yLabel)

    return(p)
}
