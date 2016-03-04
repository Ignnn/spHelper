#' @title [!!!] Plot a crosstabulation (classification table)
#'
#' @description Plot a crosstabulation (classification table)
#'
#' @param tabl A classification table, crosstabulation: either an object of
#'             a class "table" or a square matrix.
#' @template labels
#' @template subtitle
#' @param bgcolor A background color.
#' @param text.size The size of a text inside cells.
#' @param decimals The number of decimal positions in rounding. Default is 2
#'        (i.e., precission is 0.01).
#'
#' @template ggplot
#' @examples
#'
#' # Generate data: Random guess  ============================
#'  N <- 1000 # number of observations
#'
#' Prediction <- sample(c("A","B","C","D"), N, replace = TRUE)
#' Reference  <- sample(c("A", "B","C","D"),N, replace = TRUE)
#'
#' tabl <- table(Prediction,Reference)
#' qplot_crosstab(tabl)
#'
#' qplot_crosstab(prop.table(table(Prediction,Reference)))
#'
#' @export
#' @family \pkg{spHelper} plots
#' @author Vilmantas Gegzna
qplot_crosstab <- function(tabl,
                            Title  = "Cross-tabulation",
                            xLabel = NULL,
                            yLabel = NULL,
                            subTitle = NULL,
                            text.size = 5,
                            bgcolor = "wheat2",
                            decimals = 2) {
    if (!is.table(tabl)) {       tabl <- as.table(tabl)    }

    # Preserve names of dimensions
    tabl.a   <- round(tabl,decimals)
    tabl.a   <- as.table(tabl.a)
    names(dimnames(tabl.a)) <- names(dimnames(tabl))

    # Make a long format data frame *****************************************
    tabl.m <- reshape2::melt(tabl.a)

    # # Sort levels to plot data correctly ************************************
    tabl.m[[1]] <- factor(tabl.m[[1]], levels = rev(levels(tabl.m[[1]])))
    tabl.m[[2]] <- factor(tabl.m[[2]], levels =     levels(tabl.m[[2]]))

    # # Determine COLORS ******************************************************
    ColValue <- rep(NA, length(tabl))
    ColValue <- as.factor(tabl.m$value != 0)
    tabl.m$ColValue <- ColValue
    # *************************************************************

    # Plot
    # *************************************************************
    nameX <- names(tabl.m)[2]
    nameY <- names(tabl.m)[1]

    p <- ggplot(tabl.m, aes_string(x = nameX, y = nameY)) +
        geom_tile(fill = bgcolor, colour = "grey50") +
        geom_text(aes(label = value, colour = ColValue), size = text.size) +

            scale_color_manual(values = c("FALSE" = "grey60","TRUE" = "black"),
                             guide = FALSE
        ) +

        labs(title = subt(Title, subTitle) ,
             x = {if (is.null(xLabel)) nameX else xLabel},
             y = {if (is.null(yLabel)) nameY else yLabel}
        )

    # p <- cowplot::ggdraw(cowplot::switch_axis_position(p, axis = 'x'))

    return(p)
}


