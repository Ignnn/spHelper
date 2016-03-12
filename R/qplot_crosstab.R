#' @title [!] Plot a crosstabulation (classification table)
#'
#' @description Plot a crosstabulation (classification table)
#'
#' @param tabl A classification table, crosstabulation: either an object of
#'             a class "table" or a square matrix.
#' @template labels
#' @template subtitle
#' @param sort.maxOnDiag Logical. If \code{TRUE}, function \code{sortMaxOnDiag} is
#'        applied (rows and columns are sorted in so that maximum values
#'        were on diagonal, if possible).
#' @param bg.color The main background color (used for high values too).
#' @param low.color A background for low values.
#' @param max.color A background for maximum values.
#' @param text.size The size of text inside cells.
#' @param decimals The number of decimal positions in rounding. Default is 2
#'        (i.e., precission is 0.01).
#' @param show.max The kind of top values to be highlighted. Possible entries:
#'  "colMax" (maxima of every column), "rowMax" (maxima of every row),
#'   "max" (maximum of whole matrix) or  \code{FALSE} (maxima are not highlighted).
#' @param shades Logical. If \code{TRUE}, color of a cell varies depending
#'        on its value (except the cells with top values if \code{show.max} is not
#'         \code{FALSE}).
#' @param guide Either "legend", "colorbar" or \code{FALSE}, if no guide is needed).
#' @template ggplot
#' @examples
#'
#' # Generate data: Random guess  ============================
#'  N <- 1000 # number of observations
#'
#' Prediction <- sample(c("A","B","C","D"), N, replace = TRUE)
#' Reference  <- sample(c("A", "B","C","D","E"),N, replace = TRUE)
#'
#' tabl <- table(Prediction,Reference)
#' qplot_crosstab(tabl)
#' qplot_crosstab_sort(tabl)   # different order of columns and rows
#' qplot_crosstab0(tabl)    # no colors
#' qplot_crosstab0s(tabl)   # no colors, different order of columns and rows
#'
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
                            sort.maxOnDiag = FALSE,
                            bg.color  = "wheat2",
                            max.color = "gold3",
                            low.color = "grey80",
                            decimals = 2,
                            show.max = c("colMax", "rowMax", "max",  FALSE),
                            shades = TRUE,
                            guide = c("legend","colorbar",FALSE)) {
    if (!is.table(tabl)) {       tabl <- as.table(tabl)    }

    # Round *****************************************************************
    tabl.a   <- round(tabl,decimals)

    # tabl.a   <- as.table(tabl.a)
    #
    # # Preserve names of dimensions ******************************************
    # names(dimnames(tabl.a)) <- names(dimnames(tabl))

    # Sort: put maxima on diagonal, if possible *****************************
    if (sort.maxOnDiag == TRUE) {tabl.a <- sortMaxOnDiag(tabl.a)}

    # Make a long format data frame *****************************************
    tabl.m <- reshape2::melt(tabl.a)

    # # Sort levels to plot data correctly **********************************
    tabl.m[[1]] <- factor(tabl.m[[1]], levels = rev(levels(tabl.m[[1]])))
    tabl.m[[2]] <- factor(tabl.m[[2]], levels =     levels(tabl.m[[2]]))

    # # Determine COLORS ****************************************************
    ColValue <- rep(TRUE, length(tabl.a))
    ColValue <- as.factor(tabl.m$value != 0)
    tabl.m$ColValue <- ColValue

    # # Determine FILL colors ***********************************************
    FillValue   <- rep(.8,length(tabl.a))

    # Shades
    if (shades == TRUE) {
        FillValue[] <- scales::rescale(tabl.m$value, to = c(0, .8))
        gBreaks0 = c(.8, .4, 0)
        gLabels0 = c("High",
                     "Intermediate",
                     "Low")
    } else {
        FillValue[] <- .8
        gBreaks0 = NULL
        gLabels0 = NULL

    }

    # Max value
    gBreaksMax = NULL
    gLabelsMax = NULL

    switch(show.max[1],

                   colMax = {
                       indF <- which.max.perCol(tabl.a)
                       FillValue[indF] <- 1
                       gBreaksMax = c(.95)
                       gLabelsMax = c("Maximum (per column)")
                   },

                   rowMax = {
                       indF <- which.max.perRow(tabl.a)
                       FillValue[indF] <- 1
                       gBreaksMax = c(.95)
                       gLabelsMax = c("Maximum (per row)")
                   },

                   max    = {
                       indF <- which.max.all(tabl.a)
                       FillValue[indF] <- 1
                       gBreaksMax = c(.95)
                       gLabelsMax = c("Maximum")
                   }
   )

    # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    tabl.m$FillValue <- FillValue
    gBreaks <- c(gBreaksMax, gBreaks0)
    gLabels <- c(gLabelsMax, gLabels0)

    if (any(is.null(gBreaks),is.null(gLabels))) guide <- FALSE
    # ***********************************************************************
    #  Plot   ***************************************************************
    nameX <- names(tabl.m)[2]
    nameY <- names(tabl.m)[1]

    p <- ggplot(tabl.m, aes_string(x = nameX, y = nameY)) +
         geom_tile(aes(fill = FillValue), colour = "grey50") +
         geom_text(aes(label = value, colour = ColValue), size = text.size) +
         scale_color_manual(values = c("FALSE" = "grey60","TRUE" = "black"),
                             guide = FALSE ) +

        labs(title = subt(Title, subTitle) ,
             x = {if (is.null(xLabel)) nameX else xLabel},
             y = {if (is.null(yLabel)) nameY else yLabel}
        ) +

        # p <- p +
        scale_fill_gradientn(colours = c(low.color, bg.color, max.color, max.color),
                                 values  = c(0,.8,.93,1),
                                 limits = c(0,1),
                                 na.value = "grey60",
                                 guide = guide[1],
                                 name = "Freqency/Counts",
                                 breaks = gBreaks,
                                 labels = gLabels )


    # p <- cowplot::ggdraw(cowplot::switch_axis_position(p, axis = 'x'))

    return(p)
}

#  ------------------------------------------------------------------------
#' @rdname qplot_crosstab
#' @export
qplot_crosstab_sort <- function(tabl,
                                sort.maxOnDiag = TRUE,
                                show.max = TRUE,
                                shades   = TRUE,
                                ...) {
    qplot_crosstab(tabl,
                   sort.maxOnDiag = sort.maxOnDiag,
                   show.max = show.max,
                   shades   = shades,
                   ...)
}


#  ------------------------------------------------------------------------


#' @rdname qplot_crosstab
#' @template same
#'
#' @export

qplot_crosstab0 <- function(tabl,
                           Title  = "Cross-tabulation",
                           xLabel = NULL,
                           yLabel = NULL,
                           subTitle = NULL,
                           text.size = 5,
                           sort.maxOnDiag = FALSE,
                           bg.color  = "wheat2",
                           decimals = 2,
                           show.max = FALSE,
                           shades   = FALSE,
                           guide    = FALSE,
                           ...) {
    qplot_crosstab(tabl,
     Title  = Title,
     xLabel = xLabel,
     yLabel = yLabel,
     subTitle = subTitle,
     text.size = text.size,
     sort.maxOnDiag = sort.maxOnDiag,
     bg.color  = bg.color,
     decimals = decimals,
     show.max = show.max,
     shades   = shades,
     guide    = guide,
     ...)
}

#  ------------------------------------------------------------------------


#' @rdname qplot_crosstab
#' @export
qplot_crosstab0s <- function(tabl,
                            Title  = "Cross-tabulation",
                            xLabel = NULL,
                            yLabel = NULL,
                            subTitle = NULL,
                            text.size = 5,
                            sort.maxOnDiag = TRUE,
                            bg.color  = "wheat2",
                            decimals = 2,
                            show.max = FALSE,
                            shades   = FALSE,
                            guide    = FALSE,
                            ...) {
    qplot_crosstab(tabl,
                   Title  = Title,
                   xLabel = xLabel,
                   yLabel = yLabel,
                   subTitle = subTitle,
                   text.size = text.size,
                   sort.maxOnDiag = sort.maxOnDiag,
                   bg.color  = bg.color,
                   decimals = decimals,
                   show.max = show.max,
                   shades   = shades,
                   guide    = guide,
                   ...)
}


