# ~~~~~~ Atvaizdavimo funkcijos ~~~~~~ ----------------------------------------


# ***** Komponentų spektrai (kartu) ***** ---------------------------------------

#' Plots spectra of components (a.k.a loadings) on one plot.
#'
#' @param loadings - \code{\link{hyperSpec}} object
#' @param Title    - the title of graph
#' @param ylab     - label of ordinate axis
#'
#' @return ggplot2 object
#' @examples
#' plot_kSp(loadings)
#'
#' @export
#'
#' @import hyperSpec
#' @import ggplot2


plot_kSp <- function(loadings,
                     Title = "Komponentai (kartu)",
                      ylab = "I, sant.vnt")
{
    hyperSpec::chk.hy(loadings)

    l <- loadings
# Create names of components, if they do not exist
    if (!('kNames' %in% colnames(l))) l$kNames = rownames(l)

    l$rows = 1:nrow(l)               # Create variable with row numbers
    l <- l[,c('spc','kNames','rows')]# Rename variables

    l <- as.long.df(l)

# Define the limits
    limMIN <- ifelse(min(l$spc)>=0, 0, min(l$spc)*1.03)
    limMAX <- ifelse(max(l$spc)<=0, 0, max(l$spc)*1.03)
# Plot
    ggplot(l, aes (x = .wavelength,
                   y = spc,
                   group = rows,
                   color = kNames,
                   fill  = kNames)) +
        geom_hline(yintercept = 0, size = 1,linetype=1, alpha = .5)+
        geom_line(size=1) +
        geom_density(stat="Identity", alpha= .1) +
        theme_bw() +
        theme(legend.title=element_blank()) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0),
                           limits = c(limMIN, limMAX)
                           # limits = c(0, max(l$spc)*1.03)
                          # , breaks = round(seq(0, max(l$spc),length.out = 3))
                                          ) +
        ggtitle(Title) +
        xlab(labels(loadings, ".wavelength")) +
        ylab(label = ylab)
}

# Komponentų spektrai (atskirai) ---------------------------------

#' Plots spectra of components (a.k.a loadings) on separate graphs
#'
#'
#' @param loadings - \code{\link{hyperSpec}} object
#' @param Title    - the title of graph
#' @param ylab     - label of ordinate axis
#' @param normalize - flag if plot normalized components
#' default: normalize, if needed:
#'   Below0 <- any(loadings$spc < 0)
#'   Above0 <- any(loadings$spc > 0)
#'
#'   if (Above0 - Below0):
#'   `0` - do not normalize;
#'  `+1` - normalize to max value;
#'  `-1` - normalize to min value
#'
#' @return object of class "ggplot2"
#' @examples
#' ...
#'
#' @export
#'
#' @import hyperSpec

plot_kSpFacets <- function(loadings,
                           Title = "Komponentai (atskirai)",
                            ylab = "I, sant.vnt.",
                       normalize = any(loadings$spc>0)-any(loadings$spc<0)
                       )
{


    loadings_norm <- switch(as.character(as.numeric(normalize)),
         `0` =  loadings,
         `1` =  sweep(loadings, 1, max, `/`),# `+1` normalize to max value
        `-1` = -sweep(loadings, 1, min, `/`),# `-1` normalize to min value
         stop("'normalize' is incorrect. Must be either -1, 0 or 1.")
        )

    # plot
    plot_kSp(loadings = loadings_norm,
             Title = Title,
             ylab  = ylab) +
         facet_grid(kNames ~., scales = "free")
}

# ***** Komponentų amplitudės ***** ---------------------------------------------------

#' Plot component amlitudes (a.k.a scores)
#' @param scores - object of class \code{\link{hyperSpec}} with scores after factorisation/decomposition
#' @return object of class "ggplot"
#' @examples plot_kAmp(scores)
#' @export
#'
#' @import hyperSpec
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @import ggplot2

plot_kAmp <- function(scores, Title = "Komponentų amplitudžių skirstiniai grupėmis")
{
    hyperSpec::chk.hy(scores)

#     library(dplyr)
#     library(tidyr)

    kNames <- colnames(scores$spc)
    sc     <- scores
    AMP2   <- as.data.frame(sc$spc)
    names(AMP2) <- kNames # paste0("Komp_", names(AMP2))

    sc <-AMP2   %>%
        cbind(sc$..["gr"])   %>%
        dplyr::mutate(row = row_number())  %>%
        tidyr::gather(Komponentas,Amplitude, -gr,  -row) %>%
        dplyr::mutate(Komponentas = factor(Komponentas,sort(kNames),sort(kNames)))

    # Braižomas paveikslas
    ggplot(sc, aes(y = Amplitude, x = Komponentas, fill = gr), size = 1)+
        geom_violin(alpha=.2)  +
        #geom_point(alpha=.05,size = 2,
        #         position = position_jitterdodge(dodge.width=0.9)) +
        geom_boxplot(alpha=.6,
                     position = position_dodge(width = 0.9))     +
        facet_grid( ~ Komponentas, scales="free") +

        ggtitle(Title) +
        xlab(labels(scores,'.wavelength'))+
        ylab(labels(scores,'spc'))+

        theme(axis.text.x=element_blank(),
              legend.title=element_blank()) +
        geom_hline(yintercept = 0, size = .5,linetype=2, alpha = .5)
}


# ***** Plot confusion matrix ***** --------------------------------------
#
#' Plot a confusion matrix, \code{PlotConfusion}
#'
#' @param conf - a table, a square matrix
#' @return ...
#' @examples
#' Prediction <- sample(c("A","B","C"),1000, replace = T)
#' Reference  <- sample(c("A","B","C"),1000, replace = T)
#' conf <- table(Prediction,Reference)
#'
#'
#' PlotConfusion(conf)
#' PlotConfusion(prop.table(conf,1))
#' PlotConfusion(prop.table(conf,2))
#'
#' @export
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#'
PlotConfusion <- function(conf)
{
    conf <- round(conf,2)

    conf.m <- reshape2::melt(conf)
    names(conf.m)[1:2] <- c("Actual","Predicted")
    conf.m$Actual <- factor(conf.m$Actual, levels = rev(levels(conf.m$Actual)))

    # Spalvinam įstrižainę ****************************************
    nRows <- nrow(conf.m)
    nCols <- length(unique(conf.m[,2]))
    ind <- seq(1,nRows,nCols+1)

    #     conf.m$ColValue      <- conf.m$value*(-1)
    #     conf.m$ColValue[ind] <- conf.m$ColValue[ind]*(-1)

    conf.m$ColValue      <- -1
    conf.m$ColValue[ind] <- 1
    # *************************************************************

    p <-
        ggplot(conf.m, aes(Actual, Predicted)) +
        scale_fill_gradient2(high = "#006400", mid="#f2f6c3",
                             midpoint=0,
                             low  = "#cd0000")+
        geom_tile(aes(fill = ColValue),colour = "white") +
        geom_text(aes(label=value),size=6)
    return(p)
}

