# ~~~~~~ Atvaizdavimo funkcijos ~~~~~~ ----------------------------------------


# ***** Komponentų spektrai (kartu) ***** ---------------------------------------

#' [!] Plot spectra of components (a.k.a loadings) on one plot.
#'
#' Plot spectra of components (a.k.a loadings) on one plot.
#'
#' @param loadings - \code{\link{hyperSpec}} object
#' @param Title    - the title of graph
#' @param yLabel     - label of ordinate axis
#' @param xLabel     - label of x axis
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
                     Title = "Components",
                     xLabel = labels(loadings, "spc"),
                     yLabel = labels(loadings, ".wavelength"))
{
    hyperSpec::chk.hy(loadings)

    l <- loadings
# Create names of components, if they do not exist
    if (!('kNames' %in% colnames(l))) l[,'kNames'] = rownames(l)

    l$rows = 1:nrow(l)               # Create variable with row numbers
    l <- l[,c('spc','kNames','rows')]# Rename variables

    l <- as.long.df(l)

# Define the limits
    limMIN <- ifelse(min(l$spc)>=0, 0, min(l$spc)*1.03)
    limMAX <- ifelse(max(l$spc)<=0, 0, max(l$spc)*1.03)
# Plot
    p <- ggplot(l, aes (x = .wavelength,
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
        ggtitle(subt(Title)) +
        xlab(xLabel) +
        ylab(yLabel)

    return(p)
}

# Komponentų spektrai (atskirai) ---------------------------------

#' Plots spectra of components (a.k.a loadings) on separate graphs
#'
#'
#' @param loadings - \code{\link{hyperSpec}} object
#' @param Title     - the title of graph
#' @param yLabel    - label of ordinate axis
#' @param xLabel    - label of ordinate axis
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
                            Title = "Components (stacked)",
                           xLabel = labels(loadings, "spc"),
                           yLabel = labels(loadings, ".wavelength"),
                        normalize = any(loadings$spc>0)-any(loadings$spc<0))
{
    hyperSpec::chk.hy(loadings)

    loadings_norm <- switch(as.character(as.numeric(normalize)),
         `0` =  loadings,
         `1` =  sweep(loadings, 1, max, `/`),# `+1` normalize to max value
        `-1` = -sweep(loadings, 1, min, `/`),# `-1` normalize to min value
         stop("'normalize' is incorrect. Must be either -1, 0 or 1.")
        )

    # Plot: !!! pass all necessary variables
    p <- plot_kSp(loadings = loadings_norm,  xLabel = xLabel, yLabel = yLabel) +
         facet_grid(kNames ~., scales = "free")


    return(p)
}


# ***** Add title with SubTitle ***** ------------------------------------------------------

#' @name withSubTitle
#' @aliases withSubTitle
#' @aliases subt
#'
#' @title Make title with subtitle
#'
#' Make title with second line as subtitle. Function
#' uses \code{\link[base]{bquote}} and \code{\link[grDevices]{atop}}.
#'
#' @param Title - the first line of title, which will be in bold.
#' @param subTitle - the second line of title, which will have smaller
#' font size and will be in italic.
#'
#' @return Formated title
#' @export
#' @import grDevices
#'
#' @examples
#'
#' identical(subt("Cars"), withSubTitle("Cars"))
#' ## TRUE
#'
#' plot(cars[,1:2], main = "Cars")
#' plot(cars[,1:2], main = withSubTitle("Cars"))
#' plot(cars[,1:2], main = subt("Cars")) # the same as previous line
#' plot(cars[,1:2], main = withSubTitle("Cars","Distance vs. speed"))
#' plot(cars[,1:2], main = withSubTitle(subTitle = "Distance vs. speed"))
#'
#'
#' library(ggplot2)
#' qplot(mpg, wt, data=mtcars) + ggtitle("Cars") # non-bold title
#' qplot(mpg, wt, data=mtcars) + ggtitle(withSubTitle("Cars")) # bold title
#' qplot(mpg, wt, data=mtcars) + ggtitle(subt("Cars")) # bold title
#' qplot(mpg, wt, data=mtcars) + ggtitle(subt("Cars","Distance vs. speed"))
#' qplot(mpg, wt, data=mtcars) + ggtitle(subt(subTitle = "Distance vs. speed"))
#'
withSubTitle <- function(Title = NULL, subTitle = NULL)
    {library(grDevices)
    # Format Title
    if (is.null(subTitle)) {#If subtitle is not provided, use only main title
        Title <- bquote(bold(.(Title)))
    } else {# otherwise add the subtitle
        Title <- bquote(atop(bold(.(Title)), atop(italic(.(subTitle)))))}

    return(Title)
    }

#' @rdname withSubTitle
#' @export
subt <- function(Title = NULL, subTitle = NULL) {
    withSubTitle(Title = Title, subTitle = subTitle)
    }

# ***** Komponentų amplitudės ***** ---------------------------------------------------

#' [!] Plot component amplitudes (a.k.a scores)
#'
#' Plot component amplitudes (a.k.a scores).
#'
#' @note May be incorrect, if cals is not hyperSpec \cr
#'        xLabel = labels(scores, ".wavelength") \cr
#'        yLabel
#'
#' @param Title      - Title
#' @param subTitle   - Second line of title
#' @param scores - object of class \code{\link[=hyperSpec-class]{hyperSpec}}
#' with scores after factorisation/decomposition.
#' @param xLabel - label of x axis
#' @param yLabel - label of y axis
#'
#'
#' @return object of class "ggplot"
#' @examples plot_kAmp(scores)
#' @export
#'
#' @import hyperSpec
#' @import ggplot2

plot_kAmp <- function(scores,
                       Title = "Component amplitudes",
                    subTitle = NULL,
                      xLabel = labels(scores, ".wavelength"),
                      yLabel = labels(scores, "spc"))
{
    hyperSpec::chk.hy(scores)

    kNames <- colnames(scores$spc)
    sc     <- scores
    AMP2   <- as.data.frame(sc$spc)
    names(AMP2) <- kNames # paste0("Komp_", names(AMP2))

    sc <-AMP2   %>%
        cbind(sc$..["gr"])   %>%
        dplyr::mutate(row = row_number())  %>%
        tidyr::gather(Komponentas,Amplitude, -gr,  -row) %>%
        dplyr::mutate(Komponentas = factor(Komponentas,sort(kNames),sort(kNames)))

    # Plot



    p <- ggplot(sc, aes(y = Amplitude, x = Komponentas, fill = gr), size = 1)  +
        geom_violin(alpha=.2)  +
        #geom_point(alpha=.05,size = 2,
        #         position = position_jitterdodge(dodge.width=0.9)) +
        geom_boxplot(alpha=.6,
                     position = position_dodge(width = 0.9))     +
        facet_grid( ~ Komponentas, scales="free") +

        ggtitle(subt(Title,subTitle)) + xlab(xLabel) + ylab(yLabel) +

        theme(axis.text.x=element_blank(),
              legend.title=element_blank()) +
        geom_hline(yintercept = 0, size = .5,linetype=2, alpha = .5)

    if (length(unique(sc$gr)) == 1) {p <- p + scale_fill_grey()}

    return(p)


}


# ***** Plot confusion matrix ***** --------------------------------------
#
#' Plot a confusion matrix, \code{PlotConfusion}
#'
#' @param conf - a table, a square matrix.
#' @param Title Title.
#' @param subTitle Subtitle.
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

# ***** ***** -----------------------------------------------------
#' [!] Plot Remainders After Subtracting Components
#'
#' Plot difference between experimental and reconstructed spectra \cr \cr
#'
#' Plot difference between experimental (original) and reconstructed spectra.
#' Uses function \code{\link{getReconstructed}}, to calculate the reconstructed
#' spectra and subtracts it from original spectra. \cr
#'
#' Difference between experimental and reconstructed spectra'
#'
#'
#' @param loadings loadings
#' @param scores scores
#' @param Spectra Spectra (hyperSpec object)
#' @param Title Title of the plot.
#' @param spc.nmax max number of spectra to plot
#' @param color ...
#' @param stacked ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot_SpDiff
#'
plot_SpDiff <- function(loadings,scores,Spectra,
                        Title = 'Remainders After Subtracting Components',
                        color = if(".color" %in% ls(Spectra$..)) Spectra$.color else "tan3",
                        stacked = if ("ID" %in% ls(Spectra$..)) Spectra$ID else NULL,
                        spc.nmax = 2000)
{

    SpRE <- getReconstructed(loadings,scores,Spectra)

    plot(Spectra - SpRE,
         spc.nmax = spc.nmax,
         col = color,
         stacked = stacked,
         title.args = list(main = Title)
    )
}



# ***** ggplot LAYERS ***** ------------------------------------------------

#' [!] Convex hull layer for ggplot2
#'
#' Source: vignette \href {https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html}{Extending ggplot2}
#'
#' @param mapping
#' @param data
#' @param geom
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'              geom_point() +
#'              stat_chull(fill = NA, colour = "black")
#'
#'
#' ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'              geom_point() +
#'              stat_chull(fill = NA)
#'
#'
#'ggplot(mpg, aes(displ, hwy)) +
#'              stat_chull(geom = "point", size = 4, colour = "red") +
#'              geom_point()
#'
stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {

    StatChull <- ggplot2::ggproto("StatChull", Stat,
                                  compute_group = function(data, scales) {
                                      data[chull(data$x, data$y), , drop = FALSE]
                                  },

                                  required_aes = c("x", "y")
    )
    layer(
        stat = StatChull, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

# ***** -------------------------------------------------------------------------

#' [!] Plot a statistic of spectra in hyperSpec object by group.
#'
#' Plot a statistic of spectra in hyperSpec object by group.
#'
#' @param Spectra - ...
#' @param by by <- as.factor(by)
#' @param FUN - ...
#' @param Title - ...
#' @param All - plot additional statistic of all spectra.
#' @param fixed.colors - flag if color scheme where ".All" is plotted as
#'        a black line shpuld be used.
#' @param All.color - ...
#' @param gr.color - ...
#' @param All.linetype - ...
#' @param gr.linetype - ...
#' @param All.size - ...
#' @param gr.size - ...
#' @param legend.title - ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' qplotStat(chondro,clusters,mean)
#' qplotStat(chondro,clusters,mean,All=FALSE)
#' qplotStat(chondro,clusters,mean_sd,All=FALSE) + facet_grid(.~clusters)
#'
#' qplotStat(chondro,clusters,median,All=FALSE, fixed.colors=FALSE)
#' qplotStat(chondro,clusters,median, "My Title")
#'
#'
#' # Use .aggregate in make facets, to avoid facet called "NA":
#'
#' qplotStat(chondro,clusters,mean_pm_sd) + facet_grid(.~clusters)
#' qplotStat(chondro,clusters,mean_pm_sd) + facet_grid(.~.aggregate)
#'
qplotStat <- function(Spectra,
                      by =  stop("Argument 'by' is missing."),
                      FUN = stop("Argument 'FUN' is missing."),
                      Title = fCap(as.character(match.call()$FUN)),
                      All = TRUE,
                      fixed.colors = TRUE,
                      All.color = "black",
                       gr.color = RColorBrewer::brewer.pal(8,"Dark2"),
                      All.linetype = "dashed",
                       gr.linetype  = "solid",
                      All.size =  1.1,
                       gr.size =  0.8,
                      legend.title = element_blank()
                      ){

    varName <- as.character(match.call()$by)
    by <- if (varName %in% colnames(Spectra)) Spectra[[,varName]] else by
    # by <- as.factor(by)

    # match.call()$by

    if (All == TRUE) {# All - plot statistic by all spectra?
            sp <-    spStat(Spectra, by = by, FUN = FUN)
    } else {sp <- aggregate(Spectra, by = by, FUN = FUN)}


    nl.gr  <- sum(levels(sp$.aggregate)!=".All")

    fixedColors <- if (fixed.colors){
        colors    <- c(gr.color[1:nl.gr], All.color)
        scale_color_manual(values = colors)
        } else NULL


    p <- qplotspc(sp, spc.nmax = 1000,
                  mapping = aes(x = .wavelength,
                                y = spc,
                                colour   = .aggregate,
                                group    = .rownames,
                                size     = .aggregate,
                                linetype = .aggregate)) +
        labs(title= subt(Title))+
        scale_size_manual(    values=c(rep(gr.size,     nl.gr), All.size))  +
        scale_linetype_manual(values=c(rep(gr.linetype, nl.gr), All.linetype)) + # ,guide=FALSE
        fixedColors +
        theme_bw() +
        theme(legend.title = legend.title)


    return(p)
}
