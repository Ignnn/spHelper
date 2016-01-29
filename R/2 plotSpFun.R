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
                       normalize = any(loadings$spc<0)-any(loadings$spc>0)
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
        # labs(x="Komponento numeris")+
        ggtitle(Title) +
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



# ****** Komp: informacijos dimensija ******* -----------------------------------

#' Calculate information dimension of a matix.
#'
#' The function calculates a measure, called "information dimension".
#'
#'
#' @param  Matrix - data matrix (rows = observations, columns = variables)
#'
#' @return A list with fields:
#'  \itemize{
#'  \item{$dim }{- information dimension ,rounded towards positive infinitive}
#'  \item{$exactDim  }{- information dimension (fractional, not rounded)}
#'  \item{$explained  }{- a vector of eigenvalues, normalized by sum of eigenvalues,
#'   which can be used to determine the importance of (principal) components}
#'  \item{$eigenvalues }{- a vector of eigenvalues}
#'  \item{$n.comp }{- avector with integers from 1 to length(eigenvalues)}
#' }
#'
#' @references [1]	R. Cangelosi and A. Goriely, Component retention in principal
#'       component analysis with application to cDNA microarray data.
#'       Biol Direct, 2, 2 (2007), \url{http://dx.doi.org/10.1186/1745-6150-2-2}
#'
#' @note
#' Prieš pradedant vykdyti operaciją, svarbus žingsnis pasirinkti tinkamą
#' normavimo buda. To nepadarius gausime klaidingą atsakymą.
#' y=sp_normuok(y,x,'1',495);
#'
#' Taip pat labai svarbus ir triukšmo lygis. Didėjant triukšmui atitinkamai
#' padidinamas maksimalus dimensijų skaičius.
#'
#'
#' @note
#' eigenvalues - Tikrines reiksmes / Singular values
#' pk - tikimybines dimensiju vertes, skirtos entropijos ivertinimui.
#' explain = pk;
#'
#' @seealso InfoDim_plot
#' @export
#'
#' @examples
#'  my_matrix <- matrix(rexp(200, rate=.1), ncol=20)
#'
#'  my_result <- InfoDim(my_matrix)
#'
#'  # Investigate the result
#'  str(my_result)
#'  my_result$exactDim
#'  my_result$dim
#'
#'  #Plot
#'  my_plot <- InfoDim_plot(my_result)
#'  my_plot
#'
InfoDim <- function(Matrix)
{

     eigenval   <- svd(Matrix)$d
      explain   <- eigenval / sum(eigenval);
    exact_dim   <- prod(sapply(explain,function(x){x^-x}));
    dim         <- ceiling(exact_dim);    # % Round towards infinitive

    output <- list(      dim   = dim,
                    exactDim   = exact_dim,
                   explained   = explain,
                   eigenvalues = eigenval,
                   n.comp      = 1:length(explain))
    return(output)
}


#' Plot the result of \code{\link{InfoDim}}
#'
#' @param Object - an object (list), generated by function \code{\link{InfoDim}}
#' @param n.comp.SHOW - number of components to show, default is 20. This
#' number can be corrected if either vector of eigenvalues is smaller than 20
#' or information dimension is higher than 15.
#'
#' @return A plot of class "trellis" which helps to estimate the number of
#'  nenessary components
#'
#' @export
#'
#' @examples
#'  my_matrix <- matrix(rexp(200, rate=.1), ncol=20)
#'
#'  my_result <- InfoDim(my_matrix)
#'
#'  # Investigate the result
#'  str(my_result)
#'  my_result$exactDim
#'  my_result$dim
#'
#'  #Plot
#'  my_plot <- InfoDim_plot(my_result)
#'  my_plot
#'
#' @importFrom lattice xyplot
#'
InfoDim_plot <- function(Object, n.comp.SHOW = 20){

    # Adjust n.comp.SHOW
            At_least <- max(n.comp.SHOW,  Object$dim+5)
    But_no_more_than <- length(Object$eigenval)
    n.comp.SHOW = min(At_least, But_no_more_than)

    # Plot
    PlotExplained <- lattice::xyplot(
        log(100*explained[1:n.comp.SHOW]) ~ n.comp[1:n.comp.SHOW],
        data = Object,
        type = c("b","g"),
        cex = 1.2,
        xlim = c(0,n.comp.SHOW+0.5),
        xlab = "Number of components",
        ylab = "Explained, log %",
        abline = list(v = Object$exactDim, lty = "dotted", col = "red"),
        key=list(space="top",
                 lines=list(col=c("#0080ff","red"), lty=c(1,2), lwd=1),
                 text =list(c("Normalized eigenvalues","Info.dimension")),
                 columns = 2
        )
    )
 return(PlotExplained)
}


