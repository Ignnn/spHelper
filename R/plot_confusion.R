# ***** Plot a confusion matrix ***** --------------------------------------
#
#' @name plot_confusion
#' @aliases plot_confusion
#' @aliases plot_confusion2
#'
#' @title [!] Visualize a confusion matrix (classification table)
#'
#' @description Plot a confusion matrix (classification table) and
#'  additional statistics.
#'
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
#' @param guide Logical. If \code{TRUE}, a legend is plotted.
#' @param text.size The size of text inside cells
#' @param decimals The number of decimal positions in rounding. Default is 2
#'        (i.e., precission is 0.01).
#' @return A plot of confusion matrix and additional statistics (`ggplot` object).
#' @examples
#'
#' # Generate data: Random guess  ============================
#'  N <- 1000 # number of observations
#'
#' Prediction <- sample(c("A","B","C","D"), N, replace = TRUE)
#' Reference  <- sample(c("A", "B","C","D"),N, replace = TRUE)
#'
#' # This function:
#' plot_confusion2(Prediction,Reference)
#'
#' # does the same as:
#' conf <- table(Prediction,Reference)
#' plot_confusion(conf)
#'
#' # At least 50% of the cases agree =========================
#' ind <- sample(1:N,round(0.50*N))
#' Reference[ind] <- Prediction[ind]
#' conf2 <- table(Prediction,Reference)
#'
#' plot_confusion(conf2)
#'
#' # Most of the cases agree =================================
#' ind <- sample(1:N,round(N*.80))
#' Reference[ind] <- Prediction[ind]
#' conf3 <- table(Prediction,Reference)
#'
#' plot_confusion(conf3)
#'
#' # Proportions =============================================
#'
#' plot_confusion(conf3)
#'
#' # Shades: proportional =====================================
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
#' # Shades: constant and none ================================
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
                          xLabel = NULL,
                          yLabel = NULL,
                          subTitle = NULL,
                          shades = c("prop","max","const","none"),
                          guide = FALSE,
                          text.size = 4,
                          decimals = 2) {
    # require(dplyr)

    if (!is.table(conf)) {       conf <- as.table(conf)    }

    # Calculate accuracy measures
    `<Sensitivity>` <- diag(prop.table(conf,2)) # Sensitivity
           PV       <- diag(prop.table(conf,1)) # "Positive Predictive Value"
           K        <- psych::cohen.kappa(conf)[["kappa"]]

    # Add accuracy measures to the main matrix/table
    `<PPV>` <- c(PV, K)
    conf.a <- rbind(conf,  `<Sensitivity>`)  %>% cbind( ., `<PPV>`)


    conf.a   <- round(conf.a,decimals)
    # Preserve names of dimensions
    conf.a <- as.table(conf.a)
    names(dimnames(conf.a)) <- names(dimnames(conf))

    # Make a long format data frame *****************************************
    conf.m <- reshape2::melt(conf.a)

    # Sort levels to plot data correctly ************************************
    conf.m[[1]] <- factor(conf.m[[1]], levels = rev(levels(conf.m[[1]])))
    conf.m[[2]] <- factor(conf.m[[2]], levels =     levels(conf.m[[2]]))

    # Determine COLORS ******************************************************

    N <- length(conf.a) # number of elemants in the extended matrix
    nr <- nrow(conf)
    nc <- ncol(conf)
    n <- max(nr,nc);# number of rows/columns in the main matrix
    ind.Se   <- base::setdiff(which.in(row,conf.a, nrow(conf.a)), N)
    ind.PV   <- base::setdiff(which.in(col,conf.a, ncol(conf.a)), N)
    ind.SePV <- sort(c(ind.Se,ind.PV))
    ind.main <- base::setdiff(1:N, c(ind.SePV, N)) # indices of the main matrix elements
    ind.diag <- base::setdiff(which.in.diag(conf.a), N) # ind. of the diag. el. in main matrix

    ColValue <- rep(NA, N)

    accShades <- function(){
        #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Rescale Se and PV
        ColValue[ind.SePV] <- scales::rescale(conf.a[ind.SePV],c(-1,1),c(1/n,1) )
        # Rescale Kappa  - - - - - - - - - - - - - - - - - - - - - - -
        ColValue[N] <- scales::rescale(conf.a[N],c(-1,1),c(0,1))
        # Correct too small and too high values- - - - - - - - - - - -
        ColValue[ColValue < -1] <- -1
        ColValue[ColValue >  1] <-  1
        #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        return(ColValue)
    }

    switch(shades[1],
           prop =  {# best for square matrix
               ColValue[ind.main] <- -conf.m$value[ind.main] /
                                      sum(conf) * n * (n - 1);
               ColValue[ind.diag] <- -ColValue[ind.diag] / (n - 1)
               ColValue <- accShades()
            },

           max = {
               ColValue[ind.main] <- -conf.m$value[ind.main] / max(conf);
               ColValue[ind.diag] <- -ColValue[ind.diag]
               ColValue <- accShades()
            },

           const = {
               ColValue[ind.main]   <- -.60
               ColValue[ind.diag]   <-  .70
               # ColValue[N]          <- 0.10

            },
        # Just constant grey color (no red nor green colors)
           ColValue[] <- 0
    )

    conf.m$ColValue <- ColValue
    # *************************************************************
    # K <- substitute(kappa == K, list(K = conf.a[N]))
    # conf.m$value[N] <- K
    conf.m$value[N] <- sprintf("k=%.2f",conf.a[N])
    #
    # Plot
    # *************************************************************
    nameX <- names(conf.m)[2]
    nameY <- names(conf.m)[1]

    p <- ggplot(conf.m, aes_string(x = nameX, y = nameY)) +
         geom_tile(aes(fill = ColValue), colour = "grey50") +
         geom_text(aes(label = value), size = text.size) +
         geom_hline(size = 1.2, color = "grey30", yintercept = 1.5    ) +
         geom_vline(size = 1.2, color = "grey30", xintercept = nc + .5) +

        scale_fill_gradient2(high  = "#209D20",   # "#008000",
                              mid  = "#eeeeee", #mid = "#f2f6c3",
                              midpoint = 0,
                              low  = "#dd4040",#"tomato2",
                              na.value = "grey60",

                              guide = {if (guide) "colourbar" else FALSE} ,
                              name = "Accuracy",
                              limits = c(-1,1),
                              breaks = c(1,-1),
                              labels = c("High",
                                         "Low")
         ) +

         labs(title = subt(Title, subTitle) ,
              x = {if (is.null(xLabel)) nameX else xLabel},
              y = {if (is.null(yLabel)) nameY else yLabel}
         )

    p <- cowplot::ggdraw(cowplot::switch_axis_position(p, axis = 'x'))

    return(p)
}


# ============================================================================
#' @rdname plot_confusion
#'
#' @param Prediction A factor variable with \bold{predicted} groups.
#' @param Reference A factor variable  with \bold{reference} groups.
#' @param ... Parameters used in \code{plot_confusion}.
#'
#' @export

plot_confusion2 <- function(Prediction, Reference,...){
    if (length(Prediction) != length(Reference)) {
        stop("Lengths of vectors `Prediction` and `Reference` must be equal.")
    }
    conf <- table(Prediction,Reference)
    plot_confusion(conf = conf, ...)
}
