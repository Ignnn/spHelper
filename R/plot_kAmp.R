# ***** Amplitudes of components ***** ---------------------------------------------------

#' [!] Plot component amplitudes (a.k.a scores)
#'
#' Plot component amplitudes (a.k.a scores).
#'
#' @note May be incorrect, if cals is not hyperSpec \cr
#'        xLabel = labels(scores, ".wavelength") \cr
#'        yLabel
#'
#' @template labels
#' @template subtitle
#' @param scores An object of class \code{\link[=hyperSpec-class]{hyperSpec}}
#' with scores after factorisation/decomposition.
#' @param by A name of grouping variable. Default is \code{by = "gr"}.
#'
#'
#' @return object of class "ggplot"
#' @examples
#'
#' data(Scores)
#' plot_kAmp(Scores)
#'
#' data(Scores3)
#' plot_kAmp(Scores3, by = "class")
#'
#' @export
#' @family spHelper plots
#'
#' @import ggplot2
#' @import hyperSpec
#'
#'

plot_kAmp <- function(scores,
                      Title = "Component amplitudes",
                      subTitle = NULL,
                      xLabel = labels(scores, ".wavelength"),
                      yLabel = labels(scores, "spc"),
                      by = "gr")
{
    hyperSpec::chk.hy(scores)

    ## Quotes are not necessary if uncommented:
    # CALL <- match.call()
    # if (!is.null(CALL$by)) {
    #     by <- as.character(c(CALL$by))
    # }

    kNames <- colnames(scores$spc)
    sc     <- scores
    AMP2   <- as.data.frame(sc$spc)
    names(AMP2) <- kNames # paste0("Komp_", names(AMP2))

    sc$gr <- sc$..[,by]

    sc <- AMP2   %>%
        cbind(sc$..["gr"])   %>%
        dplyr::mutate(row = row_number())  %>%
        tidyr::gather(Komponentas,Amplitude, -gr, -row) %>%
        dplyr::mutate(Komponentas = factor(Komponentas,sort(kNames),sort(kNames)))

    # Plot
    p <- ggplot2::ggplot(sc,
                         aes(y = Amplitude, x = Komponentas, fill = gr),
                         size = 1
                   )  +
        geom_violin(alpha = .2)  +
        #geom_point(alpha =. 05,size = 2,
        #         position = position_jitterdodge(dodge.width = 0.9)) +
        geom_boxplot(alpha = .6,
                     position = position_dodge(width = 0.9))     +
        facet_grid(~Komponentas, scales = "free") +

        ggtitle(subt(Title,subTitle)) + xlab(xLabel) + ylab(yLabel) +

        theme(axis.text.x = element_blank(),
              legend.title = element_blank()) +
        geom_hline(yintercept = 0, size = .5,linetype = 2, alpha = .5)

    if (length(unique(sc$gr)) == 1) {p <- p + scale_fill_grey()}

    return(p)
}
