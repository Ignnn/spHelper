#' A screeplot made with \code{lattice} [Use \code{plot_infoDim} instead]
#'
#' Plot the result of \code{\link{InfoDim}}
#'
#' Plot the results of function \code{\link{InfoDim}} and return `trellis`
#' graphics object.
#'
#' @param Object An object (list), generated by function \code{\link{InfoDim}}
#'
#' @param n.comp.SHOW A number of components to show, default is 20. This
#' number can be corrected if either vector of eigenvalues is smaller than 20
#' or information dimension is higher than 15.
#'
#' @param selected (optional parameter) A number of components sellected
#'        (will be plotted as a separate vertical line).
#'
#' @param Title The title of the plot.
#'
#'
#' @return A scree plot: a "lattice" (class "trellis") plot which helps to
#'         determine the number of nenessary components (e.g. for PCA).
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
#' @references http://www.originlab.com/doc%5Cen/Tutorial/images/Principal_Component_Analysis/Pca_scree_plot.png
#'
#' @family \pkg{spHelper} plots
#' @family information dimension functions

InfoDim_plot <- function(Object, n.comp.SHOW = 20, selected = NULL,
                         Title = "Scree Plot"){
    # Adjust n.comp.SHOW
    At_least <- max(n.comp.SHOW,  Object$dim + 5)
    But_no_more_than <- length(Object$eigenval)
    n.comp.SHOW = min(At_least, But_no_more_than)
    #     x.step  <- floor(n.comp.SHOW/5)
    #     x.ticks <- seq(x.step,n.comp.SHOW,x.step)

    # Plot
    PlotExplained <- lattice::xyplot(
        100 * explained[1:n.comp.SHOW] ~ n.comp[1:n.comp.SHOW],
        scales = list(y = list(log = 10)#,
                      # x = list(at = x.ticks)
        ),
        yscale.components = latticeExtra::yscale.components.log10ticks,
        # xscale.components = xscale.components.subticks(),
        main = Title,
        data = Object,
        type = c("b","g"),
        cex = 1.2,
        xlim = c(0,n.comp.SHOW + 0.5),
        xlab = "Number of components",
        ylab = "Percentage of variance explained",
        abline = list(v = c(Object$exactDim,selected),
                      lty = "dotted",
                      col = c("red","green4")),
        key = list(corner = c(.95, .95),
                 lines = list(col = c("#0080ff","red","green4"),
                              lty = c(1,2,2),
                              lwd = 1),
                 text = list(c("Percentage explained",
                              paste("Information dimension =",
                                    round(Object$exactDim,1)),
                              paste("Selected =",selected))
                 )
        )
    )
    return(PlotExplained)
}
