# ***** ggplot LAYER ***** ------------------------------------------------

#' [!] Convex hull layer for ggplot2
#'
#' Source: vignette \href {https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html}{Extending ggplot2}
#'
#' @param mapping ...
#' @param data ...
#' @param geom ...
#' @param position ...
#' @param na.rm ...
#' @param show.legend ...
#' @param inherit.aes ...
#' @param ... ...
#'
#' @return Layer ofconvex hull for ggplot object.
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
                       inherit.aes = TRUE, ...)
{
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
