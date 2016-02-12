# ***** Add title with SubTitle ***** ------------------------------------------------------

#' @title M<ake bold title and subtitle
#'
#' @description Make bold title with second line as subtitle. Function
#'               uses \code{\link[base]{bquote}} and \code{\link[grDevices]{atop}}.
#'
#' @param Title The first line of title, which will be in bold.
#' @param subTitle The second line of title, which will have smaller
#'        font size and will be in italic.
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
#' plot(cars[,1:2], main = subt("Cars")) # the same as previous line
#' plot(cars[,1:2], main = subt("Cars","Distance vs. speed"))
#' plot(cars[,1:2], main = subt(subTitle = "Distance vs. speed"))
#'
#'
#' library(ggplot2)
#' qplot(mpg, wt, data=mtcars) + ggtitle("Cars") # non-bold title
#' qplot(mpg, wt, data=mtcars) + ggtitle(subt("Cars")) # bold title
#' qplot(mpg, wt, data=mtcars) + ggtitle(subt("Cars","Distance vs. speed"))
#' qplot(mpg, wt, data=mtcars) + ggtitle(subt(subTitle = "Distance vs. speed"))
#'
subt <- function(Title = NULL, subTitle = NULL)  {
    # library(grDevices)
    # Format Title
    if (is.null(subTitle)) {#If subtitle is not provided, use only main title
        Title <- bquote(bold(.(Title)))
    } else {# otherwise add the subtitle
        Title <- bquote(atop(bold(.(Title)),
                                        atop(italic(.(subTitle)))))}

    return(Title)
}
