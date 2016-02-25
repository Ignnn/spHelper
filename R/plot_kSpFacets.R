# Komponentų spektrai (atskirai) ---------------------------------
#' @rdname plot_kSp
#' @template same
#'
#'
#' @export
#' @family spHelper plots
plot_kSpFacets <- function(...,Title = "Components (stacked)",
                           normalize = "auto", Facets = TRUE)
{ p <- plot_kSp(...,Title = Title, normalize = normalize, Facets = Facets)
return(p)
}
