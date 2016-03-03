# Komponentų spektrai (atskirai) ---------------------------------
#' @rdname plot_kSp
#' @template same
#' @export
qplot_kSpFacets <- function(...,Title = "Components (stacked)",
                            normalize = "auto", facets = TRUE)
{ p <- plot_kSp(...,Title = Title, normalize = normalize, facets = facets)
return(p)
}
