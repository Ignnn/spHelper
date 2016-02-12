# Apskaičiuoti Komponenų amplitudes matricų daugybos būdu ------------------------------
#
#' [!] Calculate component amplitudes (a.k.a. scores) by matrix multiplication
#'
#' [!] Calculate component amplitudes (a.k.a. scores) by matrix multiplication
#'
#' @details
#'  \deqn{scores = sp * loadings * inv(loadings' * loadings)}
#'
#'  formula is taken  and adapted from [1]
#' @references [1] M. Brydegaard et al. IEEE Photonics J 2011:3(3);406-21.
#'
#' @template sp
#' @template loadings
#' @param xLabel - label that will be used for plotting x axis
#'        \code{labels(scores,".wavelength") <- xLabel}
#'
#' @param yLabel - label that will be used for plotting y axis
#'        \code{labels(scores,"spc") <- yLabel}
#'
#' @return scores - amplitudes of the components (scores)
#' @examples
#' # e.g.:
#'     sp = Object
#'     loadings = loadings
#'
#' getScores(sp, loadings)
#'
#' @export
#'
#' @import hyperSpec
#'
getScores <- function(sp, loadings,
                      xLabel = "Component",
                      yLabel = "Amplitude")
{
    y2 <- hy2mat(sp)
    loadings2 <- hy2mat(loadings)

    if (dim(y2)[2] == dim(loadings2)[2])   loadings2 <- t(loadings2)

    scores <- y2 %*% (loadings2 %*% solve(crossprod(loadings2)))

    if (class(sp) == "hyperSpec"){
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Komponentų amplitudes (išrikiuotas) paverčiam į "HyperSpec"" objektą

        scores <- decomposition(sp, scores,
                                label.wavelength = "Komponentai",
                                label.spc = "Amplitudė, a.u.")
        # Suteikiam pavadinimus
        if("kNames" %in% colnames(loadings)){
            kNames <- gsub("max: ","k_", loadings$kNames)
        }else {kNames <- paste0("Nr", 1:min(dim(loadings2)))}

        colnames(scores$spc) <- kNames

        labels(scores,".wavelength") <- xLabel
        labels(scores,"spc")         <- yLabel
    }
    # ======================================================================
    return(scores)
}
