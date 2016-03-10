
#' [!] Add a standardised label for x-axis in \code{hyperSpec} object
#'
#' @template sp-hy
#' @param modeX Mode of x axis. A string, that indicates either "Fluorescence",
#'       "wavelength", "Raman" or "IR".
#' @param label A manually eneterd text or expression of the label.
#'
#' @return Object with added label.
#' @export
#'
#' @examples
#'
#' Sp2 <- hyAdd.Label.wl(Spectra, "IR")
#'
#' labels(Spectra, ".wavelength")
#' #> expression(lambda/nm)
#'
#' labels(Sp2, ".wavelength")
#' #> expression(list(Delta * tilde(nu), cm^-1))
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

hyAdd.Label.wl <- function(sp, modeX, label) {
    # Labels for wavelengths:
    if (missing(label)) {
        modeX0 <- match(tolower(modeX), c("fluorescence","wavelength",
                                          "raman",
                                          "ir"))
        if (is.na(modeX0)) stop(sprintf("The value of parameter `modeX` is not supported: %s",modeX))

        label <- switch(modeX0,
                        "1" = expression(list(lambda, nm)),               # Fluorescence
                        "2" = expression(list(lambda, nm)),               # wavelength
                        "3" = expression(list(Delta * tilde(nu), cm^-1)), # Raman
                        "4" = expression(list(tilde(nu), cm^-1))          # IR
        )
    }
    labels(sp, ".wavelength") <- label
    return(sp)
}
