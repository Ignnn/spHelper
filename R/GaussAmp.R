# GaussAmp ------------------------------------------------------------------
#
# [!] Funkcija GaussAmp skirta vienai ar kelioms gausinėms kreivėms braižyti.
#
# *PARAMETRAI:*
# x  - x ašies reikšmių vektorius;
# xc - vektorius su centro padėtimis;
# w  - vektorius su vidutiniais kvadratiniais nuokrypiais, sigma;
# A  - vektorius su amplitudėmis;
# y0 - kreivės pagrindo aukštis virš x ašies (konstanta, vienoda visoms
#      kreivėms).
#
# *IŠVESTIS:*
# loadings - matrica su kreivių reikšmėmis ties atitinkamais x.
#
# *SINTAKSĖ:*
#        GaussAmp; # Funkcijos demonstracija
# loadings = GaussAmp(x,xc,w,A)
# loadings = GaussAmp(x,xc,w,A,y0)
#
#
# Autorius Ignas Čiplys       2014-10-28
# Modifikavo Vilmantas Gėgžna 2014-12-03

#' Generate Gausian curve(s) (GaussAmp)
#'
#' @param x vector of x values
#' @param xc vector with centers of Gaussian curves
#' @param w  vector with parameter w, which determines the width of Gaussian curves
#' @param A  vector with Amplitudes of Gaussian curves
#' @param y0 vector with offsets on y axis
#'
#' @note The number of curves is determined by maximal length of any of 4
#' Gausian curve parameters' (xc, w, A, y0) vector. Other parameters are
#' recycled as shown in example 2 (parameter "A")
#'
#' @return y values of Gaussian curve
#' @export
#'
#' @examples
#' # Example 1
#' x <- seq(-9.9, 10, 0.2)
#' y <- GaussAmp(x)
#'
#' plot(x,y, type = "l", col = "green3"); grid()
#'
#' # Example 2
#'
#' require(hyperSpec)
#'
#' # Make 7 lines
#' y <- GaussAmp(x, xc = 1:7,A = c(1,2))
#'
#' dim(y)
#' ##[1]   7 100
#'
#' Obj <- new("hyperSpec",spc = y,    wavelength = x,
#'          label = list (spc = "y", .wavelength = "x"))
#' plot(Obj, col = 1:nrow(Obj)); grid()
#'
GaussAmp <- function(x, xc = 0, w = 1, A = 1, y0 = 0){
    P <- max(length(xc),length(w),length(A),length(y0))

    xc <- rep_len(xc, P)
    w  <- rep_len(w,  P)
    A  <- rep_len(A,  P)
    y0 <- rep_len(y0, P)

    # Prealocate y
    y = matrix(NA ,P,length(x))

    # Generate the curves
    for (i in 1:P){ y[i,] <- y0[i]+A[i]*exp(-(((x-xc[i])^2)/(2*w[i]^2)))}

    ## Output
    return(y)
}
