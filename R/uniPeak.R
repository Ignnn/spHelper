# uniPeak -----------------------------------------------------------------
#
#' @name uniPeak
#' @aliases uniPeak
#' @aliases unipeak
#'
#' @title [!] Keep only the highest positive peak of a curve.
#'
#' Keep only the highest positive peak
#'
#' @param y A vector
#'
#' @return Modified \code{y}
#' @export
#'
#' @examples
#' #  Example 1 ------------------------------------------------------------------------
#'
#' x     <- seq(-10,20,.1)
#' y0    <- GaussAmp(x,xc=0, A=1) + GaussAmp(x,xc=10, A=2) -.5
#' y0NEW <- uniPeak(y0)
#'
#' # Plot the results
#' par(mfrow = c(1,1))
#' plot( x, y0,    type = "l", lty = 3,
#'         main = "'uniPeak' keeps positive part \n of highest peak only" );
#' lines(x, y0NEW, type = "l", lty = 1, lwd = 3);
#' legend("topleft", legend = c("Before","After"), lty = c(3,1))
#'
#'
#' #  Example 2 ------------------------------------------------------------------------
#'
#' x  = seq(-10,20,.1)
#' y1 = (sin(x/4) + GaussAmp(x))
#' y2 = (2*sin(x) + sin(x/5) + GaussAmp(x, xc = 5))
#' y  = rbind(y1,y2)
#'
#' yNEW <- apply(y,1,uniPeak)
#'
#' par(mfrow = c(3,1))
#'
#' # plot 1
#' matplot(x, t(y), type = "l", lty = 3,
#'         main = "A - Initial curves");
#' abline(h=0)
#'
#' # plot 2
#' matplot(x,yNEW, type = "l", lty = 1,lwd = 3,
#'         main = "B - Only the highest positive\n peaks per curve");
#' abline(h=0)
#'
#' # plot 3: both plots together
#' matplot(x, t(y), type = "l", lty = 3, main = "A and B together");
#' matlines(x,yNEW, type = "l", lty = 1,lwd = 3);
#' abline(h=0)
#'
uniPeak <- function(y)  {

    y   <- as.vector(y)
    i   <- seq_along(y)
    len <- length(y)

    di   <- c(diff(as.numeric(y<=0)),0)
    imax <- which.max(y)

    # Indices of part to keep
    iBegin <- which(di==-1 & i<(i[imax]))
    iBegin <- tail(iBegin, n=1)[1]

    iEnd  <- which(di==1 & i>(i[imax]))[1]
    # Corrections of Indices
    if (is.na(iBegin)) iBegin <- 1 else iBegin <- iBegin + 1
    if (is.na(iEnd))     iEnd <- len
    # New y
    yNEW <- rep_len(0, len)
    yNEW[iBegin:iEnd] <- y[iBegin:iEnd]
    return(yNEW)
}

#' @rdname uniPeak
#' @export
unipeak <- function(y){uniPeak(y)}
