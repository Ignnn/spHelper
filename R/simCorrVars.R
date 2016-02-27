#' [!!!] Simulate correlated continuous data from standard normal distribution
#'
#' Simulate correlated continuaous data from standard normal distribution.
#'
#' @param corrMat A symmetricmatrix of correlation coefficients.
#' @param N A number of observations.
#'
#' @return A dataframe of correlated variables.
#'
#' @seealso \link{corr_vec2mat}, \link[MASS]{mvrnorm}
#' @export
#' @source \url{http://www.r-bloggers.com/simulating-random-multivariate-correlated-data-continuous-variables/}
#'
#' @examples
#'
#' # ------------------------------------------------------------
#' simCorrVars()
#' simCorrVars(N = 10)
#'
#' # ------------------------------------------------------------
#' # Generate 2 correlated variables with 10 observations
#'
#' corrMat <- matrix(c(1, .5, .5, 1), 2)
#' print(corrMat)
#' simCorrVars(corrMat, N = 10)
#'
#' # ------------------------------------------------------------
#'
#' @family simmulation functions
#'
simCorrVars <- function(corrMat, N = 100){
    if (missing(corrMat)) {
        corrMat = matrix(cbind(1,   0.8, 0.2,
                               0.8, 1  , 0.7,
                               0.2, 0.7, 1   ),
                         nrow = 3)
    }

    if (!isSymmetric(corrMat)) stop("Correlation matrix must be symmetric.")

    U = t(chol(corrMat))
    nvars = dim(U)[1]

    # set.seed(1)
    random.normal = matrix(rnorm(nvars*N,0,1), nrow = nvars, ncol = N);
    X = U %*% random.normal
    newX = t(X)
    as.data.frame(newX)

    # raw = as.data.frame(newX)
    # orig.raw = as.data.frame(t(random.normal))
    # names(raw) = c("response","predictor1","predictor2")
    # cor(raw)
    # plot(head(raw, 100))
    # plot(head(orig.raw,100))
}
