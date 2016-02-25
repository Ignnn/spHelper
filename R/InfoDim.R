#'
#' Calculate information dimension of a matix
#'
#' The function calculates a measure, called "information dimension".
#'
#'
#' @param  Matrix - data matrix (rows = observations, columns = variables)
#'
#' @return A list with fields:
#'  \describe{
#'  \item{$dim }{Information dimension ,rounded towards positive infinitive}
#'  \item{$exactDim  }{Information dimension (fractional, not rounded)}
#'  \item{$explained  }{A vector of eigenvalues, normalized by sum of eigenvalues,
#'   which can be used to determine the importance of (principal) components}
#'  \item{$eigenvalues }{A vector of eigenvalues}
#'  \item{$n.comp }{A vector with integers from 1 to length(eigenvalues)}
#' }
#'
#' @references [1]	R. Cangelosi and A. Goriely, Component retention in principal
#'       component analysis with application to cDNA microarray data.
#'       Biol Direct, 2, 2 (2007), \url{http://dx.doi.org/10.1186/1745-6150-2-2}
#'
#' @note
#' [LT] Pries pradedant vykdyti operacija, svarbus zingsnis pasirinkti tinkama
#' normavimo buda. To nepadarius gausime klaidinga atsakyma. \cr
#' sp = sp_normuok(sp,x,'1',495);
#'
#' Taip pat labai svarbus ir triuksmo lygis. Didejant triuksmui atitinkamai
#' padidinamas maksimalus dimensija skaicius.
#'
#'
#' @note
#' eigenvalues - Singular values
#' pk - tikimybines dimensiju vertes, skirtos entropijos ivertinimui.
#' explain = pk;
#'
#' @seealso InfoDim_plot
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
InfoDim <- function(Matrix){
    eigenval   <- svd(Matrix)$d
    explain    <- eigenval / sum(eigenval);
    exact_dim   <- prod(sapply(explain,function(x){x ^ -x}));
    dim         <- ceiling(exact_dim);    # % Round towards infinitive

    output <- list(      dim   = dim,
                         exactDim   = exact_dim,
                         explained   = explain,
                         eigenvalues = eigenval,
                         n.comp      = 1:length(explain))
    return(output)
}
