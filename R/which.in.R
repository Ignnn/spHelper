#' @name which.in
#' @aliases which.in
#' @aliases which.in.diag
#' @aliases which.in.offdiag
#' @aliases which.in.col
#' @aliases which.in.row
#' @aliases which.in.trilow
#' @aliases which.in.triupp
#'
#' @title Indices of certain matrix elements
#'
#' @description Calculate indices of certain (e.g., diagonal) matrix elements.
#' These elements can be in:
#' \enumerate{
#'  \item{diag}{diagonal;}
#'  \item{offdiag}{offdiagonal;}
#'  \item{col}{certain columns;}
#'  \item{row}{certain rows;}
#'  \item{trilow}{lower triangle;}
#'  \item{triupp}{upper triangle.}
#' }
#'
#' @param x A matrix or an object, which can be coerced to a marix.
#' @param type (just for \code{which.in}) Either \code{diag}, \code{offdiag},
#'  \code{col}, \code{row}, \code{trilow} or \code{triupp}.
#' @param ... Other appropriate parameters, as indicated below.
#'
#' @return Indices of indicated* elements of a matrix (*that belong to either
#'         diagonal, offdiagonal, upper/lower triange of matrix, certain row(s)
#'         or column(s)).
#' @export
#'
#' @examples
#'
#' m1 <- matrix(NA, 5, 5)
#' which.in.diag(m1)
#'  ## [1]  1  7 13 19 25
#'
#' m2 <- matrix(NA, 5, 2)
#' which.in.diag(m2)
#'  ## [1]  1  7
#'
#' #================================
#'
#'  which.in(diag, m1)
#'
#' @family Matrix operations


which.in <- function(type, x,  ...){
    type <- as.character(match.call()$type)
    ind  <- switch(type,
                 diag    = which.in.diag(x, ...) ,
                 offdiag = which.in.offdiag(x, ...) ,
                 col     = which.in.col(x, ...) ,
                 row     = which.in.row(x, ...) ,
                 trilow  = which.in.trilow(x, ...) ,
                 triupp  = which.in.(x, ...)
    )
    return(ind)
}

# ========================================================================
#' @rdname which.in
#' @export
which.in.diag <- function(x){
    m      <- ind.matrix(x)
    ind.d  <- diag(m)
    return(ind.d)
}

# ========================================================================
#' @rdname which.in
#' @export
which.in.offdiag <- function(x){
    m      <- ind.matrix(x)
    ind.d  <- diag(m)
    ind.o  <- setdiff(m, ind.d)
    return(ind.o)
}


# ========================================================================
#' @rdname which.in
#' @export
#' @param col An index/Indices of columns that contain elements of interest.
which.in.col <- function(x, col){
    m      <- ind.matrix(x)
    ind.c  <- as.vector(m[,col])
    return(ind.c)
}

# ========================================================================
#' @rdname which.in
#' @export
#' @param row An index/Indices of rows that contain elements of interest.
which.in.row <- function(x, row){
    m      <- ind.matrix(x)
    ind.r  <- as.vector(m[row,])
    return(ind.r)
}

# ========================================================================
#' @rdname which.in
#' @export
#' @inheritParams base::lower.tri
which.in.trilow <- function(x, diag = FALSE){
    m      <- ind.matrix(x)
    ind.l  <- m[lower.tri(m)]
    return(ind.l)
}

# ========================================================================
#' @rdname which.in
#' @export
which.in.triupp <- function(x, diag = FALSE){
    m      <- ind.matrix(x)
    ind.u  <- m[upper.tri(m)]
    return(ind.u)
}

# ========================================================================
# [Internal function]
# ind.matrix()
#
# Genereta a matrix of the same size as `x`, where each element represents its
# index.
#
ind.matrix <- function(x){
    x      <- as.matrix(x)
    dims   <- dim(x)
    m      <- matrix(1:length(x),nrow = dims[1],ncol = dims[2])
    return(m)
}
