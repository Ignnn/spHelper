# ============================================================================
#' @rdname qplot_confusion
#' @template same
#' @export

qplot_confusion2 <- function(Prediction, Reference,...){
    if (length(Prediction) != length(Reference)) {
        stop("Lengths of vectors `Prediction` and `Reference` must be equal.")
    }
    conf <- table(Prediction,Reference)
    qplot_confusion(conf = conf, ...)
}

