# ***** Parse input of function ***** -------------------------------------

#
#
# #' [!] Parse function's arguments: check if \code{variable} is inside \code{data} and return values
# #'
# #' Parse function's arguments: Check if \code{variable} is inside \code{data}
# #'  and return values of either \code{data[,"variable"]} (if TRUE) or
# #'  \code{variable} (if FALSE)
# #'
# #' @use Use inside of a function.
# #'
# #' @param data
# #' @param variable
# #'
# #' @return
# #' @export
# #'
# #' @examples
# varValues <-  function(data, variable) {
#     varName <- as.character(match.call()$variable)
#     output  <- if (varName %in% ls(data)) data[,varName] else variable
#     return(output)
# }


