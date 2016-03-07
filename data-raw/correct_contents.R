# [tags] Rename, Correct contents
#
#
# correct_contents <- function(FILE){
#     # Read
#     x <- readLines(con = FILE)
#     # Correct
#     x <- gsub("(family functions for \pkg{hyperSpec})|(family functions for \pkg{hyperSpec})",
#               "family functions for \\\\pkg{hyperSpec}", x, perl = TRUE)
#
#     x <- gsub("family (spHelper)|(family \pkg{spHelper})|(\\\\code\\{spHelper\\})",
#               "family \\\\pkg{spHelper}", x, perl = TRUE)
#
#     # x <- gsub("family family",
#     #           "family", x, perl = TRUE)
#     # Writte
#     writeLines(x, con = FILE)
#
#     # print(x)
# }

stop("This script can be harmful")
correct_contents2 <- function(FILE){
    # Read
    x <- readLines(con = FILE)
    # Correct
    # x <- gsub("plot_stat",      "qplot_stat", x, perl = TRUE)
    # x <- gsub("plot_confusion", "qplot_confusion", x, perl = TRUE)
    # x <- gsub("plot_infoDim",   "qplot_infoDim", x, perl = TRUE)
    # x <- gsub("plot_kAmp",      "qplot_kAmp", x, perl = TRUE)
    # x <- gsub("plot_kSpFacets", "qplot_kSpFacets", x, perl = TRUE)
    # x <- gsub("plot_kSp",       "qplot_kSp", x, perl = TRUE)
    # x <- gsub("plot_scores",    "qplot_scores", x, perl = TRUE)
    # x <- gsub("plot_sp",        "qplot_sp", x, perl = TRUE)

    # x <- gsub("qqplot_",        "qplot_", x, perl = TRUE)
    #
    # x <- gsub("qplot_spDiff",        "plot_spDiff", x, perl = TRUE)
    # x <- gsub("foldsTets",        "foldTests", x, perl = TRUE)
    # Writte
    writeLines(x, con = FILE)

    # print(x)
}

require(spHelper)
Start <-  Sys.time()
setwd('D:\\Dokumentai\\R\\spHelper\\R\\')
# setwd('D:\\Dokumentai\\R\\Spektroskopija\\PAP_PD_2014')
AllFiles <- dir()
FILES <- as.list(AllFiles[grepl(".*\\.R$",AllFiles)])

lapply(FILES, correct_contents2)

shell.exec(getwd())
printDuration(Start)
