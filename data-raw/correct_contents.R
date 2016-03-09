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


# Function 1 --------------------------------------------------------------

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
    # x <- gsub("qplot_spDiff",   "plot_spDiff", x, perl = TRUE)
    # x <- gsub("foldsTets",      "foldTests", x, perl = TRUE)
    # x <- gsub("listFunctions",  "list.functions", x, perl = TRUE)
    # x <- gsub("makeFirstCapital",  "make.firstCapitals", x, perl = TRUE)
    # x <- gsub("qplot_stat",  "qplolt_spStat", x, perl = TRUE)


    # Writte
    writeLines(x, con = FILE)

    # print(x)
}

# Function 2 --------------------------------------------------------------

apply_content_corrections <- function(x){
    Start <-  Sys.time()
    setwd(x)

    AllFiles <- dir()
    FILES <- as.list(AllFiles[grepl("(.*\\.R$)|(.*\\.Rmd$)|(.*\\.html$)",AllFiles)])

    lapply(FILES, correct_contents2)

    shell.exec(getwd())
    printDuration(Start,returnString = TRUE)
}

# Function 3 --------------------------------------------------------------

require(spHelper)

# List all directories of interest
directories  <- as.list(
    c(paste0('D:\\Dokumentai\\R\\spHelper\\',
             c("R","vignettes","inst\\doc"),
             "\\"),
      'D:\\Dokumentai\\R\\Spektroskopija\\PAP_PD_2014\\')
)

# Apply corrections
stop("This script can be harmful")
lapply(directories, apply_content_corrections)


# setwd('D:\\Dokumentai\\R\\spHelper\\')
