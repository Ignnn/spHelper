# [tags] Rename, Correct contents
#
#
correct_contents <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x  <- x0
    # Correct
    # x <- gsub("(family functions for \pkg{hyperSpec})|(family functions for \pkg{hyperSpec})",
    #           "family functions for \\\\pkg{hyperSpec}", x, perl = TRUE)

    # x <- gsub("family \\\\pkg\\{spHelper\\} functions for \\\\code\\{hyperSpec\\}",
    #           "family \\\\pkg{spHelper} functions for \\\\pkg{hyperSpec}",
    #           x, perl = TRUE)
    #
    # x <- gsub("family functions for \\\\pkg\\{hyperSpec\\}",
    #           "family \\\\pkg{spHelper} functions for \\\\pkg{hyperSpec}",
    #           x, perl = TRUE)
#
#     x <- gsub("family \\\\pkg\\{spHelper\\} functions for \\\\pkg\\{hyperSpec\\}",
#               "family \\\\pkg{spHelper} functions for spectroscopy and \\\\pkg{hyperSpec}",
#               x, perl = TRUE)



        # x <- gsub("family component analysis / factorisation related functions",
        #           "family component analysis / factorisation related functions in \\\\pkg{spHelper}",
        #           x, perl = TRUE)
        #
        # x <- gsub("family matrix operations",
        #           "family matrix operations in \\\\pkg{spHelper}",
        #           x, perl = TRUE)
        #
        # x <- gsub("family simmulation functions",
        #           "family simmulation functions in \\\\pkg{spHelper}",
        #           x, perl = TRUE)
        #
        # x <- gsub("family curves",
        #           "family curves in\\\\pkg{spHelper}",
        #           x, perl = TRUE)

    # x <- gsub("(family functions for \pkg{hyperSpec})|(family functions for \pkg{hyperSpec})",
    #           "family functions for \\\\pkg{hyperSpec}", x, perl = TRUE)
    x <- gsub("family family",
              "family", x, perl = TRUE)
    # Writte

    if (any(x0 != x))    {
        cat(FILE,sep = "\n")
        writeLines(x, con = FILE)
    }

    # print(x)
}


# Function 1 --------------------------------------------------------------

correct_contents2 <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x <- x0

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

    # x <- gsub("addLabels_",  "hyAdd.Labels_", x, perl = TRUE)

    # x <- gsub("sortMaxOnDiag",  "sortDescOnDiag", x, perl = TRUE)
    #
    x <- gsub("plot_spFilt",  "plot_spCompare", x, perl = TRUE)




    # Writte
    if (any(x0 != x))    { cat(FILE,sep = "\n"); writeLines(x, con = FILE) }
    # print(x2)
}

# Function 2 --------------------------------------------------------------

apply_content_corrections <- function(x){
    Start <-  Sys.time()
    setwd(x)

    AllFiles <- dir()
    FILES <- as.list(AllFiles[grepl("(.*\\.R$)|(.*\\.Rmd$)|(.*\\.html$)",AllFiles)])

    # lapply(FILES, correct_contents)
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
      "D:\\Dokumentai\\R\\spHelper\\",
      paste0("D:\\Dokumentai\\R\\Spektroskopija\\",
             c("PAP_PD_2014\\",
               "PAP_RK_2014\\",
               "TD_2009\\"))


      # 'D:\\Dokumentai\\R\\Spektroskopija\\__ TO DELETE __\\',
      # 'D:\\Dokumentai\\R\\Spektroskopija\\TD_2009\\',
      # 'D:\\Dokumentai\\R\\Spektroskopija\\'
      )
)

# Apply corrections
stop("This script can be harmful!!!")
lapply(directories, apply_content_corrections)


# setwd('D:\\Dokumentai\\R\\spHelper\\')
