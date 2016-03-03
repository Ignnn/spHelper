# [tags] Rename, Correct contents
# 
# 
correct_contents <- function(FILE){
    # Read
    x <- readLines(con = FILE)
    # Correct
    x <- gsub("(family functions for \pkg{hyperSpec})|(family functions for \pkg{hyperSpec})",
              "family functions for \\\\pkg{hyperSpec}", x, perl = TRUE)

    x <- gsub("family (spHelper)|(family \pkg{spHelper})|(\\\\code\\{spHelper\\})",
              "family \\\\pkg{spHelper}", x, perl = TRUE)
    
    # x <- gsub("family family",
    #           "family", x, perl = TRUE)
    # Writte
    writeLines(x, con = FILE)

    # print(x)
}
require(spHelper)
Start <-  Sys.time()
setwd('D:\\Dokumentai\\R\\spHelper\\data-raw\\')
AllFiles <- dir()
FILES <- as.list(AllFiles[grepl(".*\\.R$",AllFiles)])

lapply(FILES, correct_contents)

shell.exec(getwd())
printDuration(Start)
