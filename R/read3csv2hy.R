# Read 3 CSV spectra (from Matlab) to hyperSpec --------------------------------------------
#
#' [!] Read spectroscopic data from 3 CSV files ("data", "wavelengths" and "spectra") to hyperSpec object
#'
#' Read spectroscopic data from 3 CSV files ("data", "wavelengths" and
#'  "spectra") to hyperSpec object. These fileas are usually exported
#'  from Matlab.
#'
#' @note Data columns, that have unique values are removed.
#'
#' @param FileName_base - base of file name (a string). See more in section "Details"
#'
#' @return Object of class \code{\link[=hyperSpec-class]{hyperSpec}}
#'
#' @details
#' If base name is "TD_2009", then CSV files must be:
#' [1] "TD_2009 (wavelengths).csv"
#' [2] "TD_2009 (spectra).csv"
#' [3] "TD_2009 (data).csv"
#'
#' Base name can be a path, e.g.  "D:/spectra/TD_2009"
#'
#' @export
#'
#' @import hyperSpec
#' @importFrom  pander pander
#'
read3csv2hy <- function(FileName_base){
    # require(hyperSpec)

    requiredFiles <- paste0(FileName_base,
                            c(" (wavelengths)", " (spectra)", " (data)"),
                            '.csv')
    #  ------------------------------------------------------------------------
    fileIsFound <- basename(requiredFiles) %in% dir(dirname(requiredFiles))
    if (!all(fileIsFound))
        stop("Some of requitred files are not found in the working directory")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read wavelengths        # "(wavelengths).csv"
    wl_x   <- as.numeric(read.csv(requiredFiles[1],header = FALSE))
    # Read spectra            # "(spectra).csv"
    sp_y   <-  as.matrix(read.csv(requiredFiles[2],header = FALSE))
    # Read data               # "(data).csv"
    data   <-            read.csv(requiredFiles[3],sep = "|",
                                  na.strings = c("<netirta>","<undefined>","?")
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    findNonSingles <- function(x)(length(unique(x))>1)
    findSingles    <- function(x)(length(unique(x))==1)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    uniqueInfo <- unique(Filter(findSingles, data))
    message("Variables with constanant values are eliminated:")
    row.names(uniqueInfo)<- c("Value_of_eliminated_variable")
    message(pander::pander(t(uniqueInfo)))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data      <- Filter(findNonSingles, data)
    if ("ID" %in% data)   data$ID   <- as.factor(data$ID)

    #  Create hyperSpec object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sp <- new('hyperSpec', spc = sp_y, wavelength = wl_x, data = data)
    return(sp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
