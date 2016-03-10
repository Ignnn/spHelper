#' [!] Read header lines of OceanView file
#'
#' Read header lines with non-spectroscopic information of OceanView
#' (version 1.5) file.
#'
#' @param file The name of the file which header lines are to be read from.
#' @param dec The character used in the file for decimal points. Default is
#' a point (\code{.}).
#' @param n Number of lines to be read. Default is 17.
#' @param indicator The text, that indicates the last line of the header
#'        lines and the beggining of spectroscopic information.
#'         Default is \code{">>>>>Begin Spectral Data<<<<<"}.
#' @param text Text of header as if it was read with function
#'        \code{\link[base]{readLines}}. If \code{text} is provided, \code{file}
#'        is ignored.
#'
#' @return A list with 2 enties:
#' \enumerate{
#'      \item{$data}{A dataframe with information}
#'      \item{$last_line} {The number of the last header line}
#' }
#' @export
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#'  read.OceanView.header("MySpectra.txt")
#'  read.OceanView.header("MySpectra.txt")$data
#'
#' }}
#'
#' ## An example of header lines:
#'
#' #    Data from MySpectra.txt Node
#' #
#' #    Date: Fri Jan 15 16:15:16 GMT 2016
#' #    User: Scientist1
#' #    Spectrometer: USB2E2222
#' #    Autoset integration time: false
#' #    Trigger mode: 4
#' #    Integration Time (sec): 1.000000E1
#' #    Scans to average: 1
#' #    Electric dark correction enabled: true
#' #    Nonlinearity correction enabled: true
#' #    Boxcar width: 0
#' #    XAxis mode: Wavelengths
#' #    Stop averaging: false
#' #    Number of Pixels in Spectrum: 2048
#' #    >>>>>Begin Spectral Data<<<<<
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

read.OceanView.header <- function(file = NULL,  dec = ".", n = 17,
                                  indicator = ">>>>>Begin Spectral Data<<<<<",
                                  text) {

    # Read file line by line --------------------------------------------------
    if (missing(text))   text  <- readLines(file, n = n)

    # Extract information from Header lines of file ---------------------------
    last_line <- grep(indicator, text)
    if (length(last_line) > 1)
        stop("`text.in.last.line` is ambiguous an is present in several lines.")

    if (length(last_line) == 1) {
        header    <- paste(text[1:last_line], collapse = "")
        if (dec != ".") {header <- gsub(dec, '.', header)}
        # Parse header ------------------------------------------------------------
        pattern = paste0(
            'Data from (?<Data_from>.*) Node.*',
            'Date: (?<Beggin_at>.*)',
            'User: (?<User>.*)',
            'Spectrometer: (?<Spectrometer>.*)',
            'Autoset integration time: (?<Autoset_integration_time>.*)',
            'Trigger mode: (?<Trigger_mode>.*)',
            'Integration Time \\((?<Integration_time_Units>.*)\\): ',
            '(?<Integration_time>.*)',
            'Scans to average: (?<Scans_averaged>.*)',
            'Electric dark correction enabled: (?<Electric_dark_correction>.*)',
            'Nonlinearity correction enabled: (?<Nonlinearity_correction>.*)',
            'Boxcar width: (?<Boxcar_width>.*)',
            'XAxis mode: (?<XAxis_mode>.*)',
            'Stop averaging: (?<Stop_averaging>.*)',
            'Number of Pixels in Spectrum: (?<N_Pixels_in_Spectrum>.*)',
            '>>>>>Begin Spectral Data<<<<<'
        )
        # Sort variables ----------------------------------------------------------
        header_data <- regexp2df(header, pattern)[,c(3,4,1,2,5,6,8,7,10,9,11:15)]
        # Change class or variables -----------------------------------------------
        NumVar <- c("Trigger_mode",
                    "Integration_time",
                    "Scans_averaged",
                    "Boxcar_width",
                    "N_Pixels_in_Spectrum")

        LogVar <- c("Autoset_integration_time",
                    "Electric_dark_correction",
                    "Nonlinearity_correction",
                    "Stop_averaging")

        header_data[,NumVar] <- mapply(as.numeric, header_data[,NumVar])
        header_data[,LogVar] <- mapply(as.logical, header_data[,LogVar])
    # If no header lines were found
    } else {
        last_line = 0
        header_data = data.frame()
        warning("No header lines were read. Value of parameter `n` may be incorrect")
    }
    # Output ------------------------------------------------------------------
    return(list(data = header_data, last_line = last_line))
}
