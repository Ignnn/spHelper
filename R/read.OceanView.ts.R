#' [!v0.1] Read OceanView timeseries spectra
#'
#' Read contents of spectroscopic timeseries data file to
#' \code{\link[=hyperSpec-class]{hyperSpec}} object. File is created by
#'  software `OceanView 1.5`.
#'
#' @details In \code{read.OceanView.ts} the default decimal symbol is dot (\code{.}).\cr
#'          In \code{read.OceanView.ts2} - \emph{comma} (\code{,}).
#'
#'
#' @param file The name of the file which the data are to be read from.
#' @param dec The character used in the file for decimal points.
#' @param n The number of file lines to scan to search for indicator of
#'         spectral data beginning.
#' @param indicator The text, that indicates the last line of the header
#'        lines and the beggining of spectroscopic information.
#'         Default is \code{">>>>>Begin Spectral Data<<<<<"}.
#'
#' @return A \code{\link[=hyperSpec-class]{hyperSpec}} object with technical and
#'         spectroscopic information from file \code{file}.
#' @export
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#' read.OceanView.ts("Spectra.txt")
#' read.OceanView.ts("Spectra.txt", dec = ",")
#' read.OceanView.ts2("Spectra.txt")
#'
#' # Read several files to one `hyperspec` object:
#'
#' Files   <- dir()[1:4]                         # 4 files are sellected
#' sp_list <- lapply(Files, read.OceanView.ts2)  # Makes a list of objects
#' sp      <- collapse(sp_list)                  # Makes one object
#'
#' plotmat(sp)
#' }}
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
read.OceanView.ts <- function(file, dec = '.', n = 17,
                              indicator = ">>>>>Begin Spectral Data<<<<<") {


    header <-  read.OceanView.header(file, dec = dec, n = n, indicator = indicator)

    # Select correct label for  wavelength ------------------------------------
    # Here may be a bug if `wl_mode == NULL`
    wl_mode <- as.character(header$data$XAxis_mode[1])
    wl_label <- switch(wl_mode,
                       Wavelengths = expression(list(lambda, nm)),
                       wl_mode)  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Incomplete: other x-axis modes are needed
    # Read wavelengths ---------------------------------------------------------
    wl_x <- read.table(file,
                       header = F,
                       skip = header$last_line,
                       nrows = 1,
                       dec = ",") %>%
        as.numeric()

    # Read spectra  ---------------------------------------------------------
    y      <- read.table(file, header = FALSE,
                         skip = header$last_line + 1,
                         dec = dec,
                         stringsAsFactors = FALSE)
    sp_y   <- as.matrix(y[,-(1:2)])

    #  Extract Time and Date --------------------------------------------------

    # time      <- chron::times(y[,1],format = 'h:m:s')
    timestamp <- y[,2]
    datetime  <- as.POSIXct(as.numeric(timestamp)/1000,
                            origin = '1970-01-01',
                            tz = 'EET')
    t <-  (timestamp - timestamp[1]) / 1000 # time, seconds

    # Construct a data frame for non-spectroscopic information -----------------
    data <- data.frame(FileName = file, datetime = datetime, t = t)
    data <- merge(header$data,data)

    # Create a spectra object --------------------------------------------------
    sp <- new('hyperSpec', spc = sp_y, wavelength = wl_x, data = data,
              label = list(spc = "I, a.u.",
                           .wavelength = wl_label, # Fluorescencijai
                           FileName = "File name",
                           t = "t, s",
                           datetime = "Date and Time"))

    # ==========================================================================
    return(sp)
    # ==========================================================================
}

#' @rdname read.OceanView.ts
#' @export
read.OceanView.ts2 <- function(file, dec = ',', n = 17,
                               indicator = ">>>>>Begin Spectral Data<<<<<")
    {read.OceanView.ts(file, dec, n, indicator)}
