#' [!v0.1] Read OceanView timeseries spectra
#'
#' Read contents of spectroscopic timeseries data file to
#' \code{\link[=hyperSpec-class]{hyperSpec}} object. File is created by
#'  software `OceanView 1.5`.
#'
#' @param file The name of the file which the data are to be read from.
#' @param dec The character used in the file for decimal points.
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
#'
#' # Read several files to one hyperspec object:
#'
#' Files   <- dir()[1:4]
#' sp_list <- lapply(Files, read.OceanView.ts2) # A list of objects
#' sp     <- collapse(sp_list)
#' plotmat(sp)
#' }}
#'
#' @family functions for \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
read.OceanView.ts <- function(file, dec = '.') {
    # Read file line by line --------------------------------------------------
    text     <- readLines(file, n = 20)
    # Extract information hrom Header -----------------------------------------
    head_end <- which(text == ">>>>>Begin Spectral Data<<<<<")
    header   <- paste(text[1:head_end], collapse = "")
    if (dec == ",") {header <- gsub(dec, '.', header)}

    header <- gsub('true',  'TRUE',  header)
    header <- gsub('false', 'FALSE', header)

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
    header_data <- regexp2df(header, pattern)[,c(3,4,1,2,5:7,8,10,9,11:15)]

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

    # Read wavelengths ---------------------------------------------------------
    wl_x <- read.table(file,
                       header = F,
                       skip = head_end,
                       nrows = 1,
                       dec = ",") %>%
        as.numeric()
    # Select correct label for  wavelength ------------------------------------
    wl_mode <- as.character(header_data$XAxis_mode[1])
    wl_label <- switch(wl_mode,
                       Wavelengths = expression(lambda, ", nm"),
                       wl_mode)  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Incomplete: other modes are needed

    # Read spectra  ---------------------------------------------------------
    y      <- read.table(file, header = F,
                         skip = head_end + 1,
                         dec = dec,
                         stringsAsFactors = F)
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
    data <- merge(header_data,data)

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
read.OceanView.ts2 <- function(file, dec = ',') {read.OceanView.ts(file, dec)}
