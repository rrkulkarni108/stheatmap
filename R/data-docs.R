#' Drought Dataset
#'
#' Drought is the variable name of the DataFrame in Drought.rda file
#' @docType data
#' @usage data(Drought)
#' @format A data frame with 61828 rows and 12 columns:
#' \describe{
#'   \item{MapDate}{Date in form YYYYMMDD}
#'   \item{FIPS}{County identifier}
#'   \item{County}{County name}
#'   \item{State}{State abbreviation}
#'   \item{None}{Drought Severity Category 1}
#'   \item{D0}{Drought Severity Category 2}
#'   \item{D1}{Drought Severity Category 3}
#'   \item{D2}{Drought Severity Category 4}
#'   \item{D3}{Drought Severity Category 5}
#'   \item{D4}{Drought Severity Category 6}
#'   \item{ValidEnd}{End Drought Date in YYYY-MM-DD}
#'   \item{ValidStart}{Start Drought Date in YYYY-MM-DD}
#' }
#' @source US Drought Monitor, dataset names for the same object as Drought.rda.
"Drought"

#' ENSO Dataset
#'
#' Time series ENSO indices with monthly values.
#' @docType data
#' @usage data(ENSO)
#' @format A data frame with 72 rows and 13 columns:
#' \describe{
#'   \item{Season}{Season is a two year range: YYYY-YYYY}
#'   \item{JJA}{Oceanic Niño Index value (numeric) for June July August }
#'   \item{JAS}{Oceanic Niño Index value (numeric) for July August Sept}
#'   \item{ASO}{Oceanic Niño Index value (numeric) for August Sept Oct }
#'   \item{SON}{Oceanic Niño Index value (numeric) for Sept Oct Nov}
#'   \item{OND}{Oceanic Niño Index value (numeric) for Oct Nov Dec }
#'   \item{NDJ}{Oceanic Niño Index value (numeric) for Nov Dec Jan }
#'   \item{DJF}{Oceanic Niño Index value (numeric) for Dec Jan Feb}
#'   \item{JFM}{Oceanic Niño Index value (numeric) for Jan Feb Mar }
#'   \item{FMA}{Oceanic Niño Index value (numeric) for Feb Mar Apr}
#'   \item{MAM}{Oceanic Niño Index value (numeric) for Mar Apr May }
#'   \item{AMJ}{Oceanic Niño Index value (numeric) for Apr May Jun}
#'   \item{MJJ}{Oceanic Niño Index value (numeric) for May Jun Jul }
#' }
#' @source NOAA
"ENSO"
