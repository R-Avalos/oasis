#' Get CAISO System Messages
#'
#' @param from_date A date value defining how far to look back. 31 day window limit.
#' @param to_date A date value defining the end date of the look back period. 31 day window limit.
#' @param message_severity A character value defining message type (ALL, Emergency, urgent, Normal).
#' @param resultformat A character value defining what type of results to return (csv, xml).
#' @param base_url A character value of Oasis api site.
#' @param tz_hrs_from_gmt A numeric value of hours from GMT
#' @param api_version A numeric value of the OASIS API version
#'
#' @return A dataframe of system messages
#' @export
#'
#' @examples
#' df <- system_message_caiso()
#' df <- system_message_caiso(
#'   from_date = as.Date("2024-03-01"),
#'   to_date = as.Date("2024-03-07"),
#'   message_severity = "Emergency"
#' )

system_message_caiso <- function(
    from_date = Sys.Date()-14,
    to_date = Sys.Date()+1,
    message_severity = c("ALL", "Emergency", "Urgent", "Normal"),
    resultformat = c("csv", "xml"),
    base_url = "http://oasis.caiso.com/oasisapi",
    tz_hrs_from_gmt = 7,
    api_version = 1
){
  # Input value checks
  # Check if message severity is correctly defined
  message_severity <- match.arg(message_severity)
  # Change result format to Oasis encoding
  if(match.arg(resultformat)=="csv"){
    resultformat = 6
  } else {
    resultformat = 5
  }
  # Test that number of days is within Oasis API limit
  if(as.numeric(to_date-from_date) > 30) usethis::ui_stop("Date window exceeds CAISO limit. CAISO System Messages may be requested for a max window of 31 days")
  # test date inputs
  if(lubridate::is.Date(from_date) == FALSE) usethis::ui_stop("Please provide Date value for from_date")
  if(lubridate::is.Date(to_date) == FALSE) usethis::ui_stop("Please provide Date value for to_date")
  if(tz_hrs_from_gmt > 23) usethis::ui_stop("TZ hrs from GMT cannot be greater than 23")
  if(is.na(as.numeric(api_version))==TRUE) usethis::ui_stop("Non-numeric for OASIS API version provided")

  # Format TZ hours to Oasis API format
  tz_hrs_from_gmt <- stringr::str_pad(stringr::str_pad(tz_hrs_from_gmt, width = 2, side = "left", pad = "0"), width = 4, side = "right", pad = "0")
  # Format start and end datetime for api
  msg_start_datetime <- paste0(
    stringr::str_remove_all(from_date, pattern = "-"),
    "T00:00-",
    tz_hrs_from_gmt
  )
  msg_end_datetime <- paste0(
    stringr::str_remove_all(to_date, pattern = "-"),
    "T00:00-",
    tz_hrs_from_gmt
  )


  # Form api request
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("SingleZip") |>
    httr2::req_url_query(
      queryname = "ATL_OSM",
      msg_severity = message_severity,
      startdatetime = msg_start_datetime,
      enddatetime = msg_end_datetime,
      version = as.character(api_version),
      resultformat = resultformat
    ) |>
    httr2::req_throttle(rate = 10/60) # throttle 10 requests per minute

  # Query API
  resp <- httr2::req_perform(req)

  # Return results
  if(resultformat == 6) {
    # Extract and load CSV Format
    temp_file <- tempfile() # create temp file
    writeBin(httr2::resp_body_raw(resp), temp_file) # unzip to temp file
    df <- readr::read_csv(
      temp_file,
      col_names = TRUE,
      col_types = cols(
        MSG_ID = col_double(),
        MSG_TIME = col_datetime(format = ""),
        MSG_TIMESTAMP = col_datetime(format = ""),
        MSG_SEVERITY = col_character(),
        MSG_SCID = col_logical(),
        MSG_TEXT = col_character(),
        TIMESTAMP = col_datetime(format = ""),
        MSG_TIME_GMT = col_datetime(format = ""),
        MSG_TIMESTAMPE_GMT = col_datetime(format = ""),
        TIMESTAMP_GMT = col_datetime(format = ""))
      ) # read temporary file
    file.remove(temp_file) # remove temp file
    return(df)
  } else {
    # Return XML result buried in response content
    return(resp)
  }
}
