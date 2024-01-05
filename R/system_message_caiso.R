system_message_caiso <- function(
    from_date = Sys.Date()-14,
    to_date = Sys.Date()+1,
    message_severity = c("ALL", "Emergency", "Urgent", "Normal"),
    resultformat = c("6", "5"),
    base_url = "http://oasis.caiso.com/oasisapi",
    tz_hrs_from_gmt = "0700",
    api_version = "1"
){
  # Format start and end datetime for api
  msg_start_datetime <- paste0(
    str_remove_all(from_date, pattern = "-"),
    "T00:00-",
    tz_hrs_from_gmt
  )

  msg_end_datetime <- paste0(
    str_remove_all(to_date, pattern = "-"),
    "T00:00-",
    tz_hrs_from_gmt
  )

  # Form api request
  req <- request(base_url) |>
    req_url_path_append("SingleZip") |>
    req_url_query(
      queryname = "ATL_OSM",
      msg_severity = message_severity,
      startdatetime = msg_start_datetime,
      enddatetime = msg_end_datetime,
      version = api_version
    )

  # Query API
  resp <- req_perform(req)

  # Extract (unzip csv) and load results

  # Extract CSV Format
  # else return the response
  if(resultformat == "6") {
    temp_file <- tempfile() # create temp file
    writeBin(resp_body_raw(resp), temp_file) # unzip to temp file
    df <- read_csv(temp_file) # read temporary file
    file.remove(temp_file) # remove temp file
    return(df)
  } else {
    return(resp)
  }

}
