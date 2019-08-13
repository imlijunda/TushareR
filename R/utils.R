#Auxiliary functions

fix_date <- function(datetime) {
  if (is.character(datetime) && datetime != "") {
    datetime <- as.Date(datetime,
                        tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d"))
  }
  as.character(datetime, format = tus.globals$date_fmt)
}
fix_date_teleplay <- function(datetime) {
  if (is.character(datetime) && datetime != "") {
    datetime <- as.Date(datetime,
                        tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d"))
  }
  as.character(datetime, format = tus.globals$date_teleplay_fmt)
}
fix_time <- function(datetime) as.character(datetime, format = tus.globals$datetime_fmt)

fix_code <- function(code) toupper(code)

cast_date <- function(date) {
  switch(date,
         POSIXct = function(x) as.POSIXct(x,
                                          tz = "UTC",
                                          format = tus.globals$date_fmt),
         Date = function(x) as.Date(x,
                                    tz = "UTC",
                                    format = tus.globals$date_fmt),
         char = as.character
  )
}

cast_time <- function(time) {
  switch(time,
         POSIXct = function(x) as.POSIXct(x,
                                          tz = tus.globals$timezone,
                                          format = tus.globals$datetime_fmt),
         char = as.character
  )
}

cast_logical <- function(type) {
  switch(type,
         logical = cast_logical01,
         char = as.character)
}


cast_logical01 <- function(x) {

  ans <- vector(mode = "logical", length = length(x))
  if (is.character(x)) {
    idx <- x == "1" | x == "Y"
    ans[idx] <- TRUE
  } else {
    idx <- x != 0
    ans[idx] <- TRUE
  }

  ans
}
