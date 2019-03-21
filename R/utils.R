#Auxiliary functions

fix_date <- function(datetime) as.character(datetime, format = tus.globals$date_fmt)

fix_time <- function(datetime) {

  if ("POSIXt" %in% class(datetime)) {
    fixed <- as.character(datetime, format = tus.globals$datetime_fmt)
  } else {
    fixed <- as.character(datetime, format = tus.globals$date_fmt)
  }

  fixed
}

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
