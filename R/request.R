#' Set Tushare Pro API token
#'
#' @param token a character vector
#'
#' @return token itself, invisibly.
#' @export
#'
#' @examples
#' SetToken("YOUR_API_TOKEN")
SetToken <- function(token) {

  tus.globals$api_token <- as.character(token)

  invisible(tus.globals$api_token)
}

GetToken <- function() {

  if (is.null(tus.globals$api_token)) {
    stop("API token is not set. Please run SetToken('OUR_API_TOKEN') first.")
  }

  tus.globals$api_token
}

#' Make raw request to Tushare Pro API
#'
#' @param api_name name of API function, please refer to online document for more information.
#' @param ... passed to API function.
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' top10 <- TusRequest("top10_holders", ts_code = "000001.SZ")
#' }
TusRequest <- function(api_name, ...) {

  api_url <- "http://api.tushare.pro"

  args <- list(
    token = GetToken(),
    api_name = api_name,
    params = list(...)
  )
  req <- httr::content(
    httr::POST(api_url, body = args, encode = "json"),
    encoding = "UTF-8",
    as = "parsed",
    type = "application/json"
  )
  if (is.null(req$data)) {
    stop(req$msg)
  }

  if (length(req$data$items)) {
    dt <- tryCatch({
      data.table::rbindlist(req$data$items)
    }, error = function(e) {
      #error happens when null is parsed by fromJSON()
      #traverse req$date$items up to depth 2
      tmp <- lapply(req$data$items, function(row) {
        lapply(row, function(x) if (is.null(x)) NA else x)
      })
      data.table::rbindlist(tmp)
    })
  } else {
    #create an empty data.table
    dt <- do.call(data.table::data.table, lapply(req$data$fields, function(x) logical()))
  }
  data.table::setnames(dt, unlist(req$data$fields))

  dt
}
