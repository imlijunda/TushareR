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

#' Wrapped httr::POST with retry for timeout error.
#'
#' Notes on TimedPOST: non-timeout errors are rethrown
#'
#' @param ... passed to httr::POST()
#' @param retry max retries
#'
#' @return response
#'
TimedPOST <- function(..., retry) {

  args <- list(...)

  flag <- FALSE
  for (i in seq_len(retry + 1L)) {
    ans <- tryCatch({
      do.call(httr::POST, args)
    }, error = function(e) {
      if (grepl("Timeout", e$message, fixed = TRUE)) {
        NULL
      } else {
        #Not timeout error, throw e
        stop(e)
      }
    })
    if (!is.null(ans)) {
      flag <- TRUE
      break
    }
  }

  if (flag) {
    ans
  } else {
    stop(sprintf("Timeouts were reached after %d retries.", retry))
  }
}

#' Make raw request to Tushare Pro API
#'
#' @param api_name name of API function, please refer to online document for more information.
#' @param ... passed to API function.
#' @param timeout timeout in seconds for httr request.
#' @param retry max retries if timeout.
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' top10 <- TusRequest("top10_holders", ts_code = "000001.SZ")
#' }
TusRequest <- function(api_name, ..., timeout = 10.0, retry = 5L) {

  api_url <- "http://api.waditu.com"

  args <- list(
    token = GetToken(),
    api_name = api_name,
    params = list(...)
  )

  req <- TimedPOST(url = api_url,
                   config = httr::timeout(timeout),
                   body = args,
                   encode = "json",
                   retry = retry)
  res <- httr::content(
    req,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  if (is.null(res$data)) {
    stop(res$msg)
  }

  if (length(res$data$items)) {
    dt <- tryCatch({
      data.table::rbindlist(res$data$items)
    }, error = function(e) {
      #error happens when null is parsed by fromJSON()
      #traverse res$date$items to depth 1 to remove NULL rows
      null_row <- sapply(res$data$items, is.null)
      #traverse res$date$items to depth 2 to replace NULL cells to NAs
      tmp <- lapply(res$data$items[!null_row], function(row) {
        lapply(row, function(x) if (is.null(x)) NA else x)
      })
      #TODO performance: since the NULL replacements are not done in-place/by-ref,
      #objects are copied and altered every loop thus slow. However, considering
      #most time are spent in httr::POST and res$data is usually only a few hundred
      #kilobytes this is not a big issue ATM.
      data.table::rbindlist(tmp)
    })
  } else {
    #create an empty data.table
    dt <- do.call(data.table::data.table, lapply(res$data$fields, function(x) logical()))
  }
  data.table::setnames(dt, unlist(res$data$fields))

  dt
}
