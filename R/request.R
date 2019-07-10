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

#' Wrapped httr::POST with retry. Rethrow errors as warnings.
#'
#' @param ... passed to httr::POST()
#' @param retry max retries
#' @param sleep seconds to sleep if error occurs
#' @param warn whether to throw warnings if error occurs
#'
#' @return reponse
#'
#' @examples
POSTRetry <- function(..., retry, sleep = 0, warn = TRUE) {

  args <- list(...)

  flag <- FALSE
  for (i in seq_len(retry)) {
    ans <- tryCatch({
      do.call(httr::POST, args)
    }, error = function(e) {
      if (warn) {
        warning(e$message)
      }
      Sys.sleep(sleep)
      NULL
    })
    if (!is.null(ans)) {
      flag <- TRUE
      break
    }
  }

  if (flag) {
    ans
  } else {
    stop(sprintf("Failed to POST after %d attempts.", retry))
  }
}

#' Make raw request to Tushare Pro API
#'
#' @param api_name name of API function, please refer to online document for more information.
#' @param ... passed to API function.
#' @param timeout timeout in seconds for httr request.
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' top10 <- TusRequest("top10_holders", ts_code = "000001.SZ")
#' }
TusRequest <- function(api_name, ..., timeout = 5.0) {

  api_url <- "http://api.waditu.com"

  args <- list(
    token = GetToken(),
    api_name = api_name,
    params = list(...)
  )

  req <- POSTRetry(url = api_url,
                   config = httr::timeout(timeout),
                   body = args,
                   encode = "json",
                   #retry settings
                   retry = 3L, sleep = 2.0, warn = TRUE)
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
      #TODO performance opt
      data.table::rbindlist(tmp)
    })
  } else {
    #create an empty data.table
    dt <- do.call(data.table::data.table, lapply(res$data$fields, function(x) logical()))
  }
  data.table::setnames(dt, unlist(res$data$fields))

  dt
}
