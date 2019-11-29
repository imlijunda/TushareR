#' Set/Get Tushare Pro API token
#'
#' @param token a character vector
#'
#' @return token itself, invisibly.
#' @export
#'
#' @examples
#' SetToken("YOUR_API_TOKEN")
#' GetToken()
SetToken <- function(token) {

  tus.globals$api_token <- as.character(token)

  invisible(tus.globals$api_token)
}

#' @rdname SetToken
#' @export
GetToken <- function() {

  if (is.null(tus.globals$api_token)) {
    stop("API token is not set. Please run SetToken('OUR_API_TOKEN') first.")
  }

  tus.globals$api_token
}

#' Simple do.call retry wrapper
#'
#' @param what passed to do.call
#' @param args passed to do.call
#' @param quote passed to do.call
#' @param envir passed to do.call
#' @param attempt max number of attempts
#' @param sleep sleep time between attempts
#' @param warn whether to throw warning when error captured, or an error handling function
#'
#' @return The returned value from function call
do.retry <- function(what, args, quote = FALSE, envir = parent.frame(),
                     attempt = 3, sleep = 0, warn = TRUE) {

  flag <- FALSE
  if (is.logical(warn)) {
    err_func <- function(e) {
      if (warn) {
        warning(e$message, call. = FALSE)
      }
      flag <<- FALSE
      Sys.sleep(sleep)
    }
  } else {
    warn <- match.fun(warn)
    err_func <- function(e) {
      warn(e)
      flag <<- FALSE
      Sys.sleep(sleep)
    }
  }

  for (i in seq_len(attempt)) {
    flag <- TRUE
    ans <- tryCatch({
      do.call(what, args, quote, envir)
    }, error = err_func)
    if (flag) {
      return(ans)
    }
  }

  msg <- do.call(paste0, args = as.list(
    as.character(deparse(substitute(what)))
  ))
  stop(sprintf("Calling %s failed after %d attempts.", msg, attempt), call. = FALSE)
}

#' Make raw request to Tushare Pro API
#'
#' @param api_name name of API function, please refer to online document for more information.
#' @param ... passed to API function.
#' @param fields data fields to request
#' @param token API token.
#' @param timeout timeout in seconds for httr request.
#'
#' @return data.frame/data.table
#' @export
#'
#' @examples
#' \dontrun{
#' top10 <- TusRequest("top10_holders", ts_code = "000001.SZ")
#' }
TusRequest <- function(api_name, ..., fields = c(""), token = GetToken(), timeout = 10.0) {

  api_url <- "http://api.waditu.com"

  args <- list(
    token = token,
    api_name = api_name,
    params = list(...),
    fields = fields
  )

  post_args <- list(
    url = api_url,
    config = httr::timeout(timeout),
    body = args,
    encode = "json"
  )
  req <- do.retry(httr::POST, post_args, attempt = 3L, sleep = 0.5, warn = TRUE)
  res <- httr::content(req,
                       as = "parsed",
                       type = "application/json",
                       encoding = "UTF-8")

  if (is.null(res$data)) {
    stop(res$msg, call. = FALSE)
  }

  suppressWarnings({
    if (length(res$data$items)) {
      dt <- tryCatch({
        data.table::rbindlist(res$data$items)
      }, error = function(e) {
        #error happens when null ROW is passed by fromJSON()
        null_row <- sapply(res$data$items, is.null)
        data.table::rbindlist(res$data$items[!null_row])
      })
    } else {
      #create an empty data.table
      dt <- do.call(data.table::data.table, rep_len(x = list(logical()),
                                                    length.out = length(res$data$fields)))
    }
  })
  data.table::setnames(dt, unlist(res$data$fields))

  dt
}
