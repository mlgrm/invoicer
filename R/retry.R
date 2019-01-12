#' wrap a function so that it keeps executing until none of the specified 
#' errors is returned
#' 
#' @param .f function to be wrapped
#' @param errors vector of error messages on whose return we should retry
#' @param max_tries maximum number of retries
#' 
#' @export
retry <- function(.f, errors = getOption("retry_errors", 
                                         "Error in the HTTP2 framing layer"), 
                  max_tries = 10)
  # returning a function that...
  function(...){
    tries <- 0
    while({
      # runs the function safely on its parameters, 
      res <- (purrr::safely(.f))(...)
      # checks if it returned one of our retry errors
      !is.null(res$error) && res$error$message %in% errors
    }){
      # if it did, makes sure we haven't exceeded our max
      if(tries > max_tries) stop("too many failures")
      # then signals a retry
      message("got error \"", error, "\", retrying...")
      tries <- tries + 1
    }
    # otherwise returns the result or throws the unspecified error
    if(is.null(res$error)) res$result else stop(res$error)
  }