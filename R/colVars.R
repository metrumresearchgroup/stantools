#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colVars
#' @importFrom stats var
#' @export
colVars <- function(a) {
    vars <- a[1,]
    for (n in 1:ncol(a))
        vars[n] <- stats::var(a[,n])
    return(vars)
}
