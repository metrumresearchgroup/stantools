#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname getSimsTable
#' @export
#' @importFrom dplyr mutate
getSimsTable <- function(x, ...){
  nChains <- dim(x)[2]
  nPost <- dim(x)[1]
  x %>%
    as.data.frame(...) %>%
    dplyr::mutate(chain = rep(1:nChains, ea = nPost),
           iteration = rep(1:nPost, nChains))
}
