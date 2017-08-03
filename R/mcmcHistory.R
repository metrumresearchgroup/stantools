#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fit PARAM_DESCRIPTION
#' @param pars PARAM_DESCRIPTION, Default: names(fit)
#' @param nParPerPage PARAM_DESCRIPTION, Default: 6
#' @param myTheme PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[bayesplot]{nuts_params}},\code{\link[bayesplot]{mcmc_trace}}
#' @rdname mcmcHistory
#' @export
#' @importFrom bayesplot nuts_params mcmc_trace
#' @importFrom dplyr filter
#' @importFrom ggplot2 scale_x_continuous
mcmcHistory <- function(fit, pars = names(fit), nParPerPage = 6, myTheme = NULL){
  posterior <- as.array(fit, pars = pars)
  pars <- dimnames(posterior)[[3]]
  pnuts <- bayesplot::nuts_params(fit)

  nPars <- length(pars)
  nPages <- ceiling(nPars / nParPerPage)
  parameters <- data.frame(parameter = pars,
                           page = sort(rep(1:nPages, length = nPars)),
                           stringsAsFactors = FALSE)

  for(i in 1:nPages){
    posterior <- as.array(fit, pars = with(parameters, pars[page == i]))
    if(sum((pnuts %>% dplyr::filter(Parameter == "divergent__"))$Value)){
      print(bayesplot::mcmc_trace(posterior,
                       divergences = pnuts,
                       facet_args = list(ncol = 1, strip.position = "left")) +
              myTheme +
              ggplot2::scale_x_continuous(breaks = seq(0, nPost, len = 5)))

    }else{
      print(bayesplot::mcmc_trace(posterior,
                       facet_args = list(ncol = 1, strip.position = "left")) +
              myTheme +
              ggplot2::scale_x_continuous(breaks = seq(0, nPost, len = 5)))
    }
  }
  NULL
}
