#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fit PARAM_DESCRIPTION
#' @param pars PARAM_DESCRIPTION, Default: names(fit)
#' @param byChain PARAM_DESCRIPTION, Default: FALSE
#' @param nParPerPage PARAM_DESCRIPTION, Default: 16
#' @param myTheme PARAM_DESCRIPTION, Default: NULL
#' @param prior PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mcmcDensity
#' @export
#' @importFrom bayesplot nuts_params mcmc_dens_overlay mcmc_dens
#' @importFrom dplyr left_join
#' @importFrom ggplot2 geom_line aes
mcmcDensity <- function(fit, pars = names(fit), byChain = FALSE, nParPerPage = 16,
                        myTheme = NULL, prior = NULL){
  posterior <- as.array(fit, pars = pars)
  pars <- dimnames(posterior)[[3]]
  pnuts <- bayesplot::nuts_params(fit)

  nPars <- length(pars)
  nPages <- ceiling(nPars / nParPerPage)
  parameters <- data.frame(Parameter = pars,
                           page = sort(rep(1:nPages, length = nPars)),
                           stringsAsFactors = FALSE)

  if(!is.null(prior)) prior <- prior %>% dplyr::left_join(parameters)

  for(i in 1:nPages){
    posterior <- as.array(fit, pars = with(parameters, pars[page == i]))
    if(byChain){
      p1 <- bayesplot::mcmc_dens_overlay(posterior)
    }else{
      p1 <- bayesplot::mcmc_dens(posterior)
    }
    if(!is.null(prior))
      p1 <- p1 + ggplot2::geom_line(data = subset(prior, page == i),
                                    ggplot2::aes(x = value, y = density),
                           color = "red")
    print(p1 + myTheme)
  }
  NULL
}
