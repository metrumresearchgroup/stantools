#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fit PARAM_DESCRIPTION
#' @param pars PARAM_DESCRIPTION, Default: names(fit)
#' @param byChain PARAM_DESCRIPTION, Default: FALSE
#' @param nParPerPage PARAM_DESCRIPTION, Default: 16
#' @param prior PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mcmcDensityOld
#' @export
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot aes geom_density theme element_text facet_wrap geom_line
#' @importFrom tidyr gather
mcmcDensityOld <- function(fit, pars = names(fit), byChain = FALSE, nParPerPage = 16, prior = NULL){
  simsTable <- getSimsTable(fit, pars = pars)
  posterior <- simsTable %>%
    tidyr::gather(key = parameter, value = value, -chain, -iteration)
  simsTable <- getSimsTable(fit, pars = pars)

  parameters <- sort(unique(posterior$parameter))
  nParameters <- length(parameters)
  nPages <- ceiling(nParameters / nParPerPage)
  parameters <- data.frame(parameter = parameters,
                           page = sort(rep(1:nPages, length = nParameters)),
                           stringsAsFactors = FALSE)
  posterior <- posterior %>% left_join(parameters)
  posterior$chain <- as.factor(posterior$chain)

  if(!is.null(prior)) prior <- prior %>% dplyr::left_join(parameters)

  for(i in 1:nPages){
    xplot <- subset(posterior, page == i)
    p1 <- ggplot2::ggplot(xplot, ggplot2::aes(x = value))
    if(byChain) p1 <- p1 + ggplot2::aes(color = chain)
    p1 <- p1 + ggplot2::geom_density() +
      labs(x = "value", y = "density") +
      ggplot2::theme(text = ggplot2::element_text(size = 12), axis.text = ggplot2::element_text(size = 8),
            legend.position = "none", strip.text = ggplot2::element_text(size = 8)) +
      ggplot2::facet_wrap(~ parameter, ncol = 4, nrow = 4, scales = "free")
    if(!is.null(prior))
      p1 <- p1 + ggplot2::geom_line(data = subset(prior, page == i), ggplot2::aes(x = value, y = density),
                           color = "red")
    print(p1)
  }
  NULL
}
