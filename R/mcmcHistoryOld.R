#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fit PARAM_DESCRIPTION
#' @param pars PARAM_DESCRIPTION, Default: names(fit)
#' @param nParPerPage PARAM_DESCRIPTION, Default: 6
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mcmcHistoryOld
#' @export
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot aes geom_line labs theme element_text facet_wrap
#' @importFrom tidyr gather
mcmcHistoryOld <- function(fit, pars = names(fit), nParPerPage = 6){
  simsTable <- getSimsTable(fit, pars = pars)
  posterior <- simsTable %>%
    tidyr::gather(key = parameter, value = value, -chain, -iteration)

  parameters <- sort(unique(posterior$parameter))
  nParameters <- length(parameters)
  nPages <- ceiling(nParameters / nParPerPage)
  parameters <- data.frame(parameter = parameters,
                           page = sort(rep(1:nPages, length = nParameters)),
                           stringsAsFactors = FALSE)
  posterior <- posterior %>% dplyr::left_join(parameters)
  posterior$chain <- as.factor(posterior$chain)

  for(i in 1:nPages){
    xplot <- subset(posterior, page == i)
    p1 <- ggplot2::ggplot(xplot, ggplot2::aes(x = iteration, y = value))

    print(p1 + ggplot2::aes(color = chain) + ggplot2::geom_line() +
            ggplot2::labs(x = "iteration", y = "value") +
            ggplot2::theme(text = ggplot2::element_text(size = 12), axis.text = ggplot2::element_text(size = 12),
                  legend.position = "none", strip.text = ggplot2::element_text(size = 8)) +
            ggplot2::facet_wrap(~ parameter, ncol = 1, scales = "free_y"))
  }
  NULL
}
