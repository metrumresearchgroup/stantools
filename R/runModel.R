#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param model PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param iter PARAM_DESCRIPTION
#' @param warmup PARAM_DESCRIPTION
#' @param thin PARAM_DESCRIPTION
#' @param init PARAM_DESCRIPTION
#' @param seed PARAM_DESCRIPTION
#' @param chain PARAM_DESCRIPTION, Default: 1
#' @param stepsize PARAM_DESCRIPTION, Default: 1
#' @param adapt_delta PARAM_DESCRIPTION, Default: 0.8
#' @param max_depth PARAM_DESCRIPTION, Default: 10
#' @param refresh PARAM_DESCRIPTION, Default: 100
#' @param tag PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname runModel
#' @export
runModel <- function(model, data, iter, warmup, thin, init, seed, chain = 1,
                     stepsize = 1, adapt_delta = 0.8, max_depth = 10, refresh = 100, tag=NULL){
  modelName <- basename(model)
  model <- file.path(model, modelName)
  if(! is.null(tag)) output <- paste0(model, "_", tag, "_") else output=model
  system(paste(model, " sample algorithm=hmc engine=nuts",
               " max_depth=", max_depth,
               " stepsize=", stepsize,
               " num_samples=", iter,
               " num_warmup=", warmup, " thin=",  thin,
               " adapt delta=", adapt_delta,
               " data file=", data,
               " init=", init, " random seed=", seed,
               " output file=", paste(output, chain, ".csv", sep = ""),
               " refresh=", refresh,
               sep = ""))
}
