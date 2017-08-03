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
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname runModelFixed
#' @export
runModelFixed <- function(model, data, iter, warmup, thin, init, seed, chain = 1,
                          stepsize = 1, adapt_delta = 0.8, max_depth = 10, refresh = 100){
  modelName <- basename(model)
  model <- file.path(model, modelName)
  system(paste(model, " sample algorithm=fixed_param",
               " num_samples=", iter,
               " data file=", data,
               " random seed=", seed,
               " output file=", paste(model, chain, ".csv", sep = ""),
               " refresh=", refresh,
               sep = ""), invisible = FALSE)
}
