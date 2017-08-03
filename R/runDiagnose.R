#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param model PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param init PARAM_DESCRIPTION
#' @param seed PARAM_DESCRIPTION
#' @param chain PARAM_DESCRIPTION, Default: 1
#' @param refresh PARAM_DESCRIPTION, Default: 100
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname runDiagnose
#' @export
runDiagnose <- function(model, data, init, seed, chain = 1, refresh=100){
  modelName <- basename(model)
  model <- file.path(model, modelName)
  system(paste(model, " diagnose",
               " data file=", data,
               " init=", init, " random seed=", seed,
               " output file=", paste(model, chain, ".csv", sep = ""),
               " refresh=", refresh,
               sep = ""))
}
