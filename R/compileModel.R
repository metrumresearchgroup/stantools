#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param model PARAM_DESCRIPTION
#' @param stanDir PARAM_DESCRIPTION, Default: stanDir
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname compileModel
#' @export
compileModel <- function(model, stanDir = stanDir){
  modelName <- basename(model)
  dir.create(model)
  file.copy(paste(model, "stan", sep = "."), file.path(model, paste(modelName, "stan", sep = ".")),
            overwrite = TRUE)
  model <- file.path(model, modelName)
  system(paste("make --directory=", stanDir, " ", model, sep = ""))
}
