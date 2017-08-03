#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param object PARAM_DESCRIPTION
#' @param quantiles PARAM_DESCRIPTION, Default: c(0.025, 0.25, 0.5, 0.75, 0.975)
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname summary.mcmc.list
#' @importFrom coda mcmc.list thin nchain
#' @importFrom stats start end lm sd residuals ar
#' @importFrom methods is
#' @export
summary.mcmc.list <- function (object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
                               ...)
{
  x <- coda::mcmc.list(object)
  statnames <- c("Mean", "SD", "Naive SE", "Time-series SE")
  varstats <- matrix(nrow = nvar(x), ncol = length(statnames),
                     dimnames = list(varnames(x), statnames))
  xtsvar <- matrix(nrow = nchain(x), ncol = nvar(x))
  if (is.matrix(x[[1]])) {
    for (i in 1:nchain(x)) for (j in 1:nvar(x)) xtsvar[i, j] <- safespec0(x[[i]][, j])
    xlong <- do.call("rbind", x)
  }
  else {
    for (i in 1:nchain(x)) xtsvar[i, ] <- safespec0(x[[i]])
    xlong <- as.matrix(x)
  }
  xmean <- apply(xlong, 2, mean, na.rm = TRUE)
  xvar <- apply(xlong, 2, var, na.rm = TRUE)
  xtsvar <- apply(xtsvar, 2, mean, na.rm = TRUE)
  varquant <- t(apply(xlong, 2, quantile, quantiles, na.rm = TRUE))
  varstats[, 1] <- xmean
  varstats[, 2] <- sqrt(xvar)
  varstats[, 3] <- sqrt(xvar/(niter(x) * nchain(x)))
  varstats[, 4] <- sqrt(xtsvar/(niter(x) * nchain(x)))
  varquant <- drop(varquant)
  varstats <- drop(varstats)
  out <- list(statistics = varstats, quantiles = varquant,
              start = stats::start(x), end = stats::end(x), thin = coda::thin(x), nchain = coda::nchain(x))
  class(out) <- "summary.mcmc"
  return(out)
}

safespec0 <- function (x) {
  result <- try(spectrum0.ar(x)$spec)
  if (class(result) == "try-error")
    result <- NA
  if (class(result) == "try")
    result <- NA
  result
}

#' @importFrom stats residuals
spectrum0.ar <- function (x) {
  x <- as.matrix(x)
  v0 <- order <- numeric(ncol(x))
  names(v0) <- names(order) <- colnames(x)
  z <- 1:nrow(x)
  for (i in 1:ncol(x)) {
    lm.out <- stats::lm(x[, i] ~ z)
    if (identical(all.equal(stats::sd(stats::residuals(lm.out)), 0), TRUE)) {
      v0[i] <- 0
      order[i] <- 0
    }
    else {
      ar.out <- stats::ar(x[, i], aic = TRUE)
      v0[i] <- ar.out$var.pred/(1 - sum(ar.out$ar))^2
      order[i] <- ar.out$order
    }
  }
  return(list(spec = v0, order = order))
}
