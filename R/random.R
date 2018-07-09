#' @export
random <- function(...) {
  UseMethod("random")
}

#' @rdname random
#' @export
random.default <- function(x, y, rate, ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class must be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(rate < 0 | rate > 1) {
    stop("the noise rate must be higher than 0 and lower than 1")
  }

  rate <- trunc(nrow(x)*rate)
  noise <- sample(1:nrow(x), rate)
  generate(noise, y)
}

#' @rdname random
#' @export
random.formula <- function(formula, data, rate, ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  y <- random.default(modFrame[,-1,drop=FALSE], modFrame[,1,drop=FALSE],
    rate, ...)

  modFrame[,1] <- y
  modFrame[,colnames(data)]
}

sampling <- function(n, y) {
  sample(setdiff(levels(y), y[n]), 1)
}

generate <- function(n, y) {
  y[n] <- sapply(n, sampling, y)
  return(y)
}
