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

  data <- data.frame(x, class=y)
  rate <- trunc(nrow(data)*rate)

  noise <- sample(rownames(data), rate)
  data <- generate(data, noise)

  df <- list()
  df$x <- data[,-ncol(data)]
  df$y <- data$class
  df
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

  aux <- random.default(modFrame[,-1], modFrame[,1], rate, ...)

  tmp <- data.frame(aux$y, aux$x)
  colnames(tmp) <- colnames(modFrame)
  tmp[,colnames(data)]
}

sampling <- function(i, data) {
  sample(setdiff(levels(data$class), data[i,]$class), 1)
}

generate <- function(data, noise) {
  noise <- intersect(rownames(data), noise)
  data[noise,]$class <- sapply(noise, sampling, data)
  return(data)
}
