#' @export
neighborwise <- function(...) {
  UseMethod("neighborwise")
}

#' @rdname neighborwise
#' @export
neighborwise.default <- function(x, y, rate, ...) {

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

  noise <- n2(data)
  noise <- names(rev(sort(noise))[1:rate])
  data <- random(data, noise)

  df <- list()
  df$x <- data[,-ncol(data)]
  df$y <- data$class
  df
}

#' @rdname neighborwise
#' @export
neighborwise.formula <- function(formula, data, rate, ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  aux <- neighborwise.default(modFrame[,-1], modFrame[,1], rate, ...)

  tmp <- data.frame(aux$y, aux$x)
  colnames(tmp) <- colnames(modFrame)
  tmp[,colnames(data)]
}

dist <- function(data) {
  as.matrix(cluster::daisy(data[,-ncol(data)], metric="gower", stand=TRUE))
}

intra <- function(dst, data, i) {
  tmp <- rownames(data[data$class == data[i,]$class,])
  aux <- min(dst[i, setdiff(tmp, i)])
  return(aux)
}

inter <- function(dst, data, i) {
  tmp <- rownames(data[data$class != data[i,]$class,])
  aux <- min(dst[i, tmp])
  return(aux)
}

n2 <- function(data) {

  dst <- dist(data)

  aux <- sapply(rownames(data), function(i) {
    a <- intra(dst, data, i)
    r <- inter(dst, data, i)
    c(a, r)
  })

  aux <- aux[1,]/aux[2,]
  return(aux)
}
