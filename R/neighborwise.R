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

  rate <- trunc(nrow(x)*rate)

  noise <- n2(x, y)
  noise <- rev(order(noise))[1:rate]
  generate(noise, y)
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

  y <- neighborwise.default(modFrame[,-1,drop=FALSE], modFrame[,1,drop=FALSE],
    rate, ...)

  modFrame[,1] <- y
  modFrame[,colnames(data)]
}

dist <- function(x) {
  as.matrix(cluster::daisy(x, metric="gower", stand=TRUE))
}

intra <- function(x, y, d, i) {
  tmp <- rownames(x[y == y[i],])
  min(d[i, setdiff(tmp, i)])
}

inter <- function(x, y, d, i) {
  tmp <- rownames(x[y != y[i],])
  min(d[i, tmp])
}

n2 <- function(x, y) {

  d <- dist(x)

  aux <- sapply(1:nrow(x), function(i) {
    a <- intra(x, y, d, i)
    r <- inter(x, y, d, i)
    c(a, r)
  })

  colnames(aux) <- rownames(x)
  aux <- aux[1,]/aux[2,]
  return(aux)
}
