#' @export
nonlinearwise <- function(...) {
  UseMethod("nonlinearwise")
}

#' @rdname nonlinearwise
#' @export
nonlinearwise.default <- function(x, y, rate, ...) {

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

  bins <- ovo(data)
  model <- lapply(bins, svr)

  noise <- translate(l1(bins, model))
  noise <- names(sort(noise)[1:rate])
  data <- random(data, noise)

  df <- list()
  df$x <- data[,-ncol(data)]
  df$y <- data$class
  df
}

#' @rdname nonlinearwise
#' @export
nonlinearwise.formula <- function(formula, data, rate, ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  aux <- nonlinearwise.default(modFrame[,-1], modFrame[,1], rate, ...)

  tmp <- data.frame(aux$y, aux$x)
  colnames(tmp) <- colnames(modFrame)
  tmp[,colnames(data)]
}

svr <- function(data) {
  e1071::svm(class ~ ., data, scale=FALSE, kernel="radial")
}

ovo <- function(data) {

  aux <- utils::combn(levels(data$class), 2)

  tmp <- apply(aux, 2, function(i) {
    vet <- base::subset(data, data$class %in% i)
    vet$class <- factor(vet$class)
    return(vet)
  })

  return(tmp)
}

translate <- function(data) {
  aux <- apply(data[, -1], 1, min, na.rm=TRUE)
  names(aux) <- data$r
  return(aux)
}

l1 <- function(data, model) {

  aux <- mapply(function(m, d) {
    tmp <- stats::predict(m, d, decision.values=TRUE)
    tmp <- abs(attr(tmp, "decision.values"))
    data.frame(r=rownames(tmp), d=tmp)
  }, m=model, d=data, SIMPLIFY=FALSE)

  tmp <- Reduce(function(...) {
    merge(..., by="r", all=TRUE)
  }, aux)

  return(tmp)
}
