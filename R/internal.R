sampling <- function(i, data) {
  sample(setdiff(levels(data$class), data[i,]$class), 1)
}

random <- function(data, noise) {
  noise <- intersect(rownames(data), noise)
  data[noise,]$class <- sapply(noise, sampling, data)
  return(data)
}
