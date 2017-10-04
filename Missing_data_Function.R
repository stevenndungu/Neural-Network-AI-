#Mode function 

#Missing data function
replace_missings <- function(data) {
  if (!is.data.frame(data))
    stop("The Data should be a dataframe")
  for (i in (1:ncol(data))) {
    if (is.character(data[, i]))
      warning("Covert character data to factor before running the function")
    if (is.factor(data[, i]) || is.logical(data[, i])) {
      is.na(data[, i]) = getmode(data[, i])
      return(data)
    }
    else if (is.numeric(data[, i])) {
      num = which(is.na(data[, i]))
      data[num, i] = mean(data[, i], na.rm = TRUE)
    }
  }
  return(data)
}
