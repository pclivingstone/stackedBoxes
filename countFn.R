# countFn.R
# written by Paul
# to define a little function that produces a frequency table
# last updated: Nov 2019

# count function ----------------------------------------------------------
# helper function, uses table() to construct a data.table with bins and counts
# can provide the number of bins or the bin boundaries
# might run quicker for large vectors using hist() instead of table() or data.table
# also handles vectors of type character and factor 
countFn <- function(
  x = rnorm(100)
  , bins = 10           # number of bins or bin boundaries
  , expand = FALSE      # expand the first and last bins?
  , varName = NA        # specified variable name
  , sampleN = 1e5       # sample down for speed?
) {
  require(data.table)
  if (is.numeric(x)) {
    if (length(x) > sampleN)
      x <- sample(x, sampleN)
    if (length(bins) == 1) {
      # if length of bins = 1, then number of bins, roughly equal counts
      probs <- seq(0,1,length.out = bins + 1)       # equally spaced probability values
      breaks <- unique(quantile(x,probs))           # calculate the bin boundaries, based on x input vector
    } else {
      # othersise bin contains the bin boundaries
      breaks <- bins
    }
    if (expand) {
      breaks[1] <- -Inf                             # set lower and upper boundaries to infinity
      breaks[length(breaks)] <- Inf
    }
    x <- cut(
      x, breaks
      , include.lowest = T
      , ordered_result = T
    )
  } else {
    # otherwise, x is character or factor, jump straight to here
    breaks <- NA
  }
  temp <- table(x, useNA = 'always')              # this could be slow for large x
  result <- data.table(
    breaks = breaks
    , bin = names(temp)
    , xCounts = as.integer(temp)
  )
  if (!is.na(varName))
    setnames(result, 'xCounts', varName)
  return(result)
}

# test cases --------------------------------------------------------------

if (F) {
  countFn()
  countFn(rpois(1e2, lambda = 2))
  countFn(bins = -4:4)
  countFn(bins = -2:2)
  countFn(varName = 'test')
  countFn(expand = TRUE)
  countFn(sample(x = letters[1:5], size = 100, replace = T))
  countFn(as.factor(sample(x = letters[1:5], size = 100, replace = T)))

  for (i in 5:9) {
    message('generating 10^',i,'random data:')
    flush.console()
    print(system.time(x <- rnorm(10 ^ i)))
    message('call to function:')
    print(system.time(countFn(x)))
    flush.console()
    Sys.sleep(10)
  }
  
}


