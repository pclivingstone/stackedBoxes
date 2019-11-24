# stackedBoxes.R
# written by Paul Livingstone
# to declare a function that produces a stacked boxes plot
# last saved Oct 2019

# stacked boxes plot function -----------------------------------------------------------
# also requires source('countFn.R')

stackedBoxesPlot <- function(
  x = rnorm(1e3, 0, 1)
  , y = rnorm(1e3, 0, 1)
  , nBins = 10                 # ignored if x is from xCountFn
  , hLabel = 'New'             # horizonal label
  , vLabel = 'Original'        # vertical label
  , title = TRUE               # title to use for the plot, or show the chi squared stat
  , binLabels = TRUE           # label the bins?
  , colourValue = 'area'       # colour = none, area, chisq or bin
  , plotIt = TRUE              # display the plot or just return it
  , verbose = 0                # level of detail to print
) {
  require(data.table)
  require(ggplot2)
  if (is.vector(x)) {
    # if x is a vector
    xData <- countFn(x, nBins)
  } else {
    # assume x is the result of a call to countFn
    xData <- x
  }
  # run y through the count function, using the bin boundaries from x
  yData <- countFn(y, xData$breaks, varName = 'yCounts')
  # merge together
  mData <- merge(xData, yData, by = c('breaks','bin'), all = TRUE)      
  # calculate the dimensions and vertices of the stacked boxes
  # scale height & area so that width reference lines are always at 1
  mData[, height := xCounts / sum(xCounts)]      # scaled to 1
  mData[, area := yCounts / sum(yCounts)]        # scaled to 1
  mData[, width := area / height]
  # remove rows with divide by 0 error
  mData <- mData[ height > 0]                  
  mData[, x1 := -width]
  mData[, x2 := width]
  mData[, cumsumHeights := cumsum(height)]
  mData[, y1 := shift(cumsumHeights, n = 1, fill = 0)]  # lag = 1
  mData[, y2 := cumsumHeights]
  mData[, chisq := (xCounts - yCounts)^2 / xCounts]   # this can be used for colour
  # calc the maximum width
  maxWidth <- max(3, mData$width)
  # calc chi squared goodness of fit test P value
  pValue <- chisq.test(mData$yCounts, p = mData$xCounts, rescale.p = T)$p.value
  if (verbose > 0) {
    # print(bins)
    print(xData)
    print(yData)
    print(mData)
    # print(maxWidth)
    print(pValue)
  }
  # plot the stacked boxes
  result <- qplot(
    data = mData
    , geom = 'rect'
    , xmin = x1
    , xmax = x2
    , ymin = y1
    , ymax = y2
    , xlim = c(-maxWidth, maxWidth)
  ) 
  # axis labels
  result <- result + labs(x = hLabel, y = vLabel)
  # colour depends on fill value
  result <- switch(
    colourValue
    , none = result + geom_rect(colour = 'black', fill = 'white')
    , area = result + geom_rect(aes(fill = area)) + scale_fill_gradient(low = 'yellow', high = 'red')
    , chisq = result + geom_rect(aes(fill = chisq)) + scale_fill_gradient(low = 'yellow', high = 'red')
    , bin = result + geom_rect(aes(fill = bin)) + scale_fill_brewer(type = 'qual', palette = 2)
  )
  # guide lines
  result <- result + geom_vline(xintercept = c(1, -1), linetype = 2)
  # remove the legend
  result <- result + theme(
    legend.position = "none"
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    ) 
  # bin lables
  if (binLabels)
    result <- result + geom_text(aes(x = -maxWidth, y = (y1 + y2)/2, label = bin, hjust = 'left'))
  # title
  if (is.logical(title)) {
    result <- result + labs(title = paste0('ChiSq Pvalue: ', round(pValue, 3)))
  } else {
  result <- result + labs(title = title)
  }
  if (plotIt)
    print(result)
  invisible(list(result, pValue))
}

# function runs -----------------------------------------------------------

if (F) {

  set.seed(124)
  stackedBoxesPlot(colourValue = 'none')
  
  # continuous variables ----------------------------------------------------
  
  # random variation
  stackedBoxesPlot()
  stackedBoxesPlot()
  stackedBoxesPlot()
  stackedBoxesPlot()
  stackedBoxesPlot()
  
  # from counts & bins, not raw data
  xData <- countFn(rnorm(1e3), bins = 10)
  stackedBoxesPlot(xData, rnorm(1e3))
  # not the same size
  stackedBoxesPlot(xData, rnorm(2e3))
  
  # 1k obs, 5 bins
  stackedBoxesPlot(rnorm(1e3), rnorm(1e3), 5)
  stackedBoxesPlot(rnorm(1e3), rnorm(1e3), 10)
  stackedBoxesPlot(rnorm(100), rnorm(100), 5)
  
  # increased centre
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0.1))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0.25))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0.5))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 1))
  
  # decreased centre
  stackedBoxesPlot(rnorm(1000), rnorm(1000, -0.25))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, -0.5))
  
  # increased spread
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 1))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 1.25))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 1.5))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 2))
  
  # decreased spread
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 1))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 0.9))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 0.75))
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 0.5))
  
  # ^ centre, ^ spread
  stackedBoxesPlot(rnorm(1000), rnorm(1000, 0.5, 1.5))
  # v centre, v spread
  stackedBoxesPlot(rnorm(1000), rnorm(1000, -0.5, 0.5))
  
  # discrete variables
  x <- sample(x = letters[1:5], size = 100, replace = T)
  y <- sample(x = letters[1:5], size = 100, replace = T)
  stackedBoxesPlot(x, y, colourValue = 'area')
  stackedBoxesPlot(x, y, colourValue = 'chisq')
  stackedBoxesPlot(x, y, colourValue = 'bin')
  
  stackedBoxesPlot(
    x, y
    , hLabel = 'Recent Frequency'
    , vLabel = 'Reference Frequency'
    , title = 'Stacked Boxes Plot of Some Feature'
    , colourValue = 'bin'
    )
  
  y <- sample(
    x = letters[1:5], size = 100, replace = T
    , prob = (1:5)/sum(1:5)
  )
  stackedBoxesPlot(x, y)
}

