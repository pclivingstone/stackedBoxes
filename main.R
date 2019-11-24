# main.R
# written by Paul Livingstone
# to draw some stacked boxes plots for a Data Viz meetup
# last saved Nov 2019

# cleanup -----------------------------------------------------------------

rm(list = ls())  # clears memory
graphics.off()   # clears all the graphics
cat('\014')      # clears the console

# libraries ---------------------------------------------------------------

library(data.table)
library(ggplot2)

# custom functions ---------------------------------------------------------

# count function ----------------------------------------------------------

source('countFn.R')

args(countFn)

countFn()
countFn(rpois(1e2, lambda = 2))
countFn(bins = -4:4)
countFn(bins = -2:2)
countFn(varName = 'specificVariableName')
countFn(expand = TRUE)
countFn(sample(x = letters[1:5], size = 100, replace = T))
countFn(as.factor(sample(x = letters[1:5], size = 100, replace = T)))
countFn(seq.Date(Sys.Date() - 365, Sys.Date(), length.out = 100))    # doesn't work on dates yet

# speed test
for (i in 5:9) {
  message('generating 10^',i,'random data:')
  flush.console()
  print(system.time(x <- rnorm(10 ^ i)))
  message('call to function:')
  print(system.time(countFn(x)))
  flush.console()
  Sys.sleep(5)
}

rm(i,x);gc()  # clean up

# stacked boxes -----------------------------------------------------------

source('stackedBoxes.R')

args(stackedBoxesPlot)


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
print(xData)
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
stackedBoxesPlot(rnorm(1000), rnorm(1000, 0, 1.1))
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

# grid plot --------------------------------------------------------------------

library(gridExtra)
library(egg)

xData <- countFn(rnorm(1e3), bins = 10)

# construct a list of 10 plots without displaying them
pList <- list()  # initialise empty list
for (i in 1:10)
  pList[[i]] <- stackedBoxesPlot(
    x = xData
    , y = rnorm(
      1e3
      , floor(i/8)*0.5
      )
    , plotIt = FALSE
    , hLabel = NULL
    , vLabel = NULL
    , binLabels = FALSE
    , title = NULL
    # , fillVal = 'none'
    , colourValue = 'chisq'
  )[[1]]

# plot them in a grid
ggarrange(
  plots = pList
  , labels = letters[seq(10)]
  # , debug = T
  # , padding = 0
  , nrow = 2
  # , ncol = 5
  , bottom = 'Time'
  # , left = 'Variables'
  )
# a to g are random, 
# h,i & j have a shift in mean of +0.5

# ridge plot --------------------------------------------------------------

library(ggridges)

ggplot(diamonds, aes(x = price, y = cut, fill = cut)) + geom_density_ridges(scale = 2)

myData <- copy(diamonds)
setDT(myData)

# myLevels <- rev(levels(myData$cut))
myLevels <- levels(myData$cut)
xData <- countFn(myData[cut == myLevels[1], price], bins = 10)
plotLst <- list()
for (i in 2:5)
  plotLst[[myLevels[i]]] <- stackedBoxesPlot(
    xData
    , y = myData[cut == myLevels[i], price]
    , plotIt = F
    , binLabels = F
    , vLabel = myLevels[1]
    , hLabel = myLevels[i]
    , title = NULL
    )[[1]]

ggarrange(plots = plotLst, nrow = 1)

# redraw the ridge plot on log scale
ggplot(diamonds, aes(x = log10(price), y = cut, fill = cut)) + geom_density_ridges(scale = 2)
