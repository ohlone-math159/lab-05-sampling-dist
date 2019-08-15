library(shiny)


distInfo = list()

#Set up the normal distribution
distInfo$normal = list()

normMu = 30
normSigma = 5

distInfo$normal$mu = normMu
distInfo$normal$sigma = normSigma
distInfo$normal$densFun = function(x) { dnorm(x, normMu, normSigma) }
distInfo$normal$realizationFun = function(count) { rnorm(count, normMu, normSigma) }

distInfo$normal$stats = list()
distInfo$normal$stats$mean = function() { normMu }
distInfo$normal$stats$median = function() { normMu }
distInfo$normal$stats$variance = function() { normSigma^2 }
distInfo$normal$stats[["standard deviation"]] = function() { normSigma }
distInfo$normal$stats$range = function() { Inf }

#Set up the exponential distribution
distInfo$exponential = list()

expRate = 1/10

distInfo$exponential$rate = 1/10
distInfo$exponential$densFun = function(x) { dexp(x, rate=expRate) }
distInfo$exponential$realizationFun = function(count) { rexp(count, rate=expRate) }

distInfo$exponential$stats = list()
distInfo$exponential$stats$mean = function() { 1/expRate }
distInfo$exponential$stats$median = function() { 1/expRate * log(2) }
distInfo$exponential$stats$variance = function() { 1/expRate^2 }
distInfo$exponential$stats[["standard deviation"]] = function() { 1/expRate }
distInfo$exponential$stats$range = function() { Inf }

#Set up the uniform distribution
distInfo$uniform = list()

unifLower = 10
unifUpper = 20

distInfo$uniform$lowerBound = unifLower
distInfo$uniform$upperBound = unifUpper
distInfo$uniform$densFun = function(x) { dunif(x, unifLower, unifUpper) }
distInfo$uniform$realizationFun = function(count) { runif(count, unifLower, unifUpper) }

distInfo$uniform$stats = list()
distInfo$uniform$stats$mean = function() { (unifUpper + unifLower) / 2 }
distInfo$uniform$stats$median = function() { (unifUpper + unifLower) / 2  }
distInfo$uniform$stats$variance = function() { 1/12 * (unifUpper - unifLower)^2 }
distInfo$uniform$stats[["standard deviation"]] = function() { sqrt(distInfo$uniform$stats$variance()) }
distInfo$uniform$stats$range = function() { unifUpper - unifLower }





setup = function() {
	resetting <<- TRUE

	sampleSize <<- 10
	samplesToDraw <<- 1

	selectStatFun("mean")
	
	selectDistribution("normal")
}

clearSamples = function() {
	samples <<- matrix(nrow=0, ncol=sampleSize)
}

takeSample = function() {

	for (i in 1:samplesToDraw) {
		thisSample = currentDist$realizationFun(sampleSize)
		samples <<- rbind(samples, thisSample)
	}
}


setScreenMar = function(screen) {
  screen(screen)
  par(mar=c(6,4,2,1))
}

getSSCellMatrix = function(nrow, ncol) {
  screenCells = matrix(0, nrow=nrow * ncol, ncol=4)
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      
      xj = j
      yi = 4 - i
      
      x = c((xj - 1)/ncol, xj/ncol)
      y = c((yi - 1)/nrow, yi/nrow)
      
      screenCells[ (i - 1)*ncol + j, ] = c(x[1], x[2], y[1], y[2])
      
    }
  }
  screenCells
}

mergeCells = function(cells, i1, i2) {
  dim1 = cells[i1,]
  dim2 = cells[i2,]
  
  mins = pmin(dim1, dim2)
  maxs = pmax(dim1, dim2)
  
  cells[ i1, ] = c(mins[1], maxs[2], mins[3], maxs[4])
  
  cells[ -i2, ]
}

getScreenCells = function() {
  cellMat = getSSCellMatrix(3, 4)
  
  cellMat = mergeCells(cellMat, 7, 8)
  cellMat = mergeCells(cellMat, 5, 6)
  cellMat = mergeCells(cellMat, 3, 4)
  cellMat = mergeCells(cellMat, 1, 2)
  
  cellMat = mergeCells(cellMat, 2, 4)
  cellMat = mergeCells(cellMat, 1, 3)
  
  finalCellMat = cellMat
}

countsInBreaks = function(x, breaks) {
  rval = rep(NA, length(breaks) - 1)
  for (i in 1:(length(breaks) - 1)) {
    rval[i] = sum(x >= breaks[i] & x < breaks[i+1])
  }
  rval
}

longBreaks = function(x, n) {
  breaks = pretty(x, n = n)
  
  d = breaks[2] - breaks[1]
  l = length(breaks)
  breaks = c(rep(min(breaks), l), breaks, rep(max(breaks), l)) + (d * c(-(l:1), rep(0, l), 1:l))
  
  breaks
}

plotSamples = function(matchScales=TRUE, showStats=TRUE, showPopulation=TRUE) {
  
	statCex = 1.2
	
  screenCells = getScreenCells()
  populationBarColor = "cadetblue3"
  samplingDistributionBarColor = "darkgoldenrod2"
  firstPreviousSampleBarColor = "indianred2"
  previousSamplesBarColor = populationBarColor
  
  
  close.screen(all.screens=TRUE)
  split.screen(screenCells)
  

	xlim = range(samples) + c(-1, 1) * 0.25 * (max(samples) - min(samples))
  breaks = longBreaks(samples, n=20)
  breakD = breaks[2] - breaks[1]

	setScreenMar(1)
	hist(samples, xlim=xlim, main="All sampled values", xlab="", breaks = breaks, prob=showPopulation, col=populationBarColor)
	
	lastSample = samples[ nrow(samples), ]
	for (i in 1:(length(breaks) - 1)) {
		lower = breaks[i]
		upper = breaks[i+1]
		height = sum(lastSample >= lower & lastSample < upper)
		if (showPopulation) {
			height = height / (length(samples) * breakD)
		}
		polygon(c( lower, lower, upper, upper), y=c(0, height, height, 0), col = firstPreviousSampleBarColor)
	}
	
	
	if (showStats) {
		ltext = c(paste("True Pop.", statFunName, "=", round(currentDist$stats[[statFunName]](), 2)),
							paste("Observed Pop.", statFunName, "=", round(statFun(samples), 2))
							)
		mtext(ltext[1], 1, 3, cex=statCex)
		mtext(ltext[2], 1, 4, cex=statCex)
	}
	
	if (showPopulation) {
		xv = seq(xlim[1], xlim[2], 0.1)
		lines(xv, currentDist$densFun(xv), lwd=2)
	}

	sampDist = apply(samples, 1, statFun)

	if (statFunName %in% c("variance", "standard deviation", "range")) {
		matchScales = FALSE
	}
	
	sdXlim = xlim
	sdBreaks = breaks
	if (!matchScales) {
		sdXlim = range(sampDist) + c(-1, 1) * 0.25 * (max(sampDist) - min(sampDist))
		sdBreaks = longBreaks(sampDist, n = 20)
	}
	
	#####
	# Sampling distribution
	setScreenMar(2)
	hist(sampDist, xlim=sdXlim, main=paste("Sampling distribution of the\nsample ", statFunName, "s", sep=""), 
			 xlab="", breaks=sdBreaks, prob=showPopulation, col=samplingDistributionBarColor)
	
	lastStat = sampDist[ length(sampDist) ]
	lower = max(sdBreaks[ sdBreaks <= lastStat ])
	upper = min(sdBreaks[ sdBreaks > lastStat ])
	height = 1
	if (showPopulation) {
		height = height / (length(sampDist) * breakD)
	}
	polygon(c( lower, lower, upper, upper), y=c(0, height, height, 0), col = firstPreviousSampleBarColor)
	
	if (showStats) {
		ltext = c(paste("Samp. dist. mean =", round(mean(sampDist), 2)),
							paste("Samp. dist. SD =", round(sd(sampDist), 2) ))
		#legend("topright", legend=ltext, bty='n' )
		mtext(ltext[1], 1, 3, cex=statCex)
		mtext(ltext[2], 1, 4, cex=statCex)
	}
	
	
	#Plot previous samples
	previousSampleNames = c("Last sample", "2nd to last", "3rd to last", "4th to last")
	previousSamples = samples[nrow(samples):max((nrow(samples) - 3), 1),]

	if (is.vector(previousSamples)) {
	  previousSamples = matrix(previousSamples, nrow=1)
	}
	
	maxheight = max(apply(previousSamples, 1, function(x) {
	  countsInBreaks(x, breaks)
	}))
	
	for (i in 1:nrow(previousSamples)) {
	  screen(i + 2)
	  par(mar=c(2,2,2,0))
	  
	  value = statFun(previousSamples[i,])
	  title = paste( previousSampleNames[i], "\n", statFunName, " = ", round(value, 2), sep="")
	  
	  col = ifelse(i == 1, firstPreviousSampleBarColor, previousSamplesBarColor)
	  
	  hist(previousSamples[i,], xlim=xlim, ylim=c(0,maxheight), main=title, xlab="", 
	       breaks=breaks, prob=FALSE, col=col)

	}
	
}


selectStatFun = function(newStatFunName) {
	statFunName <<- newStatFunName
	if (statFunName == "mean") {
		statFun <<- mean
	} else if (statFunName == "median") {
		statFun <<- median
	} else if (statFunName == "standard deviation") {
		statFun <<- sd
	} else if (statFunName == "variance") {
		statFun <<- function(x) { var(as.vector(x)) }
	} else if (statFunName == "range") {
		statFun <<- function(x) { max(x) - min(x) }
	} else {
		stop("Invalid stat function selected in selectStatFun().")
	}
}

selectDistribution = function(distribution) {
	currentDist <<- distInfo[[distribution]]
}

setup()



# Define UI for application that draws a histogram
thisUI = shinyUI(fluidPage(
	
	titlePanel("Sampling Distributions"),
	
	sidebarLayout(
		sidebarPanel(

			selectInput("distribution", 
									label = "Population Distribution",
									choices = list("normal", "uniform", "exponential"),
									selected = "normal"),

			selectInput("statFun", 
									label = "Statistic",
									choices = list("mean", "median",
																 "variance", "standard deviation", "range"),
									selected = "mean"),

			numericInput("sampleSize", label="Sample size", value=sampleSize),
			numericInput("samplesToDraw", label="Samples to draw at a time", value=samplesToDraw),
			
			actionButton("sample", label="Sample"),
			actionButton("clear", label="Clear"),
			checkboxInput("matchScales", label="Match scales", value=TRUE),
			checkboxInput("showStats", label="Show stats", value=FALSE),
			checkboxInput("showPopulation", label="Show population", value=FALSE)
			
		),
		
		mainPanel(
			plotOutput("distPlot", height=600)
		)
	)
))



thisServer = shinyServer(function(input, output, clientData, session) {
	

	observeEvent(input$sample, { takeSample() })
	
	observeEvent(input$clear, { clearSamples() })
	
	observeEvent(input$sampleSize, { sampleSize <<- input$sampleSize; clearSamples() })
	
	observeEvent(input$samplesToDraw, { samplesToDraw <<- input$samplesToDraw })
	
	observeEvent(input$statFun, { selectStatFun(input$statFun) })
	
	observeEvent(input$distribution, { selectDistribution(input$distribution) })
	

	
	output$distPlot <- renderPlot({
		
		input$sample
		input$clear
		input$sampleSize
		input$statFun
		
		if (nrow(samples) > 0) {
			plotSamples(matchScales=input$matchScales, showStats=input$showStats, showPopulation=input$showPopulation)
		}
		
	})
})


shinyApp(ui = thisUI, server = thisServer)

