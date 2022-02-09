globalCorrelation <- function(functionName, start, end){
  highVals <- evaluateMany(functionName, FALSE, start, end, 0.01)
  lowVals <- evaluateMany(functionName, TRUE, start, end, 0.01)
  highMean <- mean(highVals)
  highVar <- var(highVals)
  lowMean <- mean(lowVals)
  lowVar <- var(lowVals)
  sum <- 0
  for(i in length(seq(start, end, 0.01))){
    sum <- sum + (highVals[[i]] - highMean) * (lowVals[[i]] - lowMean)
  }
  print(cor(highVals, lowVals))
}

plotFunction <- function(outputFilename, functionName, start = 0, end = 1, interval = 0.1, aVals = c()){
  mult <- 0.5
  if(length(aVals) == 0){
    png(paste0("../data/plottedFunctions/", outputFilename, ".png"), width = mult*682, height =  mult*682)
    highVals <- evaluateMany(functionName, FALSE, start, end, interval)
    lowVals <- evaluateMany(functionName, TRUE, start, end, interval)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "",
         xlab = "",
         #ylim = c(min(c(highVals, lowVals)), max(c(highVals, lowVals))),
         ylim = c(-5, 35),
         xlim = c(start, end),
         # xaxt = "n",
         pch = 20)
    
    lines(seq(start, end, interval), highVals, col = "black")
    lines(seq(start, end, interval), lowVals, col = "red")
    
    legend("topleft", legend = c(expression("f"["h"]*"(x)"), expression("f"["l"]*"(x)")), col = c("black", "red"), lty = 1)
    out <- dev.off()
    
  }else{
    
    allVals <- evaluateMany(functionName, FALSE, start, end, interval)
    for(a in aVals){allVals <- c(allVals, evaluateMany(functionName, TRUE, start, end, interval, a))}
    for(a in aVals){
      highVals <- evaluateMany(functionName, FALSE, start, end, interval)
      lowVals <- evaluateMany(functionName, TRUE, start, end, interval, a)
      png(paste0("../data/plottedFunctions/", outputFilename, "a", a, ".png"), width =  mult*682, height =  mult*682)
      plot(c(), c(),
           col = "black",
           type = "p",
           ylab = "",
           xlab = "",
           ylim = c(min(allVals), max(allVals)),
           xlim = c(start, end),
           # xaxt = "n",
           pch = 20)
      lines(seq(start, end, interval), highVals, col = "black")
      lines(seq(start, end, interval), lowVals, col = "red")

      legend("topleft", legend = c("fHigh(x)", "fLow(x)"), pch = 1, col = c("black", "red"))
      out <- dev.off()
    }
    png(paste0("../data/plottedFunctions/", outputFilename, ".png"), width =  mult*682, height =  mult*682)
    allVals <- evaluateMany(functionName, FALSE, start, end, interval)
    for(a in aVals){allVals <- c(allVals, evaluateMany(functionName, TRUE, start, end, interval, a))}
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "",
         xlab = "",
         ylim = c(min(allVals), max(allVals)),
         xlim = c(start, end),
         # xaxt = "n",
         pch = 20)
    lines(seq(start, end, interval), evaluateMany(functionName, FALSE, start, end, interval), col = "black")
    cols <- c("black")
    legs <- c("fHigh(x)")
    allCols <- c("red", "orange", "blue", "green")
    for(i in 1:length(aVals)){
      lines(seq(start, end, interval), evaluateMany(functionName, TRUE, start, end, interval, aVals[[i]]), col = allCols[[i]])
      cols <- c(cols, allCols[[i]])
      legs <- c(legs, paste0("fLowA=", aVals[[i]]))
    }
    legend("topleft", legend = legs, pch = 1, col = cols)
    out <- dev.off()
  }
}

evaluateMany <- function(functionName, low, start, end, interval, aVal = -1){
  vals <- c()
  for(x in seq(start, end, interval)){
    if(aVal == -1){vals <- c(vals, functionName(x, low))}
    else{vals <- c(vals, functionName(x, low, aVal))}
  }
  return(vals)
}

LiuPedagogicalFunction <- function(x, low = FALSE){
  if(low){return(LiuPedagogicalFunction(x) + (x^3 - 0.5) * sin(3 * x - 0.5) + 4 * cos(2 * x))}
  return(5 * x^2 * sin(12 * x))
}

ShiGramacyLeeFunction <- function(x, low = FALSE){
  if(low){return(sin(10 * pi * x) / x + 2.0 * (x - 1)^4.0)}
  return(sin(10 * pi * x) / (2.0 * x) + (x - 1)^4.0)
}

ShiCurrinSinFunction <- function(x, low = FALSE){
  if(low){return(sin(2 * pi * (x - 0.1)))}
  return(sin(2 * pi * (x - 0.1)) + x^2)
}

ShiHolsclawFunction <- function(x, low = FALSE){
  if(low){return((x * sin(x) + x) / 10.0)}
  return(x * sin(x) / 10.0)
}

ShiSantnerFunction <- function(x, low = FALSE){
  if(low){return(exp(-1.4 * x) * cos(3.5 * pi * x) + 0.75 * x^2)}
  return(exp(-1.4 * x) * cos(3.5 * pi * x))
}

SongToalForretalFunction <- function(x, low = FALSE, a = 0.225){
  if(low){return((1 - a * a - 2 * a) * SongToalForretalFunction(x) + 10 * (x - 0.5) - 5)}
  return((6 * x - 2)^2 * sin(12 * x - 4))
}





addSimpleNoise <- function(value, amplitude = 0.15, freq = 10){
  return(amplitude * cos(freq * 2 * pi * value))
}

addComplexNoise <- function(value, amplitude = 0.15, freq = 10){
  return(amplitude * cos(freq * 2 * pi * value) * sin(freq * 2 * pi * value * value))
}

addGaussWhiteNoise <- function(amplitude = 0.15, seed = 1){
  set.seed(seed)
  return(amplitude * rnorm(1)/2)
}

addCauchyNoise <- function(prob = 0.048 + 0.15 * 0.15 / 0.99, amplitude = 0.15, seed = 1){
  set.seed(seed)
  calc <- rnorm(1) / (abs(rnorm(1)) + 1e-199)
  num <- runif(1)
  if(calc > 8){calc <- 8.0}
  if(calc < -8){calc <- -8.0}
  if(num < prob){
    return(amplitude * calc / 4);
  }else{
    return(0.0);
  }
}

addGaussNoise <- function(amplitude = 0.15, seed = 1){
  set.seed(seed)
  return(exp(log(1 + 2 * amplitude) * rnorm(1) / 2))
}

addUniformNoise <- function(value, amplitude = 0.15, seed = 1){
  set.seed(seed)
  
  newAlpha <- log(1 + 2*amplitude)/(log(2 * 10^9 / 25)*(log(0.99) + 0.99 * 0.49))
  
  alpha <- newAlpha * (0.49)
  
  return(runif(1)^newAlpha * max(1.0, (10^9 / (value + 1e-99))^(alpha * runif(1))))
}


COCOfunctionOneSimpleNoise <- function(x, low = FALSE){
  if(low){return(COCOfunctionOneSimpleNoise(x) + addSimpleNoise(x + 5) * 25)}
  return(x^2)
}

COCOfunctionOneComplexNoise <- function(x, low = FALSE){
  if(low){return(COCOfunctionOneComplexNoise(x) + addComplexNoise(x + 5) * 25)}
  return(x^2)
}

COCOfunctionOneWhiteNoise <- function(x, low = FALSE){
  if(low){return(COCOfunctionOneWhiteNoise(x) + addGaussWhiteNoise(seed = x * 100) * 25)}
  return(x^2)
}

COCOfunctionOneCauchyNoise <- function(x, low = FALSE){
  if(low){return(COCOfunctionOneCauchyNoise(x) + addCauchyNoise(seed = x * 100) * 25)}
  return(x^2)
}

COCOfunctionOneGaussNoise <- function(x, low = FALSE){
  if(low){return(COCOfunctionOneGaussNoise(x) * addGaussNoise(seed = x * 100))}
  return(x^2)
}

COCOfunctionOneUniformNoise <- function(x, low = FALSE){
  if(low){return(COCOfunctionOneUniformNoise(x) * addUniformNoise(COCOfunctionOneUniformNoise(x), seed = x * 100))}
  return(x^2)
}




COCOfunctionOneWhiteNoiseHeightBased <- function(x, low = FALSE){
  if(low){
    # Work out distance to height
    val <- x^2
    dist <- abs(val - 4.3)
    rad <- max(0, (1-dist / 3))
    
    mult <- max(0, 1 - (1 - abs((val)/ 25 - 0.15))/(1-0.1))
    
    #return(COCOfunctionOneWhiteNoiseHeightBased(x) + mult * addGaussWhiteNoise(amplitude = 0.3, seed = x * 100)*25)
    return(COCOfunctionOneWhiteNoiseHeightBased(x) + mult * addComplexNoise(x + 5, amplitude = 0.3) * 25)
  }
  return(x^2)
}

COCOfunctionOneWhiteNoiseHeightBasedReversed <- function(x, low = FALSE){
  if(low){
    # Work out distance to height
    val <- x^2
    dist <- abs(val - 4.3)
    rad <- max(0, (dist - 3) / 3)
    mult <- rad/3
    
    mult <- max(0, 1 - abs(val/25 - 0.15)/0.1)
      
    #return(COCOfunctionOneWhiteNoiseHeightBasedReversed(x) + mult * addGaussWhiteNoise(amplitude = 0.3, seed = x * 100)*25)
    return(COCOfunctionOneWhiteNoiseHeightBasedReversed(x) + mult * addComplexNoise(x + 5, amplitude = 0.3) * 25)
  }
  return(x^2)
}

COCOfunctionOneWhiteNoiseCentreBased <- function(x, low = FALSE){
  if(low){
    # Work out distance to height
    val <- x^2
    dist <- min(abs(x - (-2.5)), abs(x - 4.1))
    
    mult <- max(0, 1 - (1 - dist/10) / (1-0.1))
    
    #return(COCOfunctionOneWhiteNoiseCentreBased(x) + mult * addGaussWhiteNoise(amplitude = 0.3,seed = x * 100)*25)
    return(COCOfunctionOneWhiteNoiseCentreBased(x) + mult * addComplexNoise(x + 5, amplitude = 0.3) * 25)
  }
  return(x^2)
}

COCOfunctionOneWhiteNoiseCentreBasedReverse <- function(x, low = FALSE){
  if(low){
    # Work out distance to height
    val <- x^2
    dist <- min(abs(x - (-2.5)), abs(x - 4.1))
    
    mult <- max(0, 1 - dist / (0.1 * 10))
    
    #return(COCOfunctionOneWhiteNoiseCentreBasedReverse(x) + mult * addGaussWhiteNoise(amplitude = 0.3, seed = x * 100)*25)
    return(COCOfunctionOneWhiteNoiseCentreBasedReverse(x) + mult * addComplexNoise(x + 5, amplitude = 0.3) * 25)
  }
  return(x^2)
}


#plotFunction("liuPedagogical", LiuPedagogicalFunction, 0, 1, 0.01)
#plotFunction("ShiGramacyLee", ShiGramacyLeeFunction, 0.01, 1, 0.01)
#plotFunction("ShiCurrinSin", ShiCurrinSinFunction, 0, 1, 0.01)
#plotFunction("ShiHolsclaw", ShiHolsclawFunction, 0, 10, 0.01)
#plotFunction("ShiSantner", ShiSantnerFunction, 0, 1, 0.01)
#plotFunction("SongToalForretal", SongToalForretalFunction, 0, 1, 0.01, c(0.225, 0, 0.5, 1))
plotFunction("COCOfunctionOne01SimpleNoise", COCOfunctionOneSimpleNoise, -5, 5, 0.01)
plotFunction("COCOfunctionOne02ComplexNoise", COCOfunctionOneComplexNoise, -5, 5, 0.01)
plotFunction("COCOfunctionOne03WhiteNoise", COCOfunctionOneWhiteNoise, -5, 5, 0.01)
plotFunction("COCOfunctionOne04CauchyNoise", COCOfunctionOneCauchyNoise, -5, 5, 0.01)
plotFunction("COCOfunctionOne05GaussNoise", COCOfunctionOneGaussNoise, -5, 5, 0.01)
plotFunction("COCOfunctionOne06UniformNoise", COCOfunctionOneUniformNoise, -5, 5, 0.01)

plotFunction("COCOfunctionOne11ComplexNoiseHeightBased", COCOfunctionOneWhiteNoiseHeightBased, -5, 5, 0.01)
plotFunction("COCOfunctionOne12ComplexNoiseHeightBasedReversed", COCOfunctionOneWhiteNoiseHeightBasedReversed, -5, 5, 0.01)
plotFunction("COCOfunctionOne13ComplexNoiseCentreBased", COCOfunctionOneWhiteNoiseCentreBased, -5, 5, 0.01)
plotFunction("COCOfunctionOne14ComplexNoiseCentreBasedReverse", COCOfunctionOneWhiteNoiseCentreBasedReverse, -5, 5, 0.01)
# globalCorrelation(LiuPedagogicalFunction, 0, 1)
# globalCorrelation(ShiGramacyLeeFunction, 0.01, 1)
# globalCorrelation(ShiCurrinSinFunction, 0, 1)
# globalCorrelation(ShiHolsclawFunction, 0, 10)
# globalCorrelation(ShiSantnerFunction, 0, 1)
# globalCorrelation(SongToalForretalFunction, 0, 1)
