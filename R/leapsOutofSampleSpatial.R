#' @export
#'
#' @title
#' Linear regression model selection with an out-of-sample criterion for spatial extrapolation.
#' 
#' 
#' @description
#' Linear regression model selection with an out-of-sample criterion for spatial 
#' extrapolation. Current version uses the minimum mean squared forecast 
#' error method. While leaps reports
#' subsets for each number of regressors, this reports across all sizes.
#' Randomly samples several sets of observations to be removed. 
#' Brute force algorithm without much regard for memory conservation.
#' ***Future version should implement a parallel version of this.
#' 
#' @details
#' Thanks to Thomas Lumley for the similar \code{leaps()} function that selects
#' with an in-sample criterion. His uses much more efficient algorithms. The
#' current implementation is only robust for nbest=1.
#' 
#' @param xData a matrix of predictors
#' @param yData a response vector
#' @param nReps number of combinations of spatial units to be used in the 
#' estimation 
#' @param tau number of observations to remove from estimation and use in out-of-sample
#' prediction. Default is ceiling(x/10).
#' @param regressor names
#' @param int Add an intercept to the model (default is TRUE)
#' @param nbest number of regressions to report (default is 1)
#' @return list of lm objects
leapsOutofSampleSpatial <- function (xData, yData, nReps = 100, 
                                     tau = ceiling(NROW(xData)/10),
                                     names = NULL,
                                     int = TRUE, nbest = 1) {
  if (!is.logical(int)){
    stop("int should be TRUE or FALSE")
  }
  if (!is.null(names)) {
    colnames(xData) <- names
  }  
  
  if (tau >= NROW(xData)) {
    stop("tau must be strictly less than NROW(xData)")
  }
  
  if (class(xData) == "data.frame"){
    xData <- as.matrix(xData)
  }
  
  obsSets <- sapply(1:nReps, FUN = function(counterOnly){
    sample(x = 1:NROW(xData), size = tau, replace = FALSE)})
  
  finalDf <- foreach(j=1:nReps, .combine = rbind) %do% {
    obsRemoved <- obsSets[,j]
    
    # Remove the observations to be predicted
    xJ <- xData[-obsRemoved,]
    yJ <- yData[-obsRemoved]
    
    # Out of sample observations
    xOutJ <- xData[obsRemoved,]
    yOutJ <- yData[obsRemoved]
    
    # Loop through all the possible numbers of regressors
    regsJ <- foreach(numReg=1:NCOL(xJ), .combine = rbind) %do% {
      # Get the list of combinations of regressors
      listComb <- combn(NCOL(xJ), numReg)
      
      tmp <- foreach(reg=1:NCOL(listComb), .combine = rbind) %do% {
        # Get this set of regressors
        xJReg <- xJ[,listComb[,reg]]
        xOutJReg <- xOutJ[,listComb[,reg]]
        
        # Add intercept
        if (int) {
          xJReg <- cbind(1,xJReg)
          xOutJReg <- cbind(1,xOutJReg)
        }
        
        # Get the fit object
        tmpFit <- lm.fit(x = xJReg, y = yJ)
        # Predict for the other observations
        yFit <- xOutJReg %*% tmpFit$coefficients
        
        msfe <- mean((yOutJ - yFit)^2)
        return(data.frame(j=j, reg=reg, msfe=msfe, numReg=numReg, regUnique=paste("reg =", reg,"numReg =", numReg)))
      }
      return(tmp)
    }
    return(regsJ)
  }
  
  # Collapse down to average rankings
  DT <- data.table(finalDf, key = "regUnique")
  DT <- DT[, list(reg=mean(reg), 
            msfe=mean(msfe), 
            numReg=mean(numReg)), by=regUnique]
  
  # Keep only the best nbest
  DT <- DT[order(DT$msfe)[1:nbest],]
  
  listLm <- lapply(1:nbest, FUN = function(regBestNum){
    listComb <- combn(NCOL(xData), DT$numReg[regBestNum])
    
    # Get this set of regressors
    xReg <- xData[,listComb[,DT$reg[regBestNum]]]
    
    # Add intercept
    if (int) {
      xReg <- cbind(1,xReg)
    }
    
    # convert to data.frame
    xReg <- data.frame(xReg)
    if (int) {
      colnames(xReg)[1] <- "Intercept"
      colnames(xReg)[-1] <- colnames(xData)[listComb[,DT$reg[regBestNum]]]
    } else {
      colnames(xReg) <- colnames(xData)[listComb[,DT$reg[regBestNum]]]
    }
    
    xReg <- as.matrix(xReg)
    mydata <- data.frame(y=yData, xReg)
    tmp <- lm(formula = yData ~ xReg - 1)
    
    # This is really messy.
    # The problem is I would like to retain names of variables and
    # it doesn't seem to let me assign colnames to class matrix.
    # So I convert matrix -> data.frame, assign names,
    # convert back, run regression, fix names up.
    names(tmp$coefficients) <- sapply(names(tmp$coefficients), 
                                      function(name){
                                        substr(name, start = 5, 
                                               stop = nchar(name))})
    tmp
  })
  return(listLm)
}