getMatrixValue = function(xmcdaMat, a1, a2) {
	value = xmcdaMat$get(a1, a2)
	if (is.null(value)) {
		msg <- paste("The alternatives matrix does not contain value for", a1$id(), "-", a2$id())
		putProgramExecutionResult(xmcdaMessages, errors = msg)
		NULL
	} else if (! value$isValid()) {
		msg <- paste("The alternatives matrix has invalid value for", a1$id(), "-", a2$id())
		putProgramExecutionResult(xmcdaMessages, errors = msg)
		NULL
	} else if (value$size() != 1) {
		msg <- paste("The alternatives matrix can contain only single values", a1$id(), "-", a2$id())
		putProgramExecutionResult(xmcdaMessages, errors = msg)
		NULL
	} else {
		value$get(as.integer(0))$getValue()
	}
}

getOutrankingMatrix <- function(xmcdaData, alternatives) {
  extractedMatrix <- xmcdaData$alternativesMatricesList$get(as.integer(0))
  keyset <- as.list(extractedMatrix$keySet())


  outrankingMatrix <- matrix(
    0,
    nrow=length(alternatives),
    ncol=length(alternatives),
    dimnames=list(
      alternatives,
      alternatives
    )
  )
  for (key in keyset) {
    value = getMatrixValue(extractedMatrix, key$x, key$y)
    if (value) {
      outrankingMatrix[key$x$id(), key$y$id()] <- 1
    }
  }
  outrankingMatrix
}

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  alternatives <- handleException(
    function() return(getActiveAlternatives(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the active alternatives, reason: "
    )
  alternatives <- Map({
    function (alternative) list(id=alternative$id(), name=alternative$name())
  }, alternatives$alternatives)

  ids <- sapply(alternatives, function(alt) { alt$id })

  outrankingMatrix <- handleException(
    function() return(getOutrankingMatrix(xmcdaData, ids)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives comparision, reason: "
    )

  return(list(
    outrankingMatrix=outrankingMatrix,
    alternatives=alternatives
  ))
}
