getAlterniativeComparision <- function(xmcdaData) {
  matrix <- xmcdaData$alternativesMatricesList$get(as.integer(0))
  keyset <- as.list(matrix$keySet())
  
  pairs <- list()

  for (key in keyset) {
    pairs <- append(pairs, list(list(key$x$id(), key$y$id())))
  }
  return(pairs)
}

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  parameters <- getProgramParametersList(xmcdaData)
  alternatives <- handleException(
    function() return(getActiveAlternatives(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the active alternatives, reason: "
    )

  alternatives <- Map({
    function (alternative) list(id=alternative$id(), name=alternative$name())
  }, alternatives$alternatives)

  alternative_comparisions <- handleException(
    function() return(getAlterniativeComparision(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives comparision, reason: "
    )

  highlighted_alternatives <- list()
  if (xmcdaData$alternativesValuesList$size() > 0) {
    alternative_values <- handleException(
    function() return(getNumericAlternativesValuesList(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives values, reason: "
    )[[1]]

    highlighted_alternatives <- 
      Filter(function(alternative) alternative$id %in% names(alternative_values), alternatives)
    highlighted_alternatives <- sapply(highlighted_alternatives, function(alt) alt$id)
  }
  
  return(list(
    alternatives=alternatives,
    alternative_comparisions=alternative_comparisions,
    highlighted_alternatives=highlighted_alternatives,
    seed=parameters[[1]]$seed[[1]],
    font_size=parameters[[1]]$font_size[[1]],
    is_directed=parameters[[1]]$is_directed[[1]]
  ))
}
