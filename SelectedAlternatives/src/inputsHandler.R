checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  alternatives <- handleException(
    function() return(getActiveAlternatives(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the active alternatives, reason: "
    )
  
  alternative_values <- handleException(
    function() return(getNumericAlternativesValuesList(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives values, reason: "
    )
  
  alternatives <- Map({
    function (alternative) list(id=alternative$id(), name=alternative$name())
  }, alternatives$alternatives)

  selected_alternatives <- list()
  rejected_alternatives <- list()

  for (alternative in alternatives) {
    value <- alternative_values[[1]][alternative$id]
    if (value == 1) {
      selected_alternatives <- append(selected_alternatives, list(alternative))
    } else {
      rejected_alternatives <- append(rejected_alternatives, list(alternative))
    }
  }
  
  parameters <- getProgramParametersList(xmcdaData)
  return(list(
    selected_alternatives=selected_alternatives,
    rejected_alternatives=rejected_alternatives,
    seed=parameters[[1]][[1]][[1]],
    font_size=parameters[[1]][[2]][[1]]
  ))
}
