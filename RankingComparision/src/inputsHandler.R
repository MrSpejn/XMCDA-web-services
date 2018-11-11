library(rJava)
library(XMCDA3)

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

  return(list(
    first_ranking=alternative_values[[1]],
    second_ranking=alternative_values[[2]],
    alternatives=alternatives
  ))
}
