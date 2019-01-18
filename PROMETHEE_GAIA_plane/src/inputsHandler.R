library(rJava)
library(XMCDA3)

checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
  alternatives = handleException(
    function() return(getActiveAlternatives(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the active alternatives, reason: "
    )
  
  criteria = handleException(
    function() return(getActiveCriteria(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives values, reason: "
    )

  performanceTable = handleException(
    function() return(getNumericPerformanceTableList(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives values, reason: "
    )
  
  criteriaWeights = handleException(
    function() return(getNumericCriteriaValuesList(xmcdaData)),
      xmcdaMessages,
      humanMessage = "Unable to extract the alternatives values, reason: "
    )

  alternatives = Map({
    function (alternative) list(id=alternative$id(), name=alternative$name())
  }, alternatives$alternatives)

  criteria = Map({
    function (criterion) list(id=criterion$id(), name=criterion$name())
  }, criteria$criteria)

  return(list(
    alternatives=alternatives,
    criteria=criteria,
    profiles=performanceTable[[1]],
    weights=criteriaWeights[[1]]
  ))
}
