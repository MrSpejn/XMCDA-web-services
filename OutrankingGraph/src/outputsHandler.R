# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  image = "alternatives",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  image = "alternatives",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, programExecutionResult) {
  xmcdaResult <- .jnew("org/xmcda/XMCDA")
  alternatives <- .jnew("org/xmcda/Alternatives")

  for (i in 1:length(results$alternatives)) {
    alternative <- .jnew("org/xmcda/Alternative", toString(results$alternatives[[i]]$id))
    alternative$setName(results$alternatives[[i]]$name)
    alternatives$add(alternative)
  }

  xmcdaResult$alternatives <- alternatives

  return(list(image = xmcdaResult))
}

