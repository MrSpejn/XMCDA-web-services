# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  alternatives = "alternatives",
  kernel = "alternativesValues",
  outranking_matrix = "alternativesMatrix",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternatives = "alternatives",
  kernel = "alternativesValues",
  outranking_matrix = "alternativesMatrix",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}

putKernel <- function(kernel) {
  alternativesValues <- rep(1, length(kernel))
  names(alternativesValues) <- kernel

  xmcda <- .jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcda, alternativesValues)

  xmcda
}

qualifiedValue <- function(value) {
	javaValues <- .jnew("org/xmcda/QualifiedValues")
	javaValue <- .jnew("org/xmcda/QualifiedValue")

	if (is.integer(value)) {
	  javaValue$setValue(.jnew("java/lang/Integer", value))
  }
	else if (is.logical(value)) {
	  javaValue$setValue(.jnew("java/lang/Boolean", value))
  }
	else if (is.double(value)) {
	  javaValue$setValue(.jnew("java/lang/Double", value))
  }
	javaValues$add(javaValue)
	javaValues
}


putAlternatives <- function(alternatives) {
  xmcda <- .jnew("org/xmcda/XMCDA")

  javaAlternatives <- .jnew("org/xmcda/Alternatives")

  for (i in 1:length(alternatives)) {
    alternative <- .jnew("org/xmcda/Alternative", toString(alternatives[[i]]))
    javaAlternatives$add(alternative)
  }

  xmcda$alternatives <- javaAlternatives

  xmcda
}


putAlternativesMatrix <- function(matrix) {
  xmcda <- .jnew("org/xmcda/XMCDA")
	javaAlternativesAssignments <- .jnew("org/xmcda/AlternativesMatrix")

	for (i in rownames(matrix)) {
		ai = .jcast(.jnew("org/xmcda/Alternative", i), "java/lang/Object")

		for (j in colnames(matrix)) {
			mval = matrix[i, j]
			if (!is.na(mval)) {
				aj = .jcast(.jnew("org/xmcda/Alternative", j), "java/lang/Object")
				coord = .jnew("org/xmcda/utils/Coord", ai, aj)
				value = qualifiedValue(mval)
				javaAlternativesAssignments$put(coord, value)
			}
		}
	}
	xmcda$alternativesMatricesList$add(javaAlternativesAssignments)
  return(xmcda)

}

convert <- function(results, programExecutionResult) {

  alternatives = putAlternatives(results$alternatives)
  kernel = putKernel(results$kernel)
  outrankingMatrix = putAlternativesMatrix(results$acyclicGraph)

  return(list(
    alternatives = alternatives,
    kernel = kernel,
    outranking_matrix = outrankingMatrix
  ))
}
