findCycleTraverseDeepFirst <- function(nodeIdx, path, adjacencyMatrix) {
    successors = which(adjacencyMatrix[nodeIdx, ] == 1)
    path = c(path, nodeIdx)
    for (successor in successors) {
        if (successor %in% path) return(path[match(successor, path):length(path)])
        
        cycle = findCycleTraverseDeepFirst(successor, path, adjacencyMatrix)
        if (length(cycle) > 0) {
            return(cycle)
        }
    }
    return(c())
}

findCycle <- function(adjacencyMatrix) {
    for (i in seq(nrow(adjacencyMatrix))) {
        result = findCycleTraverseDeepFirst(i, c(), adjacencyMatrix)
        if (length(result) > 0) {
            return(result)
        }
    }
    return(c())
}

aggregateToSingleNode <- function(adjacencyMatrix, vertices) {
    rows_to_squash <- adjacencyMatrix[vertices, -vertices, drop = FALSE]
    cols_to_squash <- adjacencyMatrix[-vertices, vertices, drop = FALSE]
    reminderMatrix <- adjacencyMatrix[-vertices, -vertices, drop = FALSE]
    
    squashed_rows <- append(apply(rows_to_squash, 2, function(col) { min(sum(col), 1)}), 0)
    squashed_cols <- apply(cols_to_squash, 1, function(row) { min(sum(row), 1)})
    
    reminderMatrix <- cbind(reminderMatrix, squashed_cols)
    reminderMatrix <- rbind(reminderMatrix, squashed_rows)
    
    rn <- rownames(reminderMatrix)
    rn[nrow(reminderMatrix)] <- paste(rownames(adjacencyMatrix)[vertices], collapse='_')
    rownames(reminderMatrix) <- rn
    colnames(reminderMatrix) <- rn
    
    reminderMatrix
}

removeCycles <- function(adjacencyMatrix) {
    processed_matrix <- adjacencyMatrix
    cycle <- findCycle(processed_matrix)

    while (length(cycle) > 0) {
        processed_matrix <- aggregateToSingleNode(processed_matrix, cycle)
        cycle <- findCycle(processed_matrix)
    }
    processed_matrix
}

getPredecessorsForAll <- function(adjacencyMatrix, names) {
    predecessors <- list()
    for (col_idx in seq(1, ncol(adjacencyMatrix))) {
        vertex_pred <- names[adjacencyMatrix[,col_idx] == 1]
        predecessors[[col_idx]] <- vertex_pred        
    }
    
    return(predecessors)
}

removePredecessors <- function (predecessors, to_remove) {
    if (length(predecessors)) {
        should_be_removed =
            as.logical(!(predecessors %in% to_remove))

        if (any(should_be_removed)) {
            return(predecessors[should_be_removed])
        }
    }
    return(list())
}

allMarked <- function(data_frame) {
    all(data_frame$status == 'Y' | data_frame$status == 'N')
}

findKernel <- function(adjacencyMatrix) {
    names = rownames(adjacencyMatrix)
    if (is.null(names)) {
        names = seq(nrow(adjacencyMatrix))
    }
    
    predecessors <- getPredecessorsForAll(adjacencyMatrix, names)

    data_frame <- data.frame(
        vertex_name=names,
        predecessors=I(predecessors),
        status='',
        stringsAsFactors = FALSE
    )

    iterations <- 0
    while(!allMarked(data_frame) && iterations <= nrow(data_frame)) {
        to_remove <- list()

        for (row_idx in seq(1, nrow(data_frame))) {
            row <- data_frame[row_idx,]
            predecessors <- row$predecessors[[1]]
            
            if (row$status != '') next
            if (length(predecessors) == 0) {
                data_frame[row_idx,]$status <- 'Y'
            }
            else {
                in_kernel <- data_frame[data_frame$status == 'Y',]$vertex_name
                if (any(in_kernel %in% predecessors)) {
                    data_frame[row_idx,]$status <- 'N'
                    to_remove <- append(to_remove, row$vertex_name)
                }
            }
        }
        for (row_idx in seq(1, nrow(data_frame))) {
            predecessors <- data_frame[row_idx,]$predecessors[[1]]
            
            data_frame[row_idx,]$predecessors[[1]] <-
                removePredecessors(predecessors, to_remove)
        }
        iterations <- iterations + 1
    }
    if (iterations > nrow(data_frame)) stop("Maximum number iteration exeded finding kernel")
    return(data_frame[data_frame$status == 'Y',]$vertex_name)
}

Electre_I_Kernel <- function(inputs)
{
  acyclicGraph <- removeCycles(inputs$outrankingMatrix)
  kernel <- findKernel(acyclicGraph)

  return(list(
    alternatives=rownames(acyclicGraph),
    acyclicGraph=acyclicGraph,
    kernel=kernel
  ))
}
