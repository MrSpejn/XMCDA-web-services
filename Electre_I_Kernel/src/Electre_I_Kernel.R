find_cycle <- function(nodeIdx, path, adjacency_matrix) {
    successors = which(adjacency_matrix[nodeIdx, ] == 1)
    path = c(path, nodeIdx)
    for (successor in successors) {
        if (successor %in% path) return(path[match(successor, path):length(path)])
        
        cycle = find_cycle(successor, path, adjacency_matrix)
        if (length(cycle) > 0) {
            return(cycle)
        }
    }
    return(c())
}

aggregate <- function(adjacency_matrix, vertices_to_squash) {
    rows_to_squash <- adjacency_matrix[vertices_to_squash, -vertices_to_squash, drop = FALSE]
    cols_to_squash <- adjacency_matrix[-vertices_to_squash, vertices_to_squash, drop = FALSE]
    reminder_matrix <- adjacency_matrix[-vertices_to_squash, -vertices_to_squash, drop = FALSE]
    
    squashed_rows <- append(apply(rows_to_squash, 2, function(col) { min(sum(col), 1)}), 0)
    squashed_cols <- apply(cols_to_squash, 1, function(row) { min(sum(row), 1)})
    
    reminder_matrix <- cbind(reminder_matrix, squashed_cols)
    reminder_matrix <- rbind(reminder_matrix, squashed_rows)
    
    rn <- rownames(reminder_matrix)
    rn[nrow(reminder_matrix)] <- paste(rownames(adjacency_matrix)[vertices_to_squash], collapse='_')
    rownames(reminder_matrix) <- rn
    colnames(reminder_matrix) <- rn
    
    reminder_matrix
}

remove_cycles <- function(adjacency_matrix) {
    processed_matrix <- adjacency_matrix
    cycle <- find_cycle(1, c(), processed_matrix)

    while (length(cycle) > 0) {
        processed_matrix <- aggregate(processed_matrix, cycle)

        cycle <- find_cycle(1, c(), processed_matrix)
    }
    processed_matrix
}

get_predecessors <- function(adjacency_matrix) {
    predecessors <- list()
    for (col_idx in seq(1, ncol(adjacency_matrix))) {
        vertex_pred <- rownames(adjacency_matrix)[adjacency_matrix[,col_idx] == 1]
        predecessors[[col_idx]] <- vertex_pred        
    }
    
    return(predecessors)
}

all_marked <- function(data_frame) {
    all(data_frame$status == 'Y' | data_frame$status == 'N')
}

remove_predecessors <- function (predecessors, to_remove) {
    if (length(predecessors)) {
        should_be_removed <- as.logical(!(predecessors %in% to_remove))

        if (any(should_be_removed)) {
            return(predecessors[should_be_removed])
        }
    }
    return(list())
}

find_kernel <- function(adjacency_matrix) {
    predecessors <- get_predecessors(adjacency_matrix)
    data_frame <- data.frame(
        vertex_name=rownames(adjacency_matrix),
        predecessors=I(predecessors),
        status='',
        stringsAsFactors = FALSE
    )

    iterations <- 0
    while(!all_marked(data_frame) && iterations <= nrow(data_frame)) {
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
                remove_predecessors(predecessors, to_remove)
        }
        iterations <- iterations + 1
    }
    if (iterations > nrow(data_frame)) stop("Maximum number iteration exeded finding kernel")
    return(data_frame[data_frame$status == 'Y',]$vertex_name)
}

Electre_I_Kernel <- function(inputs)
{
  acyclicGraph <- remove_cycles(inputs$outrankingMatrix)
  kernel <- find_kernel(acyclicGraph)

  return(list(
    alternatives=rownames(acyclicGraph),
    acyclicGraph=acyclicGraph,
    kernel=kernel
  ))
}
