traverse_deep <- function(node, nodes_dependants, path, cycles) {
    current_path <- append(path, node)
    if (length(nodes_dependants[[node]]) == 0) {
        return(list(nodes_dependants, cycles))
    }
    
    updated_nodes_dependants <- nodes_dependants
    updated_cycles <- cycles
    
    
    for (successor_idx in seq(1:length(updated_nodes_dependants[[node]]))) {
        
        updated_nodes_dependants[[node]][[successor_idx]]$traversed <- TRUE
        successor <- updated_nodes_dependants[[node]][[successor_idx]]

        if (successor$idx %in% current_path) {
            updated_cycles[[length(updated_cycles)+1]] <- 
                unlist(current_path[match(successor$idx, current_path):length(current_path)])
            next  
        }
        new_data <- traverse_deep(successor$idx, updated_nodes_dependants, current_path, updated_cycles)
        updated_nodes_dependants <- new_data[[1]]
        updated_cycles <- new_data[[2]]
    }
    return(list(updated_nodes_dependants, updated_cycles))
}

find_cicles <- function(adjacency_matrix) {
    nodes_dependants <- list()

    for (row_idx in seq(1, nrow(adjacency_matrix))) {
        row <- adjacency_matrix[row_idx, ]
        dependants <- Filter(function (idx) { row[idx] == 1 }, seq(1, length(row)))
        nodes_dependants[[row_idx]] <- sapply(dependants, function (idx) { 
            a <- list(list(idx=idx, traversed=FALSE))
            return(a)
        })
       
    }
    
    start_node <- 0
    cycles <- list()
    while (any(unlist(nodes_dependants) == FALSE) && start_node < length(nodes_dependants)) {
        start_node <- start_node + 1
        if (all(unlist(nodes_dependants[start_node])) == TRUE) {
            next
        }
        current_path <- list(start_node)
        for (node_idx in seq(1:length(nodes_dependants[[start_node]]))) {
            nodes_dependants[[start_node]][[node_idx]]$traversed <- TRUE
            node <- nodes_dependants[[start_node]][[node_idx]]
            
            result <- traverse_deep(node$idx, nodes_dependants, current_path, cycles)
            cycles <- result[[2]]
            nodes_dependants <- result[[1]]
        }
    }

    cycles <- Map(sort, cycles)
    cycles <- Map(as.integer, cycles)

    return(cycles[!duplicated(cycles)])
}

aggregate <- function(adjacency_matrix, vertices_to_squash) {
    squashed_rows <- adjacency_matrix[vertices_to_squash, -vertices_to_squash]
    squashed_cols <- adjacency_matrix[-vertices_to_squash, vertices_to_squash]
    reminder_matrix <- adjacency_matrix[-vertices_to_squash, -vertices_to_squash]
    
    squashed_rows <- append(apply(squashed_rows, 2, function(col) { min(sum(col), 1)}), 0)
    squashed_cols <- apply(squashed_cols, 1, function(row) { min(sum(row), 1)})
    
    reminder_matrix <- cbind(reminder_matrix, squashed_cols)
    reminder_matrix <- rbind(reminder_matrix, squashed_rows)
    
    rn <- rownames(reminder_matrix)
    rn[nrow(reminder_matrix)] <- paste(rownames(adjacency_matrix)[vertices_to_squash], collapse='_')
    rownames(reminder_matrix) <- rn
    colnames(reminder_matrix) <- rn
    
    reminder_matrix
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


remove_cycles <- function(adjacency_matrix) {
    processed_matrix <- adjacency_matrix
    cycles <- find_cicles(processed_matrix)

    while (length(cycles) > 0) {
        shortest <- cycles[which.min(Map(length, cycles))][[1]]
        processed_matrix <- aggregate(processed_matrix, shortest)

        cycles <- find_cicles(processed_matrix)
    }
    processed_matrix
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
    kernel=kernel,
  ))
}
