library("rlemon")

generate_maxflow_network <- function(M, max.controls){
  nt <- nrow(M)
  nc <- ncol(M)
  
  # Find all valid matches
  match_indices <- which(!is.na(M), arr.ind = TRUE)
  
  # Edges from treatments to controls
  from_nodes <- 2 + match_indices[, 1]
  to_nodes <- 2 + nt + match_indices[, 2]
  capacities <- rep(1, nrow(match_indices))
  
  # Edges from source to treatments
  from_nodes <- c(from_nodes, rep(1, nt))
  to_nodes <- c(to_nodes, 2 + 1:nt)
  capacities <- c(capacities, rep(max.controls, nt))
  
  # Edges from controls to sink
  from_nodes <- c(from_nodes, 2 + nt + 1:nc)
  to_nodes <- c(to_nodes, rep(2, nc))
  capacities <- c(capacities, rep(1, nc))
  
  network <- list(
    from_nodes = from_nodes,
    to_nodes = to_nodes,
    capacity = capacities
  )
  network
}

generate_mincost_network <- function(M, max.controls, total.controls) {
  nt <- nrow(M)
  nc <- ncol(M)
  num_arcs <- nt * nc + nt + nc

  from_nodes <- integer(num_arcs)
  to_nodes <- integer(num_arcs)
  capacity <- numeric(num_arcs)
  cost <- numeric(num_arcs)
  idx1 <- seq_len(nt * nc)
  from_nodes[idx1] <- rep(seq.int(3, length.out = nt), each = nc)
  to_nodes[idx1] <- rep(seq.int(nt + 3, length.out = nc), times = nt)
  capacity[idx1] <- 1
  cost[idx1] <- as.vector(t(M))

  idx2 <- (nt * nc + 1):(nt * nc + nt)
  from_nodes[idx2] <- seq.int(3, length.out = nt)
  to_nodes[idx2] <- 1
  capacity[idx2] <- max.controls
  cost[idx2] <- 0
  idx3 <- (nt * nc + nt + 1):(nt * nc + nt + nc)
  from_nodes[idx3] <- seq.int(nt + 3, length.out = nc)
  to_nodes[idx3] <- 2
  capacity[idx3] <- 1
  cost[idx3] <- 0
  supplies <- c(
    total.controls - nt * max.controls,
    -total.controls,
    rep(max.controls, nt),
    rep(0, nc)
  )

  nwk <- list(
    from_nodes = from_nodes,
    to_nodes   = to_nodes,
    capacity   = capacity,
    cost       = cost,
    supply     = supplies,
    numNodes   = nt + nc + 2
  )
  nwk
}

max_controls <- function(M, max.controls){
  nwk <- generate_maxflow_network(M, max.controls)
  resulting_maxflow <- MaxFlow(arcSources = nwk[["from_nodes"]],
                               arcTargets = nwk[["to_nodes"]],
                               arcCapacities = nwk[["capacity"]],
                               sourceNode = 1,
                               destNode = 2,
                               numNodes = ncol(M) + nrow(M) + 2)
  resulting_maxflow[["cost"]]
}