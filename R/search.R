# Search functions

#' Branch and bound on a search tree
#'
#' This function assumes that the search space is a tree, meaning that it does not
#' check whether it is revisiting a node.
#'
#' We use the typical convention of the score representing a cost, meaning that lower
#' scores are better, and the optimum is a minimum.
#'
#' @param init.node Initial node of the search space
#' @param getCostBounds Function that returns the upper and lower bounds of the cost
#' given a node
#' @param getChildren Function that gets the children of the current node in the search
#' tree
#' @param isGoal Function that determines whether a node is a goal node
#' @return an optimal goal node
#' @export
branchBoundOnTree = function( init.node, getCostBounds, getChildren, isGoal ){

  # Node queue
  queue = list()
  # Cost queue
  costs = vector( mode = "numeric" )
  # Upper bound: anything with a lowerbound above this gets thrown out
  highest.cost = Inf

  # Initialize the search
  current.node = init.node

  while ( !isGoal( current.node ) ) {

    # Branch out
    for ( child in getChildren( current.node ) ){

      bounds = getCostBounds( child )

      # Bound check for pruning
      if ( bounds["lower"] <= highest.cost ){

        # Update the highest.cost
        if ( bounds["upper"] < highest.cost )
          highest.cost = bounds["upper"]

        # Insert into queues
        queue = c( queue, list( child ) )
        costs = c( costs, bounds["lower"] )
      }
    }

    # Pop next node from queue
    optima = which( costs == min( costs ) )
    index = optima[ length( optima ) ]
    if ( length( index ) < 1 ) break
    current.node = queue[[ index ]]
    costs = costs[ -index ]
    queue = queue[ -index ]
  }

  # Return
  current.node
}

#' Greedy search
#'
#' Finds the minimum score greedily
#' @param init.node the start node for the search
#' @param getChildren function listing the child nodes of the current node
#' @param getScore function that gives the score of the current node
#' @return the greedily minimal node
#' @export
searchGreedily = function ( next.node, getChildren, getScore ){

  node = NULL
  score = Inf

  while ( !is.null( next.node ) ){

    node = next.node
    next.node = NULL

    for ( child in getChildren( node ) ){
      child.score = getScore( child )
      if ( child.score < score ){
        score = child.score
        next.node = child
      }
    }
  }

  node
}
