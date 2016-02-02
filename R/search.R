# Search functions

#' Branch and bound on a search tree
#'
#' This function assumes that the search space is a tree, meaning that it does not
#' check whether it is revisiting a node.
#'
#' @param init.node Initial node of the search space
#' @param getScoreBounds Function that returns the upper and lower bounds of the score
#' given a node
#' @param getChildren Function that gets the children of the current node in the search
#' tree
#' @param isGoal Function that determines whether a node is a goal node
#' @return an optimal goal node
branchBoundOnTree = function( init.node, getScoreBounds, getChildren, isGoal ){
  # return
}
