
Node <- function(key, left=NULL, right=NULL, ...) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$key    <- key
  node$left   <- left
  node$right  <- right
  class(node) <- "Tree Node"
  return(node)
}

insertNode <- function(root, key) {
  node <- Node(key)
  x    <- root
  y    <- NULL
  while (!is.null(x)) {
    y   <- x
    if (key < x$key) {
      x <- x$left
    } else {
      x <- x$right
    }
  }
  if (is.null(y)) {
    y <- node
  } else if (key < y$key) {
    y$left  <- node
  } else {
    y$right <- node
  }
}

inorder <- function(root, ...) {
  x <- root
  stack <- Stack()
  while (!is.null(x) || (stack$len > 0L)) {
    if (!is.null(x)) {
      push(stack, x)
      x <- x$left
    } else {
      x <- pop(stack)
      print(paste0(x$key))
      x <- x$right
    }
  }
}

minValueNode <- function(node) {
  x <- node
  while (!is.null(x$left)) {
    x <- x$left
  }
  x
}

deleteNode <- function(root, key) {
  x <- root
  y <- NULL
  # Check if the key is in the BST
  while (!is.null(x) && x$key != key) {
    y <- x
    if (x$key < key) {
      x <- x$right
    } else {
      x <- x$left
    }
  }
  if (is.null(x)) { # Node not found
    return(NULL)
  }
  if (is.null(x$left) || is.null(x$right)) { # Node has at most one child
    # z will replace the node to be deleted
    z <- NULL
    # if left child does not exist
    if (is.null(x$left)) {
      z <- x$right
    } else {
      z <- x$left
    }
    # node to be deleted is the root
    if (is.null(y)) {
      return(NULL)
    }
    # node to be deleted is y's left or right child
    if (getref(x) == getref(y$left)) {
      y$left <- z
    } else {
      y$right <- z
    }
    # delete node x
    x <- NULL
  } else { # Node has two children
    p   <- NULL
    tmp <- NULL
    # inorder successor of x
    tmp <- x$right
    while (!is.null(tmp$left)) {
      p   <- tmp
      tmp <- tmp$left
    }
    # check if the parent of the inorder successor is the root
    if (!is.null(p)) {
      p$left <- tmp$right
    } else {
      x$right <- tmp$right
    }
    # copy key
    x$key <- tmp$key
    # delete node
    tmp   <- NULL
  }
}
