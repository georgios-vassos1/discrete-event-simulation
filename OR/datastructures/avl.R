
deepcopy.env <- function(env) {
  unserialize(serialize(env, NULL))
}

Node <- function(key=NULL, ...) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$key    <- key
  node$left   <- NULL
  node$right  <- NULL
  node$height <- 1L
  class(node) <- "Tree Node"
  return(node)
}

get_height <- function(root) {
  if (is.null(root)) return(0L)
  root$height
}

get_balance <- function(root) {
  if (is.null(root)) return(0L)
  get_height(root$left) - get_height(root$right)
}

left_rotate <- function(z, ...) {
  y  <- z$right
  T2 <- y$left
  # Perform rotation
  y$left  <- z
  z$right <- T2
  # Update heights
  z$height <- 1L + max(get_height(z$left), get_height(z$right))
  y$height <- 1L + max(get_height(y$left), get_height(y$right))
  # Return rotated tree
  y
}

right_rotate <- function(z, ...) {
  y  <- z$left
  T3 <- y$right
  # Perform rotation
  y$right <- z
  z$left  <- T3
  # Update heights
  z$height <- 1L + max(get_height(z$left), get_height(z$right))
  y$height <- 1L + max(get_height(y$left), get_height(y$right))
  # Return rotated tree
  y
}

insert.avl <- function(root, key) {
  # Perform normal BST
  if (is.null(root)) {
    return(Node(key))
  }
  if (key < root$key) {
    root$left <- insert.avl(root$left, key)
  } else if (key > root$key) {
    root$right <- insert.avl(root$right, key)
  } else {
    return(root)
  }
  # Update the height of the ancestor node
  root$height <- 1L + max(get_height(root$left), get_height(root$right))
  # Get the balance factor
  balance <- get_balance(root)
  # If the node is unbalanced, try out the 4 cases
  # Case 1 - Left  Left
  if (balance > 1L && key < root$left$key) {
    return(right_rotate(root))
  }
  # Case 2 - Right Right
  if (balance < -1L && key > root$right$key) {
    return(left_rotate(root))
  }
  # Case 3 - Left  Right
  if (balance > 1L && key > root$left$key) {
    root$left <- left_rotate(root$left)
    return(right_rotate(root))
  }
  # Case 4 - Right Left
  if (balance < -1L && key < root$right$key) {
    root$right <- right_rotate(root$right)
    return(left_rotate(root))
  }
  return(root)
}

delete.avl <- function(root, key) {
  # Perform standard BST delete
  if (is.null(root)) {
    return(NULL)
  } else if (key < root$key) {
    root$left <- delete.avl(root$left, key)
  } else if (key > root$key) {
    root$right <- delete.avl(root$right, key)
  } else {
    if (is.null(root$left)) {
      tmp  <- root$right
      root <- NULL
      return(tmp)
    } else if (is.null(root$right)) {
      tmp  <- root$left
      root <- NULL
      return(tmp)
    }
    tmp <- get_min_key_node(root$right)
    root$key <- tmp$key
    root$right <- delete.avl(root$right, tmp$key)
  }
  # If the tree has only one node, simply return it.
  if (is.null(root)) {
    return(NULL)
  }
  # Update the height of the ancestor node.
  root$height = 1L + max(get_height(root$left), get_height(root$right))
  # Get the balance factor
  balance <- get_balance(root)
  # If the node is unbalanced, try out the 4 cases
  # Case 1 - Left  Left
  if (balance > 1L && get_balance(root$left) >= 0L) {
    return(right_rotate(root))
  }
  # Case 2 - Right Right
  if (balance < -1L && get_balance(root$right) <= 0L) {
    return(left_rotate(root))
  }
  # Case 3 - Left  Right
  if (balance > 1L && get_balance(root$left) < 0L) {
    root$left <- left_rotate(root$left)
    return(right_rotate(root))
  }
  # Case 4 - Right Left
  if (balance < -1L && get_balance(root$right) > 0L) {
    root$right <- right_rotate(root$right)
    return(left_rotate(root))
  }
  return(root)
}

pre.order <- function(root) {
  if (is.null(root)) {
    return(invisible(NULL))
  }
  print(root$key)
  pre.order(root$left)
  pre.order(root$right)
}
