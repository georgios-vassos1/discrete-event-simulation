rm(list=ls())

source("~/solutions/OR/datastructures/utils.R")
source("~/solutions/OR/datastructures/stack.R")
source("~/solutions/OR/datastructures/bst.R")


insertNode_test <- function(...) {
  args <- list(...)
  if (is.null(keys <- args[["keys"]])) {
    keys <- c(10L, 7L, 5L, 8L, 15L, 11L, 18L)
  }
  root <- insertNode(NULL, keys[1L])
  for (key in keys[-1L]) {
    insertNode(root, key)
  }
  root
}

inorderStore_test <- function(...) {
  root <- insertNode_test()
  inorder(root)
  unlist(lapply(inorderStore(root), function(x) x$key))
}

deleteNode_test <- function(...) {
  root <- insertNode_test()
  for (key in c(11L, 15L, 10L)) {
    print(paste0("Removing ", key, " from tree."))
    deleteNode(root, key)
    inorder(root)
  }
}

constructBST_test <- function(...) {
  root <- Node(10L)
  root$left <- Node(8L)
  root$left$left <- Node(7L)
  root$left$left$left <- Node(6L)
  root$left$left$left$left <- Node(5L)
  inorder(root)
  root <- constructBST(root)
  preOrderRec(root) # 7, 5, 6, 8, 10
}

kSmallestSum_test <- function(...) {
  root <- insertNode_test(keys = c(20L, 8L, 4L, 12L, 10L, 14L, 22L))
  kSmallestSum(root, 3L) == 22L
}
