rm(list=ls())

source("~/solutions/OR/datastructures/utils.R")
source("~/solutions/OR/datastructures/stack.R")
source("~/solutions/OR/datastructures/bst.R")

keys <- c(10L, 7L, 5L, 8L, 15L, 11L, 18L)

root <- insertNode(root, 10L)
for (key in keys[-1L]) {
  insertNode(root, key)
}

inorder(root)
deleteNode(root, 11L)
inorder(root)
deleteNode(root, 15L)
inorder(root)
deleteNode(root, 10L)
inorder(root)
