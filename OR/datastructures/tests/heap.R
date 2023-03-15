rm(list=ls())

source("~/solutions/OR/datastructures/heap.R")

heap <- MaxHeap(15L)

insertKey_heap(heap,  3L)
insertKey_heap(heap, 10L)
insertKey_heap(heap, 12L)
insertKey_heap(heap,  8L)
insertKey_heap(heap,  2L)
insertKey_heap(heap, 14L)

get_heap_array(heap)
deleteKey_heap(heap, 5L)
get_heap_array(heap)
