
MaxHeap <- function(maxSize, ...) {
  heap <- new.env(hash = FALSE, parent = emptyenv())
  heap$array   <- NULL
  heap$maxSize <- maxSize
  heap$size    <- 0L
  class(heap)  <- "Max heap"
  return(heap)
}

get_heap_array <- function(heap) {
  heap$array[seq(heap$size)]
}

insertKey_heap <- function(heap, x) {
  if (heap$size == heap$maxSize) {
    return(invisible(NULL))
  }
  # Insert new key at the end
  heap$size <- heap$size + 1L
  i <- heap$size
  heap$array[i] <- x
  # Check max heap property
  while (i != 1L && heap$array[parent_heap(i)] < heap$array[i]) {
    tmp <- heap$array[i]
    heap$array[i] <- heap$array[parent_heap(i)]
    heap$array[parent_heap(i)] <- tmp
    i <- parent_heap(i)
  }
}

getMax_heap <- function(heap) {
  heap$array[1L]
}

parent_heap <- function(i) {
  i %/% 2L
}

lChild_heap <- function(i) {
  2L * i
}

rChild_heap <- function(i) {
  2L * i + 1L
}

MaxHeapify <- function(heap, i) {
  l <- lChild_heap(i)
  r <- rChild_heap(i)
  largest <- i
  if (l <= heap$size && heap$array[l] > heap$array[i]) {
    largest <- l
  }
  if (r <= heap$size && heap$array[r] > heap$array[largest]) {
    largest <- r
  }
  if (largest != i) {
    tmp <- heap$array[i]
    heap$array[i] <- heap$array[largest]
    heap$array[largest] <- tmp
    MaxHeapify(heap, largest)
  }
}

removeMax_heap <- function(heap) {
  # Check if heap array is empty
  if (heap$size <= 0L) {
    return(invisible(NULL))
  }
  # Check if heap array has a single element
  if (heap$size == 1L) {
    tmp <- heap$array[1L]
    heap$array <- NULL
    heap$size  <- heap$size - 1L
    return(tmp)
  }
  # Remove maximum element
  maxElem <- heap$array[1L]
  heap$array[1L] <- heap$array[heap$size]
  heap$size <- heap$size - 1L
  # Restore Max heap property
  MaxHeapify(heap, 1L)
  return(maxElem)
}

increaseKey_heap <- function(heap, i, x) {
  if (i > heap$size) {
    return(NULL)
  }
  heap$array[i] <- x
  while (i != 1L && heap$array[parent_heap(i)] < heap$array[i]) {
    tmp <- heap$array[i]
    heap$array[i] <- heap$array[parent_heap(i)]
    heap$array[parent_heap(i)] <- tmp
    i <- parent_heap(i)
  }
}

deleteKey_heap <- function(heap, i) {
  if (i > heap$size) {
    return(NULL)
  }
  increaseKey_heap(heap, i, Inf)
  removeMax_heap(heap)
  invisible(NULL)
}
