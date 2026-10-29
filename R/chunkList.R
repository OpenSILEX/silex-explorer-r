#' Split a Vector or List into Chunks
#'
#' Splits a vector or list into smaller chunks of a specified size.
#'
#' @param vec A vector or list to split.
#' @param chunk_size An integer specifying the maximum size of each chunk.
#'
#' @return A list containing the resulting chunks.
#'
#' @examples
#' chunkList(1:10, 3)
#'
chunkList <- function(vec, chunk_size) {
  split(vec, ceiling(seq_along(vec) / chunk_size))
}
