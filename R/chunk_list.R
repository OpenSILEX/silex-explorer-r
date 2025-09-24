#' Diviser une liste ou vecteur en morceaux (chunks)
#'
#' Cette fonction divise un vecteur ou une liste en sous-listes (chunks) de taille spécifiée.
#'
#' @param vec Un vecteur ou une liste à diviser.
#' @param chunk_size Un entier indiquant la taille de chaque chunk.
#'
#' @return Une liste contenant les chunks.
#'
#' @examples
#' chunk_list(1:10, 3)
#'
#' @export
chunk_list <- function(vec, chunk_size) {
  split(vec, ceiling(seq_along(vec) / chunk_size))
}
