#' Calculate Gower's distance
#'
#' @param data Data.frame for distance calculation
#'
#' @return dist object containing distances
#' @export
#'
#' @examples
#' data <- data.frame(
#' V1 = c(1, 2, 3),
#' V2 = c(4, 5, 6),
#' V3 = c(7, 8, 9))
#' gowers_distance(data)
gowers_distance <- function(data){
  n <- nrow(data)
  gowers_dist_mat <- matrix(0, nrow=n, ncol=n)
  for(i in 1:n){
    gowers_dist_mat[,i] <- gower::gower_dist(data[i,],data[,])
  }
  gowers_dist <- stats::as.dist(gowers_dist_mat)
  return(gowers_dist)
}
