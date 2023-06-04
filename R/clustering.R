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

#' Calculate the average weekly profile of a household
#'
#' @param data A list of data.frames in the same format as those in Irish.RData. Does not need to contain survey data.
#'
#' @return weekly_profile A tibble containing the weekly demand profiles of each household
#' @export
#'
#' @examples
#' valid_input <- list(
#' extra = data.frame(tod = c(1, 2, 1, 2), dow = c("Mon", "Tue", "Mon", "Mon")),
#' indCons = data.frame(I1002 = c(1, 10, 20, 30), I1003 = c(40, 50, 60, 70)))
#' weekly_profile(valid_input)
weekly_profile <- function(data){
  ID <- avg_demand <- demand <- dow <- tod <- todow <- yday <- NULL
  # Check if the input is a list
  if (!is.list(data)) {
    stop("Input is not a list.")
  }

  # Check if the input list has at least length two
  if (length(data) < 2) {
    stop("Input list does not have at least length two.")
  }

  # Check if the 'extra' data frame exists in the input list
  if (!("extra" %in% names(data)) || !is.data.frame(data$extra) || !all(c("tod", "dow") %in% names(data$extra))) {
    stop("Input list does not contain the 'extra' data frame with variables 'tod' and 'dow'.")
  }

  # Check if the 'indCons' data frame exists in the input list
  if (!("indCons" %in% names(data)) || !is.data.frame(data$indCons)) {
    stop("Input list does not contain the 'indCons' data frame.")
  }
  indCons <- data$indCons
  extra <- data$extra
  weekly_profile <- indCons %>%
    cbind(tod = extra$tod, dow = extra$dow) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("I"),
                 names_to = "ID",
                 values_to = "demand") %>%
    dplyr::group_by(tod, dow, ID) %>%
    dplyr::summarise(avg_demand = mean(demand)) %>%
    dplyr::mutate(todow = as.factor(paste0(tod,dow))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(tod,dow)) %>%
    tidyr::pivot_wider(names_from = todow, values_from = avg_demand)
  return(weekly_profile)
}

#' Plots the dendrogram from HAC
#'
#' @param dendrogram A 'dendrogram' object as produced by as.dendrogram
#' @param num_clus The number of clusters to be shown
#' @param pal Colour palette used, usually a vector of string hex codes
#'
#' @return A plot of the dendrogram with the specified number of clusters
#' @export
#'
#' @examples
#' hc <- hclust(dist(iris[, 1:4]))
#' dend <- as.dendrogram(hc)
#' pal <- c("#8DA0CB", "#E78AC3", "#66C2A5")
#' plot_clusters(dend, 3, pal)
plot_clusters <- function(dendrogram, num_clus, pal){
  dend <- dendrogram %>%
    dendextend::color_branches(k = num_clus, col = pal) %>%
    dendextend::set("branches_lwd", rep(2,num_clus))
  pl <- plot(dend, leaflab = "none")
}
