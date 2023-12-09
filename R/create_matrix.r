# create_matrix <- function(pairwise_list) {
#   # Extract group names and values
#   groups <- strsplit(names(pairwise_list), "-")  # Split the group names
#   values <- as.numeric(pairwise_list)  # Extract the values
#
#   # Determine the number of unique groups
#   unique_groups <- unique(unlist(groups))
#   n <- length(unique_groups)
#
#   # Create an n x n matrix filled with zeros
#   mat <- matrix(1, nrow = n, ncol = n, dimnames = list(unique_groups, unique_groups))
#
#   # Assign values to the matrix
#   for (i in seq_along(groups)) {
#     group1 <- groups[[i]][1]
#     group2 <- groups[[i]][2]
#     mat[group1, group2] <- values[i]
#     mat[group2, group1] <- values[i]  # Assuming symmetry
#   }
#
#   return(mat)
# }

create_matrix <- function(pairwise_list) {
  # Extract group names and values
  pairs <- strsplit(names(pairwise_list), " - ")  # Split the group names
  values <- as.numeric(pairwise_list)  # Extract the values

  # Determine the number of unique groups
  unique_groups <- unique(unlist(pairs))
  n <- length(unique_groups)

  # Create an n x n matrix filled with zeros
  mat <- matrix(1, nrow = n, ncol = n, dimnames = list(unique_groups, unique_groups))

  # Assign values to the matrix
  for (i in seq_along(pairs)) {
    group1 <- pairs[[i]][1]
    group2 <- pairs[[i]][2]
    idx1 <- which(unique_groups == group1)
    idx2 <- which(unique_groups == group2)
    mat[idx1, idx2] <- values[i]
    mat[idx2, idx1] <- values[i]  # Assuming symmetry
  }

  return(mat)
}

