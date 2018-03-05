vector_times_cube <- function(vec, cube, com_dim){
  mat <- matrix(0, com_dim, com_dim)
  for(j in 1:com_dim){
    mat <- mat + vec[j] * cube[,,j]
  }
  return(mat)
}

vector_times_cube2 <- function(vec, cube, com_dim, dim){
  mat <- matrix(0, dim[1], dim[2])
  for(j in 1:com_dim){
    mat <- mat + vec[j] * cube[,,j]
  }
  return(mat)
}

vector_times_field <- function(vec, field, com_dim){
  cube <- array(0, dim=rep(com_dim, 3))
  for(j in 1:com_dim){
    for(k in 1:com_dim){
      cube[,,j] <- cube[,,j] + vec[k] * field[[k]][,,j]
    }
  }
  return(cube)
}

mat_times_cube <- function(mat, cube, com_dim){
  for(j in 1:com_dim){
    cube[,,j] <- as.matrix(cube[,,j]) %*% mat
  }
  return(cube)
}
