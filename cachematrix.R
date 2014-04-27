## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix<- function(M = matrix(), ...) {
  ## Creates a special "matrix" object that can 
  ## cache its inverse.
  cache_inverse<- NULL
  set_matrix<- function(N) {
    M<<- N
    cache_inverse<- NULL
  }
  get_matrix<- function() M
  set_inverse<- function(inverse_matrix) cache_inverse<<- inverse_matrix
  get_inverse<- function() cache_inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)   
}


## Write a short comment describing this function

cacheSolve<- function(M, ...) {
  ##  Computes the inverse of the special "matrix"
  ##  returned by makeCacheMatrix above.
  ##  If the inverse has already been calculated 
  ##  (and the matrix has not changed), then 
  ##  should retrieve the inverse from the cache
  cache_inverse<- M$get_inverse()
  if (!is.null(cache_inverse)){
    message("getting cached data")
    return(cache_inverse)
  }
  M1<- M$get_matrix()
  M2<- solve(M1)
  M$set_inverse(M2)
  M2
}

