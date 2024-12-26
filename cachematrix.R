# Function to create a special "matrix" object that can cache its inverse
# * set(matrix): Updates the matrix value and resets the cached inverse.
# * get(): Returns the current matrix.
# * setInverse(inverse): Stores the computed inverse of the matrix.
# * getInverse(): Retrieves the cached inverse or returns NULL if not computed.
# * Returns a list of these functions to manage the matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse to NULL
  # Function to set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix is changed
  }
  # Function to get the matrix value
  get <- function() x
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  # Function to get the cached inverse
  getInverse <- function() inv
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix:
# * Computes the inverse of the matrix created by makeCacheMatrix and caches the result.
# * Checks if the inverse is already cached using getInverse().
# * If cached, retrieves and returns the inverse without recalculating.
# * If not cached, computes the inverse using solve(), caches it with setInverse(), and returns the computed result.
# * Takes the special "matrix" object created by makeCacheMatrix as input.
# * Outputs the inverse of the matrix.

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse, if it exists
  inv <- x$getInverse()
  # If the inverse is already cached, return it with a message
  if (!is.null(inv)) {
    message("Fetching cached inverse...")
    return(inv)
  }
  # Otherwise, compute the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse using solve()
  # Cache the computed inverse
  x$setInverse(inv)
  # Return the inverse
  inv
}
