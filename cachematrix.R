## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x) {
  # first test if the object provided is a matrix
  if (!is.matrix(x)) {stop("object provided is not a matrix")}
  # local 'inv' variable will host the inverse computed,
  # for starting is defined as NULL
  inv <- NULL
  # set function allows to set new matrix and clears 'inv' previously computed,
  # set in both cases assigns values to his parent enviroment instead locally
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get returns the matrix loaded in the makeCacheMatrix enviroment
  get <- function() x
  # setInverse assigns the inverse of the matrix in the 'inv' variable hosted in
  # the parent makeCacheMatrix enviroment
  setInverse <- function(inverse) inv <<- inverse
  # getInverse returns the 'inv' variable that hosts the inverse of the matrix
  # if it was previously computed
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get the value of 'inv' variable from the 'x' special matrix enviroment
  # and assign it to the local 'inv' variable
  inv <- x$getInverse()
  # test if the value assigned to local 'inv' is NULL
  # if not NULL then returns that value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if the local 'inv' has NULL value then gets the data matrix hosted in the 'x' object
  data <- x$get()
  # next solve the matrix and assign the value to the local 'inv' variable
  # if 'solve' was succesfull will return the inverse of data
  # if it was not, then the 'solve' error handler will come up
  inv <- solve(data, ...)
  # finally the computed value of the inverse of the data matrix now hosted in
  # the local 'inv' variable is passed as argument to 'x$setInverse' and therefore
  # the local 'inv' value is assigned to the 'inv' variable in the enviroment of
  # the 'x' object
  x$setInverse(inv)
  # at last, returns 'inv', the computed value of the inverse of the matrix
  inv
}

# some example data taken from the forum in coursera
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# this two below are for test our error handling
x <- c("a",2)
y <- 1:5

# starts with m1 matrix to create the special matrix with makeCacheMatrix
matrixSpecial <- makeCacheMatrix(m1)
# see if it get the data assigned
matrixSpecial$get()
# lets see what is the value cached as the inverse
matrixSpecial$getInverse()
# next computed the inverse
cacheSolve(matrixSpecial)
# if we try it again, it gets the cached inverse instead computing again
cacheSolve(matrixSpecial)
# and also the inverse has been updated in the special matrix
matrixSpecial$getInverse()

# here we update the data matrix of the special matrix object
matrixSpecial$set(n1)
# and as we updated the matrix, now there is no inverse cached
matrixSpecial$getInverse()
# so lets computed again the inverse
cacheSolve(matrixSpecial)
# one more time,  reading the cached inverse
cacheSolve(matrixSpecial)
# and over again the inverse was saved in our special matrix object
matrixSpecial$getInverse()

# if we try to pass a non matrix object, the function stop and prints error message
x2 <- makeCacheMatrix(x)
y2 <- makeCacheMatrix(y)


## The task was to create two functions that cache the inverse of a matrix.


## The first function creates the matrix, and the special object that 
## will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(x){
    x <<- y
    inver <<- NULL
  }
  get <-- function() {x}
  setInverse <- function(inverse) {inver <<- inverse}
  getInverse <- function() {inver}
  list( set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## The second function computes the inverse of the special "matrix" object.
## The inverse has already been calculated and the matrix has not changed, so
## it will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}
