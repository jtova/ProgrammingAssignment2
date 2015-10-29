## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}

# pruebas
m <- matrix(c(5,6,7,8 ),nrow = 2, ncol = 2 )
m
im <- matrix(c(-4,3,3.5,-2.5),nrow = 2, ncol = 2 )
im


mm <- makeCacheMatrix(m)
str(mm)

mm$get()
mm$getinverse()


cim <- cacheSolve(mm)
# Test
all.equal(cim, im)

