## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                 #initialize inverse as NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x          #function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {inv}
                      #function to opbtain inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#This is used to get the cache data

cacheSolve <- function(x, ...) #gets cache data
  {
  inv <- x$getinv()
  if(!is.null(inv)){          #checking whenther inverse is NULL
    message("getting cached data!")
    return(inv)               #returns inverse value
  }
  data <- x$get()
  inv <- solve(data,...)      #calculates inverse value
  x$setinv(inv)
  inv                         ## Return a matrix that is the inverse of 'x'
}

##Sample run 1:
f <- makeCacheMatrix(matrix(1:4, 2, 2))
f$get()
f$getinv()  
cacheSolve(f)

## Sample run 2:

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)

m$get()
cacheSolve(m)
cacheSolve(m)



