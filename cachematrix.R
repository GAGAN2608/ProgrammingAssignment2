## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  invers <- NULL    #initialize invers as NULL that will hold value of inversed matrix
  set <- function(y) {    # define the set function to assign new 
    x <<- y           # value of matrix in parent environment
    invers <<- NULL     # if there is a new matrix, reset inv to NULL
  }
  get <- function() x   # define the get fucntion - returns value of the matrix argument
  
  
  setinverse <- function(inverse) invers <<- inverse    # assigns value of inv in parent environment
  getinverse <- function() invers   # gets the value of inv where called
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) # you need this in order to refer to function to use cache function
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
