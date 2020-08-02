## This R file has twos function one "makeCacheMatrix" which stores a matrix
## and it's inverse and the second is "cacheSolve" which checks whether a
## Inverse matrix is stored in cache and returns it if it is or finds the 
## inverse matrix if it isn't and returns that.

## makeCacheMatrix initiates two object i and x and four function which it
## returns to the parent envrionment
makeCacheMatrix <- function(x = matrix()) {   # initiates the matrix
  i <- NULL                                   # initiates the inverse of x
  set <- function(y=matrix()) {
    x <<- y     #sets the value x and allows it to be accessed in the parent
                #environment
    i <<- NULL  #set the value i and allows the object to be accessed in the
                #parent environment
  }
  
  get <- function() x # gets the matrix to find the inverse of
  
  setInverse <- function(solve) i <<- solve 
  # The function for setting the Inverse Matrix, i is accessed from the parent
  # envionment
  
  getInverse <- function() i                #the function for getting inverse
                              
  list(set = set, get = get,    # Creates a list of the set, get, setInverse,
       setInverse = setInverse, # and getInverse functions and returns it
       getInverse = getInverse) # for use in the parent environment
}


## Checks for a cache inverse matrix and return the cached matrix if there is,
## if not then calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
    i <- x$getInverse() #Get the inverse of x
    if(!is.null(i)) { #Checks to see if there is a cached matrix
      message("getting cached data")
      return(i) ##Returns the cached inverse matrix of x
    }
    data <- x$get()       ## Gets the matrix i
    i <- solve(data, ...) ## Finds the inverse of x
    x$setInverse(i)  ## cached inverse matrix is set to i
    i     ##Return a matrix that is the inverse of 'x'
  }

