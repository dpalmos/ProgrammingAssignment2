## This pair of functions creates a matrix, computes its inverse, and caches that value


## Function 1 creates a matrix and calls four sub-functions to cache the matrix and its inverse.
## Usage: makeCacheMatrix(matrix(data, nrow = , ncol = ))

makeCacheMatrix <- function(x = matrix()) {   # creates matrix, stores matrix in variable 'x'
      
      inv <- NULL   # sets the initial inverse value to NULL as it has not been computed; assign to variable 'inv'
      
      set <- function(y) {   # modifies existing matrix stored in 'x'
            x <<- y   # assigns the value of a new matrix 'y' to matrix 'x'  
            inv <<- NULL   # resets value of 'inv' to NULL because matrix was modified
      }
      
      get <- function()x   # prints matrix currently stored in 'x'
      
      setinverse <- function(solve) inv <<- solve   # cache result of the function solve() in variable 'inv'
      
      getinverse <- function()inv   # prints the value of 'inv' 
      
      list(set=set, get=get, setinverse=setinverse, 
           getinverse=getinverse)   # produces a list of the sub-functions described above
}
      

## Function 2 computes and prints the inverse of the matrix created in Function 1. If the inverse has
## already been computed, it prints that data which was cached in Function 1 as well. 

cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()   # calls 'getinverse' defined in Function 1, which prints value of 'inv'
      if(!is.null(inv)) {   # if 'inv' is not NULL, performs the following:
            message("getting cached data")   # print "getting cached data" in the console
            return(inv)   # print value stored in 'inv'
      }
      
      data <- x$get()   # if 'inv' is NULL, matrix stored in 'x' from Function 1 is assigned to variable 'data'
      inv <- solve(data, ...)   # computes inverse of matrix in 'data'
      x$setinverse(inv)   # calls 'setinverse' defined in Function 1, which caches value of inverse 'inv'
      inv   # prints a matrix that is the inverse of 'x'
}
