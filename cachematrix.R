## Assignment 2

## makeCacheMatrix and cacheSolve are used to calculate the inverse of square 
## matrices in large sets of data (e.g. in a loop), making computations faster.
## NOTE THAT BOTH FUNCTIONS ASSUME THE MATRIX IS ALWAYS INVERTIBLE!

## makeCacheMatrix uses four functions: set, get, setinverse and getinverse.
## It basically i) stores the original vector in the "get" function,
## ii) receives results from cacheSolve through the "set" function and "inv" 
## object, 
## iii) stores results from cacheSolve using the "setinverse" function,
## and iv) transport results to cacheSolve through "getinverse" function.

makeCacheMatrix <- function(x = matrix()) {
  
                    inv <- NULL
                    
                    set <- function(y) {
                      x <<- y
                      inv <<- NULL
                    }
                    
                    get <- function() x
                    setinverse <- function(inverse) inv <<- inverse
                    getinverse <- function() inv
                    
                    list(set = set, get = get,
                         setinverse = setinverse,
                         getinverse = getinverse) 
  
}


## cacheSolve is first going to check whether the given matrix (x) already has
## an inv stored in the makeCacheMatrix.If it has, cacheSolve retrieves the 
## value stored in inv. Otherwise, it is going to retrieve the data using "get",
## solve(x) and store the results in "inv" object through the "setinverse"
## function of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  
                inv <- x$getinverse()
                
                if(!is.null(inv)) {
                  message("Getting cached data...")
                  return(inv)
                }
                
                data <- x$get()
                inv <- solve(data)
                x$setinverse(inv)
                
                inv
}
