## Author: Marçal Nebot


#' 
#' This function creates a special "matrix" object that can cache its inverse
#'
#' @param x the matrix
#'
#' @examples makeCacheMatrix(a)
#' 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve should retrieve the 
#' inverse from the cache
#'
#' @param x the matrix cached or not
#' @param ... the ... parameters for solve function
#'
#' @return the inverse of x
#'
#' @examples cacheSolve(a)
#' 
cacheSolve <- function(x, ...) {
    
        # get the cached inverse
        i <- x$getinverse()
        
        # if isn't cached calculate the inverse and cache it
        if   (is.null(i)) {
            i <- solve(x$get(), ...)
            x$setinverse(i)
            message("Inverse cached")
        }else{
            message("Inverse not cached")
        }
        
        # Return the inverse
        i
        
}
