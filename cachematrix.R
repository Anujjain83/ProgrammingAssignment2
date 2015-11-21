## AnuJ Jain
## 11-18-2015
## This function creates a special "matrix" object that can cache its inverse

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set_cache the value of the inverse
## 4. get_cache the value of the inverse

## To run the program create a matrix "x", then create another variable of the type y <-makeCacheMatrix(x)
## using that matrix x. 
## Then execute cacheSolve(y) on the new variable.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Creates a blank holding matrix
    hold_matrix = matrix(NA, nrow = nrow(x), ncol = ncol(x))
    
    ## Inputs a matrix
    set <- function(y){
        x <<- y
        hold_matrix = matrix(NA, nrow = nrow(x), ncol = ncol(x))
    }

    ## returns a matrix
    get <- function() x
    
    ## Sets the inverse in cache
    set_cache <- function(mInverse) hold_matrix <<- mInverse
    
    ## Gets the inverse value from cache
    get_cache <- function() hold_matrix
    
    list(set = set, get = get, set_cache = set_cache, get_cache = get_cache)
}


## This function look to see if a inverse of the matrix is already available in cache and returns that value
## or calculates the inverse and returns that value.

cacheSolve <- function(x, ...) {
    
    ## creates a holding matrix to hold the cached inverse
    hold_matrix <- x$get_cache()
    
    ## If the Inverse is cached it return that value from memory
    if(!any(is.na(hold_matrix))){
        message("Getting cached data")
        return(hold_matrix)
    }
    
    ## Otherwise, calculates the matrix inverse
    data <- x$get()
    hold_matrix <- solve(data)
    x$set_cache(hold_matrix)
    
    ## Returning the Inverse Matrix
    hold_matrix
}
