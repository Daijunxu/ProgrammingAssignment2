## Those functions try to cache matrix inverse if it's calculated, they can save
## your overall time if your analysis requires calculating marix inverses repeatly

## This function structures a list to include the matrix and its inversed matrix, to 
## make caching possible, it also uses a sub function setinv to cache the inverse.

makeCacheMatrix <- function(x=matrix(numeric())) {
        inv <- matrix()
        set <- function(y) {
                x <<- y
                inv <<- matrix()
        }
        
        get <- function() x
        setinv <- function(matrixInverse) inv <<- matrixInverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function calls function solve() in R to compute the inverse of a matrix,
## Besides, it saves inverse when it's computed for the first time


cacheSolve <- function(x,...) {
        inv <- x$getinv()
        if(!is.na(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
