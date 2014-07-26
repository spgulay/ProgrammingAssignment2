# I have two functions that store a matrix, and its inverse, and prints out the 
# inverse either already stored or newly computed.

# makeCacheMatrix(x) stores the matrix x and its inverse. This is based on
# makeVector example function.

makeCacheMatrix <- function(x = matrix()) {
        # in the beginning cache is empty
        cache <- NULL
        # set the matrix with a value
        set <- function(mmatrix) {
                x <<- mmatrix
                # empty the cache when the matrix changes
                cache <<- NULL
        }
        # get the original matrix (not inverted)
        get <- function() x
        # set the cache value
        setinverted <- function(data_cache) cache <<- data_cache
        # get the cache value
        getinverted <- function() cache        
        # list the methods of the function
        list(set = set, get = get, 
             setinverted = setinverted, 
             getinverted = getinverted)
}

# cacheSolve(x, ...) checks if an inverted matrix is already stored in cache
# before inverting the matrix x, and storing the data. This is based on 
# cachemean example function. Returns the inverted matrix.

cacheSolve <- function(x, ...){
        # check if anything is stored in cache already
        cache <- x$getinverted()
        if(!is.null(cache)){
                message("got cached data")
                return(cache)
        }
        # if cache empty, solve the matrix inversion and store for later
        data <- x$get()
        cache <- solve(data, ...)
        x$setinverted(cache)
        return(cache)
}