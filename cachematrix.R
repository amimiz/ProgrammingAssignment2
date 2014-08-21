## Solve Inverse Matrix and do it only once - using cache


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # new vector - set the solution cache as null
        set <- function(y) {              # set the new vector - usage "x$set(matrix)"
                x <<- y      
                m <<- NULL
        }
        get <- function() x               # get the vector - usage "x$get()
        setsolve <- function(solve) m <<- solve # set the solution - for internal use only (used by cacheSolve() function
        getsolve <- function() m          # get the inverse matrix (if solved - otherwise will return NULL) - usage "x$getsolve()"
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)       
}

# cacheSolve function calculates the Inverse Matrix, only if it is not solved already (cached) 
cacheSolve <- function(x, ...) { 
        m <- x$getsolve()         #Checks if solved already
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()          #If not solved - solve it
        m <- solve(data, ...)
        x$setsolve(m)           # and store it in Cache
        m
        ## Return a matrix that is the inverse of 'x'
}
