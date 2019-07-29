#####Cache Matrix cache the matrix for inversing#####
#This function makes the chached Matrix
makeCacheMatrix <- function(x = matrix()) {
    #Safe check if matrix is NULL
    inv <- NULL
	   #Getter and Setter function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function return the inversed matrix


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
	##This does the inverse of the matrix
    inv <- solve(data)
    x$setinverse(inv)
    inv
}