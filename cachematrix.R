## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() function will take a vector, which is a matrix
# and store its inverse (which isn't calculated here) along with other functions
#that are listed under the main function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve(makeCacheMatrix(insert matrix here)) will go into the makeCacheMatrix()
#function and pick out getinverse() and store it in i


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                #if is.null(i) is TRUE (inverse does not previously exist)
                #then !is.null(i) would be FALSE
                message("getting cached data")
                return(i)
        }
        #this is executed if is.null(i) is TRUE (previous inverse value does not
        #exist), meaning !is.null(i) is FALSE and the if statements do not execute
        data <- x$get()
        i <- solve(data,...)
        #this gets the inverse of the said matrix 
        x$setinverse(i)
        #the inverse of the matrix is set under vector i
        i
        #print the inverse matrix vector i
}
