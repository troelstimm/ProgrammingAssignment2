#The 2 functions work together, one holding the original matrix and if the cahceSolve has been called
# also a cached version of the matrix in a inverted version.

#The following is a complete execution example flow.
# > x <- matrix(1:4, nrow = 2, ncol = 2)
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > source("cachematrix.R")
# > test <- makeCacheMatrix(x)
# > s <- cacheSolve(test)
# > s
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > s2 <- cacheSolve(test)
# getting cached data
# > s2
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 




#A storage function, which can hold the matrix x and a cached matrix myCache
#myCahce is reset if the set function is called or if 
#makeCacheMatrix is called with a new matrix
makeCacheMatrix <- function(x = matrix()) 
{

    myCache <- NULL
 
    #replace the original matrix 
    set <- function(y)
    {
        x <<-y
        myCache <<- NULL
    }
  
    #return the value of the matrix x
    get <- function() { x } 
    
    #store a cached version of the matrix inversedData
    setInverse <- function(inversedData) { myCache <<- inversedData }
    
    #return the value of the stored matrix myCache
    getInverse <- function() { myCache }
  
    #return the functions to the working environment
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#Creating the inverse of a matrix by using the R solve function
#The input are given from the makeCacheMatrix, which enables the result of the Solve to
"be stored in a cache"
cacheSolve <- function(x, ...) {
    myMatrix <- NULL
    
    #To ensure that the input x is in fact the output of makeCacheMatrix (and not a matrix itself)
    tryCatch( 
    {
        myMatrix <- x$getInverse()
    },
    error = function(e) {
        message("The input is expected to be compatible with makeCacheMatrix and contain $getInverse Error:")
        message(e)
        return(NA)
    }
     )
    
    if(!is.null(myMatrix))
    {
        message("getting cached data")
        return (myMatrix)
    }
    myData <- x$get()
    tmpInverse <- solve(myData)
    x$setInverse(tmpInverse)
    tmpInverse
}


