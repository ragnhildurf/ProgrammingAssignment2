## These two functions create a special matrix object that coan cache its inverse
## and commpute the inverse of the special matrix. If the inverse has already 
## been calculated, then the later function should retrieve the inverse from
## the cache.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {     #x is a matrix
        inv <- NULL                             #Setting the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x                     #Get the value of the matrix x
                                                #Storing the invese as inv        
        setinverse <- function(inverse) inv <<- inverse   
        
        getinverse <- function() inv            #Get the inverse
        
        list(set = set, get = get,              #Return a list with 4 functions
             setinverse = setinverse, 
             getinverse = getinverse)           

}


## This function uses makeCacheMatrix in it's implemementation. The function 
## checks if the inverse has already been calculated and cached. If it hasn´t,
## the function calculates the it matrix and saves the result back to x´s cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()                   #query the x matrix cache 
        if(!is.null(inv)) {                     #if there is a cach then give 
                                                #message and regurn inv 
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                         #if not, then solve the matrix 
        inv <- solve(data, ...)                 #and calc inv
        x$setinverse(inv)                       #save the inv as x (cache)
        inv                       ## Return a matrix that is the inverse of 'x'
}
