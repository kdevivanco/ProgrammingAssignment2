## The following functions allow us to cache the value and inverse of a matrix. Because calculating the inverse of a matrix can be a costly computation, 
## it is usefull to store the values so they can be cached instead of computing the inversion every time


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
invertible <- NULL 
  set <- function(y){ #the set function is created to set the value of the matrix
    x <<- y         #assigning a value to a x in the funct. enviornment
    invertible <<- NULL
  }
  get <- function()x
  #now the value of the inverse is set and get...
  setInverse <- function(inverse) invertible <<- inverse
  getInverse <- function() invertible 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function invertes the matrix and stores the result in the cache, once inverted the value of the inv. matrix will always be retrieved from the cache
## and along this, we will get a message "getting cached data", this is how we know it is cached.

cacheSolve <- function(x, ...) {
    invertible <- x$getInverse() #assinging the inverse of x to the variable "invertible"
      #if the value has allready been calculated it will skip computation, for that I write the following function:
   if(!is.null(invertible)){   
    message("getting cached data") 
    return(invertible)
  }
  matrix <- x$get()
  invertible <- solve(matrix,...) # I use the function "solve" for getting the inverse of the matrix
  x$setInverse(invertible) # I use the setInverse function to set the inverse in the cache
  invertible
}




## then I test if my functions are working... 
## this following text is not part of the assignment but I considered it relevant to show 
> matrixtest<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
> matrixtest$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4

> cacheSolve(matrixtest)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> matrixtest$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> cacheSolve(matrixtest)
getting cached data  #this proves that now the function retrieves the data from the cache
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

