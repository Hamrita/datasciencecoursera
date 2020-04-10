
## The following is a two functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     INV <- NULL
     # set the value of the matrix
       set <- function(y) {
           x <<- y
           INV <<- NULL
         }
     # get the value of the matrix
     get <- function() x
     # set the value of the inverse
     setinv <- function(inverse) INV <<- inverse
     # get the value of the inverse
     getinv <- function() INV
     list(set = set, get = get,setinv = setinv, getinv = getinv)
   }

 ## This function computes the inverse of the special
 ## "matrix" returned by `makeCacheMatrix` above. If the inverse has
 ## already been calculated (and the matrix has not changed), then
 ## `cacheSolve` should retrieve the inverse from the cache.
  
 cacheSolve <- function(x, ...) {
    INV <- x$getinv()
     if(!is.null(INV)) {
        message("getting cached data")
       return(INV)
       }
    data <- x$get()
    INV <- solve(data, ...)
    x$setinv(INV)
    INV
 }

 ##  tests with expected output
 
 mm=makeCacheMatrix(matrix(1:4,2,2))
 mm$get()
 
##       [,1] [,2]
## [1,]   -1    2
## [2,]    0   -3

 cacheSolve(mm)  # Computes, caches, and returns    matrix inverse
 
##      [,1]       [,2]
## [1,]   -1 -0.6666667
## [2,]    0 -0.3333333
 
 mm$getinv()  # get matrix inverse

##      [,1]       [,2]
## [1,]   -1 -0.6666667
## [2,]    0 -0.3333333
 
 cacheSolve(mm)   ## Returns cached matrix inverse using previously computed matrix inverse
 
## getting cached data
##      [,1]       [,2]
## [1,]   -1 -0.6666667
## [2,]    0 -0.3333333
 
 mm$set(matrix(c(1,0,-1,2),2,2))  # modify existing matrix
 
 mm$get()  # get the new matrix
 
 ##      [,1] [,2]
 ## [1,]    1   -1
 ## [2,]    0    2
 
 cacheSolve(mm)
 
##       [,1] [,2]
##  [1,]    1  0.5
##  [2,]    0  0.5
 
 mm$getinv()

##      [,1] [,2]
## [1,]    1  0.5
## [2,]    0  0.5 

 cacheSolve(mm)
 
## getting cached data
##      [,1] [,2]
## [1,]    1  0.5
## [2,]    0  0.5 
 