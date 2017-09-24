## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mtx = matrix()) {
        inv <- NULL
        set <- function(val_y) {
                mtx <<- val_y
                inv <<- NULL
        }
        get <- function() mtx
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}

## cacheSolver function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inv(inv)
        inv
}

## Function test case 
## 1 Sample run
##---------------------------------
##  x <- rbind(c(1, 0.3), c(0.3, 1))
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]  1.0  0.3
## [2,]  0.3  1.0

## No cache in the first run
## > cacheSolve(m)
## [,1]       [,2]
## [1,]  1.0989011 -0.3296703
## [2,] -0.3296703  1.0989011

## Cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1]       [,2]
## [1,]  1.0989011 -0.3296703
## [2,] -0.3296703  1.0989011
##------------------------------
## 2 Sample run
## x <- rbind(c(1, 0, -1/8), c(0, -1/8, 1), c(-1/8, 1, 0))
## m <- makeCacheMatrix(x)
## > m$get()
## [,1]   [,2]   [,3]
## [1,]  1.000  0.000 -0.125
## [2,]  0.000 -0.125  1.000
## [3,] -0.125  1.000  0.000

## No cache in the first run
##> cacheSolve(m)
##            [,1]       [,2]       [,3]
## [1,] 1.00195695 0.12524462 0.01565558
## [2,] 0.12524462 0.01565558 1.00195695
## [3,] 0.01565558 1.00195695 0.12524462

## Cache in the second run
## > cacheSolve(m)
## getting cached data.
##         [,1]       [,2]       [,3]
## [1,] 1.00195695 0.12524462 0.01565558
## [2,] 0.12524462 0.01565558 1.00195695
## [3,] 0.01565558 1.00195695 0.12524462
 