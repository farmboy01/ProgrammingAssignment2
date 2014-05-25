## ---------------------------------------------------------------------------------------------#
## ******************************** Overview ***************************************************#
## ---------------------------------------------------------------------------------------------#
## Two complementary functions that calculate and cache the inverse of a square matrix 		#
## Cache is checked prior to calculating inverse, if matrix has not changed function will return#
## cached inverse rather than recalcuating it									#
## _____________________________________________________________________________________________#

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        tCache <- NULL #reset
        
	  #function to create matrix
	  set <- function(y) {
                x <<- y	#global 
                tCache <<- NULL #clear global cache
        }
	  #function to return matrix
        get <- function() x
	  
	  #function to set cache to inverse of matrix
        setCache <- function(solve) tCache <<- solve
	  
	  #function to retrieve the cache
        getCache <- function() tCache
        
	  #Create list of subfunctions for working on object
	  list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed) 
## the cache is retrieved in lieu of recalculating the inverse

cacheSolve <- function(x, ...) {
        #Call function of object to get current cache
	  m <- x$getCache()
	  
	  #If cache exists, matrix has not changed so simply return cache
	  #which already contains inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  
	  #If cache does not exist calculate inverse
        data <- x$get()
        m <- solve(data, ...)
	  
	  #Store inverse in cache
        x$setCache(m)
	  
	  #return inverse 
        m
}
