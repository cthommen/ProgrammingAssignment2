# Coursera - R Programming - Assignment 2
# Christoph Thommen, 20.06.2015

# makeCacheMatrix is a function that creates a special "matrix", 
# which is a list containing a function 1. to set the value of the matrix,
# 2. get the values of the matrix, 3. set the inverse of matrix, 
# 4. get the values of the inverse of the matrix

makeCacheMatrix = function(x = matrix()) 
{
      m = NULL
      set = function(y) 
	{
      	x <<- y
            m <<- NULL
      }
      get = function() x
      setinverse = function(solve) m <<- solve
      getinverse = function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

# The cacheSolve function calculates the inverse of the special "matrix" 
# created with the makeCacheMatrix function. This function checks if the
# inverse has already been calculated. If so, it gets the value from the 
# cache and skips the computation. If not, it calculates the inverse of 
# the data and sets the inverse of the matrix with the setinverse function.

cacheSolve = function(x, ...) 
{
      m = x$getinverse()
      if(!is.null(m)) 
	{
      	message("getting cached data")
            return(m)
      }
      data = x$get()
      m = solve(data, ...)
      x$setinverse(m)
      m
}
