## Put comments here that give an overall description of what your
## functions do

## Hi, makeCacheMatrix creates a special Matrix, this first function allows us to intialize four nested functions: get() set() getmean() setmean()
##  this enables our funciton to store the input matrix and the output  inverse. 
## cacheSolve, finds inverse matrix for input, fist looking up at stored value, if stored value is NULL, then it is calculated and stored


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL 
      set <- function(y) { ##This function its for changing the  argument, so now we can work with 'y', this will be used in the last code line of cache Solve
             x <<- y
              inv <<- NULL
          }
        get <- function()  ##needs no argumentm it returns  x as stored
          x
        setinverse <- function(inverse)  ## makes a sotarge for inverse, this is not an actual calculated inverse value, it is just a storage value, to be an acutal inverse, it has to be correctly coded in cacheSolve
          inv <<- inverse
        getinverse <- function() inv
       list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  ##with put together all functions as list



}


## Write a short comment describing this function

##acheSolv will make the calculations and recieve the input matrix

cacheSolve <- function(x, ...) { ##starts with argument x, which is a matrix


       inv <- x$getinverse() ##  our previous function 'getinverse() is called, as you remember getinverse could contained a NULL value or a actual value stated as inv
        if(!is.null(inv)) { ##if getinverse does NOT return a NULL value, then we return inv from function of makeCacheMatrix
              message("getting cached data.")
              return(inv)
          }
       data <- x$get() ## if getinvers IS actually NULL in our first function, then 'inv' is assinged to function solve, which calculates inverse
        inv <- solve(data)
      x$setinverse(inv) ## we use our original function, setinverse, to set the value, stating 'inv' as its argument
      inv##finally inv is returned


}
