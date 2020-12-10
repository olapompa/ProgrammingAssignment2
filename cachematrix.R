## makeCacheMatrix and cacheSolve are a pair of functions that calculate an inverse of a matrix. If the inverse 
##already exists, the old one is printed instead of recalculating it

## makeCacheMartix contains a function that after taking up a new matrix erases the cached inverse of a 
##previously loaded matrix. Also, it contains a list of functions that are fed to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL                                   #creates empty object i 
  set <- function(y) {                        #if new matrix loaded:
    x <<- y                                   #assigns input to an object outside of parent function
    i <<- NULL                                #nullifies stored inverse outside function environment
  }
  get <- function() x                         #function will print loaded matrix
  setinverse <- function(solve) i <<- solve   #function will write solved inverse outside function environment
  getinverse <- function() i                  #feeds inverse (old or NULL) from parent environment to cacheSolve
  list(set = set, get = get,                  #defines a list of functions used by cacheSolve 
       setinverse = setinverse,                  
       getinverse = getinverse)
}


##cacheSolve checks if the inverse already exists. If yes, it prints the already calculated value with 
##a message that it did so. If the inverse is NULL, it takes the functions from the list makeCacheMatrix
##and calculates the inverse de novo

cacheSolve <- function(x, ...) {              ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()#                        #loads inverse object from parent environment
  if(!is.null(i)) {                           #checks if inverse value was nullified by the makeCacheMatrix
    message("getting cached data")            #if not, returns message about retrieving previously calculated inverse
    return(i)                                 #and returns the cached data
  } 
  
  #below is basically an else clause (when the inverse value was nullified by makeCacheMatrix)
  data <- x$get()                             #retrieves input matrix from makeCacheMatrix
  i <- solve(data, ...)                       #solves the matrix (inverses it)
  x$setinverse(i)                             #
  i                                           #and prints freshly calculated inverse
}

####usage
#example <- makeCacheMatrix(example.matrix)  
#cacheSolve(example) #calculates the inverse
#cacheSolve(example) #when run second time, it returns cached inverse