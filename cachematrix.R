## The first function, makeCacheMatrix creates a special "matrix"
## that can cache its inverse
#-----------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
              M <- NULL
              
              set <- function(y) {
                
                x <<- y
                
                M <<- NULL
              }
      
              get <- function() x
              
              setMatInv <- function(inverse) 
                M <<- inverse
              
              getMatInv <- function() M
              
              list(set = set, get = get,
                   setMatInv = setMatInv,
                   getMatInv = getMatInv)
}

#-----------------------------------------------------------------
## The following function calculates the inverse of the special
## "matrix" created by the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         M <- x$getMatInv()
         
        if(!is.null(M)) {
        message("getting cached data")
        return(M)
        }
         
       dataMat <- x$get()
       M <- solve(dataMat, ...)
       x$setMatInv(M)
       M
}

#-----------------------------------------------------------------
# Exeamples
 Exemplo1 <- makeCacheMatrix(matrix(c(1, 1, 1, 2), 2, 2))
 Exemplo1$get()
# [,1] [,2]
#[1,]    1    1
#[2,]    1    2
 
 Exemplo1$getMatInv()
 #NULL
 
 cacheSolve(Exemplo1)
# [,1] [,2]
#[1,]    2   -1
#[2,]   -1    1

##