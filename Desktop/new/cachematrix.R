## makeCacheMatrix is a function that stores a blank matrix in x
## and stores 4 other functions in a list
##function1 1: check_set() : this will checkif the submitted matrix is already present in the system
## if yes thenit will retrieve the value ofthe already calculated inverse and return
## otherwise throws NULL value.

##function 2: get():- thisjust returns thevalue of the matrix

## function 3: setinverse(): stores the value of the newly calculated inverse matrix to the global variable m

## function 4: getinverse(): returns the value of this inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        check_set <- function(y)
        {       
                assign("q",  y, envir = .GlobalEnv)
                
                if (exists("data")==TRUE)
                {
                        if (identical(q,data)==TRUE)
                                
                        { x <<- q
                        m <<- m                        
                        }
                        
                        else { x <<- q
                        m <<- NULL                        
                        }
                }
                else { x <<- q
                m <<- NULL                
                }
                assign("x", x, envir = .GlobalEnv)
                assign("m", m, envir = .GlobalEnv)
        }
        
        get <- function() x
        
        setinverse <- function(inverse)
        { m <<- inverse
        assign("m", m, envir = .GlobalEnv)
        }
        
        getinverse <- function() m 
        
        v<<-list(check_set = check_set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
        
}


## This function cacheSolve() checks if the inverse for the submitted matrix has already been
##calculated then returns the value of the inverse matrix.
## otherwise calculates the matrix and stores in cache.

## Return a matrix that is the inverse of 'y' having values like 1:4 (to be written in 'val')
## eg. cacheSolve(1:4)

cacheSolve <- function(val,...) {
        
        y<-matrix(val,nrow=2,ncol=2)
        
        v$check_set(y)
        
        m <- v$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- v$get()
        assign("data", data, envir = .GlobalEnv)
        m <- solve(data, ...)
        
        v$setinverse(m)
        
        v$getinverse()
} 