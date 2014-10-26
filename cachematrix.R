## This program supports Assignment: Caching the Inverse of a Matrix - R programming Coursera 2nd program Assignment
## Program demonstrates how to write an R function that is able to cache potentially time consuming computations
## This code demonstrates caching an inverse of a matrix.
## Primary source code structure from example code - Caching the Mean of a Vector; Debug credit stackoverflow / Goggle hacking..


## The MakeCacheMatrix itializes the variable for caching data (m); provides functions that will get set/get the value of the vector and the matrix 
MakeCacheMatrix <- function(x = matrix()) {
        ## Establishes cache variable
        m <- NULL
        ## provides functions that set/get the value of matrix and inverse matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                
                get <- function() x
                setmatrix <- function(data) m <<- data
                getmatrix <- function() m
                
                ## returns a list of functions in list format
                list(set = set, get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
                }
        


## The CacheSolve function inverses matrix X.  Prior to the potentially costly calculation of an inverse matrix it checks 
## to see if the results have previously been run and are cached in memory.  If yes it returns cached results an notification; 
## If no results are cached, it calculates inverse, stores results in cache, and returns result

CacheSolve <- function(x=matrix(), ...) {
               
                m <- x$getmatrix()
                ## checks to see if data has already been cached
        
                if(!is.null(m)) {
                        message("Getting cached data")
                        return(m)
                }
                
                else {
                ## executes inverse if data isn't cached
                message("No cached data found. Calculating...")
                matrix <- x$get()
                m <- solve(matrix, ...)
                x$setmatrix(m)
                message("Finished calculation of inverse matrix")
                return(m)
                }
        }
        
        
