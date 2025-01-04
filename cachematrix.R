## This function is aimed at cache the inverse of a matrix 
## to avoid repeated computation


makeCacheMatrix <- function(x = matrix()) { #takes an argument of x
                                            #defaults to an empty matrix
        m <- NULL #initialize m
        set <- function(y) { 
                x <<- y #assign new matrix "y" to "x"; update parent environment
                m <<- NULL #make sure default empty
        }
        get <- function()x  #define get function & return the matrix stored in x
                set_inverse <- function(solve) m <<- solve
                get_inverse <- function() m
                list(set = set, get = get, #update & retrieve the matrix
                     set_inverse = set_inverse, # store the inverse into cache
                     get_inverse = get_inverse) # retrieve the cached inverse
        
}

cacheSolve <- function(x, ...) {
        m <- x$get_inverse() # Return a matrix that is the inverse of 'x'
        if (!is.null(m)) { # check if the inverse has already been calculated
                message("getting cached data")
                return(m) # get it from cache
        }
        data <- x$get()
        m <- solve(data, ...) # calculate the inverse and sets in the cache
        x$set_inverse(m) # via set_inverse function
        m
}

#test function
test_matrix <- matrix(c(2,3,6,7),nrow = 2, ncol = 2) # set a new matrix
print(test_matrix)
cached_matrix <- makeCacheMatrix(test_matrix) # create an cache matrix object
print(cached_matrix$get())
inverse_matrix <- cacheSolve(cached_matrix) # compute the inverse
print(inverse_matrix)
cached_inverse <- cacheSolve(cached_matrix) # test cache
