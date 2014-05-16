#
# ---- IMPLEMENTATION OVERWIEW --------------------------------------
# 'makeCacheMatrix' constructs a "special" matrix from the given
# "ordinary" matrix as a list of functions that set or get
# the original matrix and its inverse in the parent frame so that 
# their values are accessible by caller functions for retrieving
# as long as these values have been already computed. 
# This way we implement a memoization or caching technique that 
# improves performance by avoiding the re-computation of
# potentially costly operations.
#
# 'cacheSolve' calls 'makeCacheMatrix' to take advantage of
# its caching support in order to compute the matrix inverse.
# If the inverse has been already computed, it is retrieved
# from the cache (parent frame); otherwise, 'cacheSolve' 
# computes the inverse by calling the built-in function solve 
# and the result of the computation is immediately stored in
# the cache for later reuse.
#
# ---- ASSUMPTIONS --------------------------------------------------
# The matrix passed to 'makeCacheMatrix' is a square and an 
# invertible matrix.
#
# ---- ERROR HANDLING (Optional) ------------------------------------ 
# If an empty matrix is passed to 'makeCacheMatrix'execution stops 
# with an appropriate error message. 
# The argument for 'makeCacheMatrix' is set to an empty matrix by 
# default just to make explicit the kind of data this function 
# recieves, and at the same time to simplify the code for error 
# handling (but it is not strictly needed).
# Error handling for non-square or non-invertible matrices is
# not implemented (relying on the assumption).
#
# ---- NOTE about the 3-dots arg ------------------------------------ 
# There is no need to declare 'cacheSolve' with the three
# dot construct ('...') as formal argument. In fact, the built-in
# solve function should not be called with more arguments than
# the first and mandatory one if we just want it to compute the
# matrix inverse (See ?solve for more information).
#
# ---- ADD-ON - CleanCache function (Optional) ----------------------
# A function 'cleanCache' is implemented as an add-on 
# to show the usefulness of the 'set' function in 'MakeCacheMatrix'
#
# ---- TESTING ------------------------------------------------------ 
# A function prototype to test this set is given below.
# Commented out test cases are also given as examples.
# Uncomment them if you wish to run these examples.
# Better test coverage would be advisable, as well as executable
# test units via, for instance, the 'testthat' library.
#
makeCacheMatrix <- function(x=matrix(numeric(), 0, 0)) {
	# Construct a "special" matrix as a list of functions
	# (getters and setters) from the given "ordinary" matrix
	# 
	# Args
	#     x square matrix (assumed: invertible)
	# Return
	#     list of functions

	# Raise error if matrix is empty 
	if (identical(dim(x), as.integer(c(0, 0)))) {
		stop("empty matrix")
	}
	
	inverse <- NULL

        # Put the given matrix into the cache and clean its inverse 
	# (cache initizalization)
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# Get the cached matrix
	get <- function() x

	# Set the matrix inverse in the cache to the given value 
	setinverse <- function(solved) inverse <<- solved

	# Get the cached matrix inverse 
	getinverse <- function() inverse 

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x) {
	# Produce the inverse of the given matrix
	# If the inverse is already in the cache, retrieve it; 
	# otherwise, compute it and store it into the cache.
	#
	# Args
	#     x a list of functions (the "special" matrix)
	# Return
        #     the inverse matrix
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse
}

#
# ---- ADD-ONs ------------------------------------------------------ 
cleanCache <- function(x) {
	# Reset the cache: store the given matrix into the cache
        # and clean its cached inverse.	
        #
        # Args	
	#     x a list of functions (the "special matrix")
	# Returns
	#     NULL
	message("\ncleaning...\n")
	x$set(x$get())
}

#
## Tests 
testCacheMatrix <- function(x) {
	# Test wrapper 
	#
	mat <- makeCacheMatrix(x)

	print(cacheSolve(mat))
	print(cacheSolve(mat))
	cleanCache(mat)
	print(cacheSolve(mat))

}

#
## Test examples (to run one at a time from within the console)
# Normal cases
#testCacheMatrix(matrix(1, 1))
#cat("------------------------\n\n")
#
#testCacheMatrix(matrix(1:4, 2))
#cat("------------------------\n\n")
#
#testCacheMatrix(matrix(rnorm(9), 3))
#cat("------------------------\n\n")
#
## Testing error handling
#testCacheMatrix(makeCacheMatrix())
#cat("------------------------\n\n")
