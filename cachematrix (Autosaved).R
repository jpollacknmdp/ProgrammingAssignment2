## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# This Function looks amazingly like the one supplied by Roger Peng.
#  This is modeled on the makeVector Function, but is used for matrices.
#
#  Like the makeVector Function,  this will have a getter,  a setter,
#    like all good object-oirented functions should have,
#    and then it will compute the inverse of itself.
#  A good link to what is the INVERSE of a matrix: http://www.mathsisfun.com/algebra/matrix-inverse.html 
#  
#  The anal-retentive part of me wantst o put the brackets in the 
#  function statements,  even though one-line functions technically
#  don't need it.  Tough.  Did it anyway.

makeCacheMatrix <- function(x = matrix()) {
         im <- NULL    # initialize the inverse cache explicitly to null
         set <- function(y) {  
         	#function that calls function that sets the value.
         	x <<- y
         	im <<- NULL
         }
         get <- function() {x}
         # Get the Inverse of x and put it in the Cache
         setInverse <- function() {im <<- solve(x)}
         # Return the Inverse of X
         getInverse <- function() {im}
         
         list(set = set,  get = get, 
         	  setInverse = setInverse, 
         	  getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
        	message("getting cached matrix")
        	return(m)
        } #end-if branch
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## Write a bunch of Unit Tests to see if I've done this correctly
## http://www.r-tutor.com/r-introduction/matrix/matrix-construction
##
testCache <- function() {
	# Unit Test #1:  Very simple 2x2 matrix
	#
	# 
	#
	#
	B <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
	print("Unit Test #1")
	print(B)
#	makeCacheMxatrix
	
	
	
#> x = makeCacheMatrix()
#> x
#$set
#function (y) 
#{
#    x <<- y
#    m <<- x
#}
#<environment: 0x7fdce1d628a8>
#
#$get
#function () 
#x
#<environment: 0x7fdce1d628a8>
#
#$setInverse
#function (solve) 
#m <<- solve
#<environment: 0x7fdce1d628a8>
#
#$getInverse
#function () 
#m
#<environment: 0x7fdce1d628a8>
#
#
#> x$set(B)
#> x$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> x$setInverse()
#Error in x$setInverse() : argument "solve" is missing, with no default
#> x$getInverse()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> 
	
	
	
	
}