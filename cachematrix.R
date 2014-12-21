##This code solves the inverse of a given matrix.
##Once the value of a given matrix is passed through
##the makeCacheMatri function, it can then be passed to
##the cacheSolve function.
##Caching the matrix first frees up processing time by
##eliminating redundant steps.

##This makeCacheMatrix function gets and sets the vaue of the matrix
##as long as the inverse has not been already calculated

makeCacheMatrix <- function(x = matrix()) {
	matrixinv<-NULL

	set<-function(y){
		x<<-y
		matrixinv<<-NULL
	}
	get<-function() x
	setmatrixinv<-function(solve) matrixinv<<-solve
	getmatrixinv<-function() matrixinv
	list(set=set, get=get, 
	setmatrixinv=setmatrixinv, getmatrixinv=getmatrixinv)
}


##This cacheSolve function calculates the inverse of a matrix
##as long as the calculation has not already been performed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	matrixinv<-x$getmatrixinv()
	if(!is.null(matrixinv)){
		message("getting cached matrix")
		return(matrixinv)
	}
	data<-x$get()
	matrixinv<-solve(data) %*% data
	x$setmatrixinv(matrixinv)
	matrixinv
}
