## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## When you change the matrix (function set), the invers of this matrix has to be recalculated (The invers object becomes null)


makeCacheMatrix <- function(x = matrix()) {
        xinv<-NULL
	set<-function(y){
	        x<<-y
	        xinv<<-NULL
	}
	get<-function()x
	setinv<-function(trans) xinv<<-trans
	getinv<-function()xinv
	list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        datin1<-x$getinv()
        if(!is.null(datin1)){
                message("getting cached data")
                return(datin1)
        }
        message("inv")
        dat<-x$get()
	datin<-solve(dat)
	x$setinv(datin)
	print(datin)
}
