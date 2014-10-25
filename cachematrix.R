## solve is a function in R that can be used to calculate the inverse 
## of a matrix. However, this calculation is computer-intensive, so it is
## a good strategy to store the inverse of the matrix (and not to calculate
## it every time) if we need to use it in several parts of the program.
## The function makeCacheMatrix and cacheSolve can be used to solve the inverse
## of a matrix and store its value in cache. When this inverse is needed, the
## functions return its value.

## makeCacheMatrix creates a list of functions which allow for the storage
## of the inverse of a matrix in cache. Matrix x and its inverse, Inv, are defined,
## so they can be accessed from outside of the function makeCacheMatrix.

makeCacheMatrix<-function(x=matrix()){
	Inv<-NULL
	set<-function(y){
		x<<-y
		Inv<<-NULL
	}
	get<-function()x
	setInv<-function(solve) Inv<<-solve
	getInv<-function() Inv
	list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## cacheSolve checks whether the inverse of a requested matrix is stored in cache.
## If it is, it returns its value. If it is noy, it computes it and then it 
## calls the setInv() function from the list created by makeCacheMatrix to cache the
## result. In either case, it returns the matrix's inverse.

cacheSolve<-function(x,...){
	Inv<-x$getInv()
	if (!is.null(Inv)){
		message("getting cached data")
		return(Inv)
	}
	data<-x$get()
	Inv<-solve(data,...)
	x$setInv(Inv)
	Inv
}

