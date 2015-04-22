## There are two functions created 1. to cache already created inverse and 2. to calculate inverse of matrix

##makeCacheMatrix function creates a matrix which is a list containing a function to 
##1. set matrix
##2. get matrix
##3. set inversematrix
##4. get inversematrix
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <- NULL
        }
        get <- function() x
        setinversematrix <- function(inverseMatrix) m <<- inverseMatrix
        getinversematrix <- function() m
        list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}

##cacheSolve function calculates inverse of a matrix using solve function as below
##1. check if the matrix inverse is already calculated then return cached inversematrix
##2. get source matrix data and apply solve on the matrix to return inverse
##3. solve is wrapped in try catch to return friendly error if the matrix is not square matrix or if inverse cannot be calculated
cacheSolve <- function(x, ...){
        m <- x$getinversematrix()
        if(!is.null(m)){
                message ("returning cached data")
                return (m)
        }
        data <- x$get()
        m <- tryCatch( solve(data), error =function(e) e)
        any(class(m) == "error")
        m
}