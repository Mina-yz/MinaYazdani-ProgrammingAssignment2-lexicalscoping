##the point of the following functions is to obtain a matrix x for the first time, 
##calculate its inverse, put it in the cache memory, so that if the next time we
##receive this same matrix, it does not need to calculate the inverse again, and
##but simply retrieve the inverse from memory.

## This function gets the matrix, and considers an empty vector v for the inverse, 
##then with the use of the set()&get(), it places the value of the matrix. later by
##using setinv(), it sets the value of the inverse in variable v in the parent environment,
# and uses getinv() to et the inverse.
## v is the variable for inverse.

makeCacheMatrix <- function(x = matrix()) {
v<-NULL
set<- function(y){
        x<<- y
        v<<- NULL
}
get<- function() x
setinv<- function(inv) v<<- inv
getinv<- function() v
list(set=set, get=get,
     setinv=setinv, getinv=getinv)

}


## with this function, using the outputs of the makeCacheMatrix(), it checks that if the
##x matrix is the same as before (what was in the parent environment), it just gets the
##inverse from memory, and returns it to the parent environment, if not( so if v is not null,
##which is a result of makeCacheMatrix()), then it gets the new matrix, calculates the inverse,
## and returns it to the parent environment.

cacheSolve <- function(x, ...) {
        v<- x$getinv()
        if(!is.null(v)){
                message("getting cached data")
                return(v)
        }
        data<- x$get()
        v<- solve(data)
        x$setinv(v)
        v
}
