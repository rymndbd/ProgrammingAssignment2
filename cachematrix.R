## 2 functions taht will cache the inverse of the matrix

## cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        #initializzation
        m<-NULL
        #set matrix
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        #get matrix
        get<-function()x
        #set inverse matrix
        setInverse<-function(inverse)m<<-inverse
        #get inverse matrix
        getInverse<-function()m
        
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)

}
## Compute inverse of special matrix produced by makeCacheMatrix. Check first
## if inverse is already calculated. If so, get inverse from the cache and
## skip computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data)%*%data
        
        #set inverse to object
        x$setInverse(m)
        
        m
}
