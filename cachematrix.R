#Programming Assignment 2
#Make inverse matrix and cache of that matrix



makeCacheMatrix <- function(x = matrix()) {     #Initialize object x, empty matrix
  m<-NULL                                       #Initialize object m
  set<-function(y){                             #
  x<<-y                                         # Assign y to x which is in parent environment
  m<<-NULL                                      # Assign NULL to m which is in parent environment
  
}
get<-function() x                               # Define "getter" for x. R retrieves from parent environment of makeVector()
setmatrix<-function(solve) m<<- solve           # Sets input argument to m in the parent environment
getmatrix<-function() m                         # "Getter" of the inverse matrix
list(set=set, get=get,              
   setmatrix=setmatrix,
   getmatrix=getmatrix)                         # Assigns names to the functions. So that they can be called by $ in cacheSolve.
}

cacheSolve <- function(x=matrix(), ...) {       # Initialze object x, empty matrix. Plus room for other arguments.
    m<-x$getmatrix()                            # calls getmatrix() on input argument x
    if(!is.null(m)){                            # check to see if getmatrix has a value for x in the parent environment of makeCacheMatrix. If returns FALSE for !is.null then moves down to solve for the inverse. If TRUE, returns the value of m from makeCacheMatrix parent environment.
      message("getting cached data")
      return(m)
    }
    datos<-x$get()                              # Retrieves x from makeCacheMatrix
    m<-solve(datos, ...)                        # Calculates the inverse matrix
    x$setmatrix(m)                              # Sets the inverse matrix of x
    m                                           # Returns the value to parent environment
}




