##Assignment for week 3 
##Single arrow can be used on the base level, double arrow used further in the function.
##There are two functions involved named "makecachematrix" and "cachesolve"

#The Makecachematrix function creates a matrix that will cache its inverse
#The function consists get, set, setinv and getinv in order to set and
#return the object created and the inverse of that object.


library(MASS)
makecachematrix <- function(x =matrix()) {
  inv<-NULL     #This ensures the inverse is initialized as null
  set<-function(y){   
    x<<-y
    inv<<-NULL
  }
  get<-function()x      #This is for returning the matrix x 
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x       #This function will obtain the inverse
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)   #A list for what is involved in the function
}

#The function cachesolve returns the result that is the inverse of matrix "x"

cachesolve <- function(X, ...){
  inv <- X$getinv()
  if(!is.null(inv)){  #This will check whether the inverse is null
    message("getting cached data") #This creates a message that the cached data is being returned
    return(inv)       #This will return the inverse value for matrix "x"
  }
  mat <-X$get()
  inv <- solve(mat, ...)  #This will solve what the inverse value is
  X$setinv(inv)
  inv
}

##Example to run to see if the function is running correctly, this is 
#assigned the name "example1" and consists of a matrix with two rows and 
#4 columns accounting from 1 to 8. The inverse of this matrix is returned
#when the cachesolve function is run on the matrix.

example1 <- makecachematrix(matrix((1:8), nrow = 2, ncol = 4))
example1$get()
cachesolve(example1)
