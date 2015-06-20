  ## makeCacheMatrix, takes a matrix as an argument and convert it into a special Matrix object that both has a data and can also perform function
  ## cacheSolve() returns the inverse of the matrix.

  ##---------------Start of makeCacheMatrix--------------------------------------------------------
  ## It creates a Matrix Object that declare one data variable 'i' (inverse), and four functions
  ## to set and get the value of the matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {

  i<-NULL				## i is the inverse of the matrix
  
  ##-----------------------Start of Comment-------------------------------------------------------
  ##Note on Scoping Rule:       a variable, say x, can be accessed, assigned and returned from an 
  ##					environment, say set() that is defined, but not called, inside an  
  ##					another environment makeMatrix()) where the variable is declared.  
  ##					However '<<' operator is used to assign a value to x inside set().
  ##------------------------End of Comment--------------------------------------------------------
  
  set<-function (y) {					## set() will initialize the value of the  
    x<<-y				              ## Matrix x to y, and its inverse to NULL	
    i<<-NULL		
  }
  
  get<-function()x						## get() will return the value of the Matrix x
  
  setinverse<-function(inverse) i<<-inverse     	## acivating this function means inverse of x   
                                                  ## is not cached, and is calculated separately
                                                  ## and provided to this function as an argument
  
  getmean<-function() i					                  ## getmean () will return the inverse
  
  list(set=set, get=get,					                ## returning value of makeMatrix is 
       setinverse=setinverse,				              ## is a list of all the above four functions 
       getinverse=getinverse)
  
}	
  ##---------------End of makeCacheMatrix-----------------------------------------------------------


  ## ---------------Start of cacheMean()----------------------------------------------------------------
  ## This funcion first checks if inverse is available or not if it is not available, then it calcualtes 
  ## it and cached it as the inverse for that matrix to be used later and finally return the inverse value

cachemean<-function(x,...) {
      i<-x$getinverse()
      if(!is.null(i))      {			        ##Checks if inverse is already available 
            message("getting cached data")
            return(i)
      }
      data<-x$get()
      i<-solve(x, ...)
      x$setinverse(i)
      i
}

  ## ----------------End of cacheMean()-----------------------------------------------------------------
