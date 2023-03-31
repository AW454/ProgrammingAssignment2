## MakeCachematrix
## Get inverse

## set Matrix
setwd("~/Downloads/rprog-data-ProgAssignment3-data/hospital-data.csv")
makeCacheMatrix <- function(x=matrix()) 
i<-NULL 
setAs(-function(y)
  x<<-y)
get<-function()x        #function to get matrix x
mean(function(x)m<<-mean
getMethod(function() m))
list(set=set , get=get,
     setmean = setmean,
     getmean = getmean) ##List of return methods
getinv<-function(){
		inver<-ginv(x)
		inver%*%x         #function to get inverse of matrix
		}

cachemean <-function(x,...) {
  m <-x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <-mean(data, ...)
  x$setmean(m)
}


## Method setting mean function
##This is used to get cache data

cacheSolve <- function(x, ...)  ##gets cache data
{
	inv<-x$getinv()
	if(!is.null(inv)){         #check whether inverse is Null
		message("getting cached data!")
		return(inv)           #returns inverse value
		}
		data<-x$get()
		inv<-solve(data,...)       #calculates inverse value
		x$setinv(inv)
        inv    ## Return a matrix that is the inverse of 'x'
}

