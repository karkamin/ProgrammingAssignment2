## Both functions work in tandem, the first functions allows you to get the inverse as part of the matrix, by using the $, and the second function does the actual computing of getting the inverse.
## I followed the example given in the course of using the <<- to work with variables in different levels of the functions. 
## Following the request of the assignment, the following two functions do as follows: 

## PS: English is not my native lenguage, so they might be some mistakes

## With makeCacheMatrix we do as follows:
## 1. Set the value of the Matrix
## 2. Get the value of the Matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
      inver <- NULL
      set <- function(y){
            x <<- y
            inver <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inver <<- inverse}
      getInverse <- function() {inver}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function does the actual computing of getting the inverse of the matrix we just created
## 1. Get the value of the inverse of the matrix we are refering
## 2. Check if the value of the inverse have already been created
## 3. If the value hasn't been created, it would compute the value of the inverse
## 4. Finally we set the value of the inverse as cache
## The function does have trouble when asked to get the inverse of a matrix that can not be inversed. I should've added an error message that is more friendly to the user.

cacheSolve <- function(x, ...){
      inver <- x$getInverse()
      if(!is.null(inver)){
            return(inver)
      }
      mat <- x$get()
      inver <- solve(mat, ...)
      x$setInverse(inver)
      inver
}

