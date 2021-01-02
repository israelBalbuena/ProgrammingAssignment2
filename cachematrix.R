## El objetivo de la tarea es crear dos funciones. La primera debe crear una 
## matriz invertible y guardar ese valor en el cache y no en el ambiente actual
## de trabajo. La segunda función debe evaluar si la matriz invertida ha sido
## calculada y en caso afirmativo devuelve un mensaje; cuando la matriz invertida
## no ha sido calculada, la función la calcula y la imprime. 

## Esta función crea una matriz invertible y guarda el valor de la matriz inversa
## en el cache y no el ambiente actual de trabajo 

makeCacheMatrix <- function(x = matrix()) {
          inversa <- NULL
          set <- function(y) {
                    x <<- y
                    inversa <<- NULL
          }
          get <- function() x
          setInversa <- function(inverse) inversa <<- inverse
          getInversa <- function() inversa
          list(set = set,
               get = get,
               setInversa = setInversa,
               getInversa = getInversa)
}


## Esta función evalúa si la inversa de la matriz ya ha sido calculada. Si la
## inversa ha sido calculada, la función imprime un mensaje; si todavía no está
## calculada, la calcula y la imprime. 

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          inversa <- x$getInverse()
          if (!is.null(inversa)) {
                    message("getting cached matrix")
                    return(inversa)
          }
          data() <- x$get()
          inv <- solve(data(), ...)
          x$setInverse(inversa)
          inversa
}

