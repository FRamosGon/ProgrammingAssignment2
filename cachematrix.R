## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function
## makeCacheMatrix es una función que carga una lista de funciones:
## 1. set: función que cambia la matriz cargada en la función principal.
## 2. get: función que devuelve la matriz cargada en la función principal.
## 3. setsolve: carga el valor que se le pasa como parámetro en una variable "m"
##              del entorno de makeCacheMartix
## 4. getsolve: devuelve el valor de la variable "m" del entorno de 
##              makeCacheMatrix
## Para acceder a las funciones de makeCacheMatrix se utiliza:
## FuncionPrincipal + $ + NombreDeLaSubfunción
## Por ejemplo: makeCacheMatrix$get()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## cacheSolve devuelve la matrix inversa de 'x'
## Comprueba que el valor de m, almacenado previamente con 
## makeCacheMatrix$getsolve existe y no el NULO. Si es NULO se termina.
## Si no es NULO, se carga en 'data' el valor almacenado con makeCacheMatrix,
## se carga en 'm' la matriz invertida, se almacena en cache el valor de 'm'
## y se devuelve la matriz invertida.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## Ejemplo de ejecución:
## > f<-matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
## > a<-makeCacheMatrix(f)
## > cacheSolve(a)
## El resultado es:
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1