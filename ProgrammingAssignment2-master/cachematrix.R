## Rafael Barros - rafaelcbarros18@gmail.com - 2020/04/20 - Belo Horizonte-MG-Brasil
## Conforme orientações esta atividade será dividida em duas funções: 1) makeCacheMatrix = Cria objeto matriz que pode armazenar seu inverso em cache;
## 2) cacheSolve = Calcula o inverso da matriz retornada pelo método anterior. Se já tiver sido calculado esta função deve recuperar o inverso do cache;

## Esta primeira função basicamente define a matriz x e calcula o seu inverso que neste caso foi definido como a variavel "inverse". Depois que o inverso é calculado
## setei o atributo inverso para o valor e retornei este valor. Logo em seguida os métodos get e set retornam o valor da matriz e depois que o inverso for calculado
## retorna o valor do inverso.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
      set <- function(y) {
      x <<- y
      inverse <<- NULL

      }
      
      get <- function() x
      setSolve <- function(solve) inverse <<- solve
      getSolve <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Na segunda função calculamos o inverso da matriz retornada pela função anterior. Se o inverso foi calculado ou não for nulo e a matriz não mudar então a função
##  cacheSolve retorna o inverso do cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setSolve(inverse)
  inverse
  
}