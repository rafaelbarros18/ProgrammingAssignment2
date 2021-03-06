## Rafael Barros - rafaelcbarros18@gmail.com - 2020/04/20 - Belo Horizonte-MG-Brasil
## Conforme orienta��es esta atividade ser� dividida em duas fun��es: 1) makeCacheMatrix = Cria objeto matriz que pode armazenar seu inverso em cache;
## 2) cacheSolve = Calcula o inverso da matriz retornada pelo m�todo anterior. Se j� tiver sido calculado esta fun��o deve recuperar o inverso do cache;

## Esta primeira fun��o basicamente define a matriz x e calcula o seu inverso que neste caso foi definido como a variavel "inverse". Depois que o inverso � calculado
## setei o atributo inverso para o valor e retornei este valor. Logo em seguida os m�todos get e set retornam o valor da matriz e depois que o inverso for calculado
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

## Na segunda fun��o calculamos o inverso da matriz retornada pela fun��o anterior. Se o inverso foi calculado ou n�o for nulo e a matriz n�o mudar ent�o a fun��o
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