## test case(s) for the matrix inversion caching code

source('cachematrix.R')
m <- matrix(c(-1, -2, 1, 1), 2, 2)
x <- makeCacheMatrix(m)
x$get()
# -1 1
# -2 1

inv <- cacheSolve(x)
inv
# 1 -1
# 2 -1

inv <- cacheSolve(x)
# getting cached data
inv
# 1 -1
# 2 -1

