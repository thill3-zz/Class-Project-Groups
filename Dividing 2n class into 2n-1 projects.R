genGroups <- function (n) {
  if ((n %% 2) != 0) {
    return('Will not work')
  }
  n <- n/2
  projMat <- matrix(rep('bb', (2 * n - 1) ^ 2), nrow = (2 * n - 1))
  m <- 2 * n - 1
  for (d in 1:m) {
    for (i in 1:m) {
      for (fill in 1:m) {
        if (d + i - 1 != m) {
          if (i < d) {
            projMat[(d + i - 1) %% m, i] <- paste(i, d, sep = ',')
          } else if (i == d) {
            projMat[(d + i - 1) %% m, i] <- paste(i, 2*n, sep = ',')
          } else {
            projMat[(d + i - 1) %% m, i] <- 'x'
          }
        } else {
          if (i < d) {
            projMat[m, i] <- paste(i, d, sep = ',')
          } else if (i == d) {
            projMat[m, i] <- paste(i, 2*n, sep = ',')
          } else {
            projMat[m, i] <- 'x'
          }
        }
      }
    }
  }
  projMat
}
g10 <- data.frame(genGroups(10))
g10
g8 <- data.frame(genGroups(8))
g8