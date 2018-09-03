incrVal <-
function (x, mini, maxi) {
  n <- x + 1
  if (n > maxi)
    return (c(mini, 1))
  else
    return (c(n, 0))
}
