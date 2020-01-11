library(rpart)
library(proto)
getParameters <- function() parent.env(environment())
# we should
itemp <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  parms$i <- 10
  environment(sfun) <- .GlobalEnv
  curEnv=parent.env(environment())
  assign("myspl", 1, envir = curEnv)
  print(paste('mys =', curEnv$myspl))
  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

etemp <- function(y, wt, parms) {
  print('at eval')
  curEnv=parent.env(environment())
  # print(curEnv$myspl)
  curEnv$myspl < - 1
  parms$i[1] <- 10
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)

  # print(paste('wm=',wt[1:5], 'rss=',rss))
  wt[1] <- 100
  list(label = wmean, deviance = rss)
}

stemp <- function(y, wt, x, parms, continuous)
{
  # Center y
  # print('at split')
  # print(parms)
  curEnv=parent.env(environment())
  curEnv$myspl <- curEnv$myspl + 1
  print(curEnv$myspl)
  print(parms$i)
  # print('x')
  # print(x)
  n <- length(y)
  wt[1] <- wt[1] + 1
  print(wt)
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    list(goodness = goodness, direction = sign(lmean), parms=parms)
  } else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    # For anova splits, we can order the categories by their means
    # then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord], parms=parms)
  }
}

ginisplit_d <- function(y, wt, x) {
  y <- y / wt
  n <- length(y)
  x1 <- x[1]
  # vals that on the left
  y_l <- y[x == x1]
  counts_l <- unnamed(tapply(y_l, as.factor(y_l), sum))
  gini_l <- 1 - sum((counts_l / lentth(y_l)) ^ 2)

  y_r <- y[x != x1]

}


ginisplit <- function(y, x, continuous) {
  if(continuous) {

  } else {}
}

mystate <- data.frame(state.x77, region=state.region)
names(mystate) <- casefold(names(mystate)) #remove mixed case

ulist <- list(eval = etemp, split = stemp, init = itemp)
fit1 <- rpart(murder ~ population + illiteracy + income + life.exp +
                hs.grad + frost + region, data = mystate,
              method = ulist, minsplit = 10, parms=list(i = 1))

par(xpd = TRUE)
plot(fit1)
text(fit1, use.n=TRUE)
# f <- function(la) {
#   print(la)
#   la$i <- 10
#   print(la)
# }
# l <- data.frame(i=1)
# print(l)
# f(l)
# print(l)
