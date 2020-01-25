

itempg <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          "levels=", ylevel,sep = '')
  }
  initSampling(parms)

  environment(sfun) <- .GlobalEnv
  numr <- length(levels(y)) + 1
  priori <- table(y) / length(y)
  parms$priori <- priori
  parms$classes <- length(levels(y))
  list(y = c(y), ylevels=(levels(y)), parms = parms, numresp = numr, numy = 1,
     print = function(yval, ylevel, digits) {
    temp <- if (is.null(ylevel)) as.character(yval[, 1L])
    else ylevel[yval[, 1L]]

    nclass <- ncol(yval) - 1L
    yprob <- if (nclass < 5L)
      format(yval[, 1L + 1L:nclass],
             digits = digits)
    else format(yval[, 1L + 1L:nclass], digits = 2L)
    if (!is.matrix(yprob)) #this case only occurs for no split trees
      yprob <- matrix(yprob, nrow = 1L)

    temp <- paste0(temp, " (", yprob[, 1L])
    for (i in 2L:ncol(yprob)) temp <- paste(temp, yprob[, i], sep = " ")
    temp <- paste0(temp, ")")
    temp
  },
  summary = function(yval, dev, wt, ylevel, digits) {
    nclass <- ncol(yval) - 1L
    group <- yval[, 1L]
    yprob <- yval[, 1L + 1L:nclass]
    if (!is.null(ylevel)) group <- ylevel[group]

    temp2 <- format(yprob, format = "%5.3f")
    if (nclass >1) {
      temp2 <- apply(matrix(temp2, ncol = nclass), 1L,
                     paste, collapse = " ")
    }
    #dev <- dev/(wt[1L] * nodeprob)
    paste0("  predicted class=", format(group, justify = "left"),
           "  expected loss=", format(dev, digits),
           "   probabilities: ", temp2)
  },
  text = function(yval, dev, wt, ylevel, digits, n, use.n) {
    nclass <- (ncol(yval) - 2L)/2L
    group <- yval[, 1L]
    counts <- yval[, 1L+ (1L:nclass)]
    if (!is.null(ylevel)) group <- ylevel[group]

    temp1 <- format(counts, digits)
    if (nclass > 1L)
      temp1 <- apply(matrix(temp1, ncol = nclass), 1L,
                     paste, collapse = "/")
    if (use.n)  paste0(format(group, justify = "left"), "\n", temp1)
    else format(group, justify = "left")
  })
}


gini_node <- function(y) {
  counts <- unname(tapply(y, as.factor(y), length))
  1 - sum((counts / length(y)) ^ 2)
}

gini_impurity <- function(y) {
  1 - sum((table(y) / length(y)) ** 2)
}


etempg <- function(y, wt, parms) {
  print('at eval1')
  newSampling()
  #wmean <- sum(y*wt)/sum(wt)
  #rss <- sum(wt*(y-wmean)^2)
  classes <- parms$classes
  priori <- parms$priori
  # refactor
  counts <- table(1:classes)
  for(i in 1:classes) {
    counts[i] <- 0
    counts[i] <- length(y[y == i])
  }
  probs <- counts / length(y)

  chosen_n <- as.numeric(names((sort(counts, decreasing = T)[1])))
  print(y)
  dev <- gini_impurity(y) * 100
  print(dev)
  lab <- c(chosen_n, unname(probs))
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label = lab, deviance = rss)
}

gini_process <-function(classes, splitvar){
  base_prob <-table(splitvar)/length(splitvar)
  if(length(base_prob) == 1) return(1)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,2]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}

ginisplit <- function(y, wt, x, parms, continuous) {
  debug <- parms$debug
  random <- parms$rand
  n <- length(y)
  nclasses <- parms$classes
  #isNotSampled <- !isSampledAttribute()
  random <- F
  isNotSampled <- F
#  print('at split1')
  # if is not sampled attribute
  if(isNotSampled)
    if(continuous)
      return(list(goodness=rep(0, n - 1), direction=rep(1, n - 1)))
    else {
      nu <- length(unique(x))
      return(list(goodness=rep(0, nu - 1), direction=rep(1, nu)))
    }

  max_impurity <- 1 - (1 / nclasses)
  if(continuous) {
    # calc max impurity


    goodness <- sapply(1:(n - 1), function(i) {
      # get left
      y_left <- y[1:i]
      # lmean <- mean(y_left)

      y_right <- y[(i + 1):n]
      # rmean <- mean(y_right)

      g_left <- gini_impurity(y_left)
      g_right <- gini_impurity(y_right)
      gnode <- 2 * max_impurity - ((g_left * i + g_right * (n - i)) / n)
    })
    #print(max_impurity)
   # print(goodness)
    if(random) {
      goodness <- rep(0.33, n - 1)
      goodness[(n - 1) %/% 2] <- 1.6
    }
   # print(goodness)
    list(goodness=goodness, direction=rep(1, n - 1))
  } else {
    xUnique <- unique(x)
    n <- length(xUnique)
    gini_val <- sapply(xUnique, function(val) {
      pass <- x == val
      # get left
      y_left <- y[pass]
      # lmean <- mean(y_left)

      y_right <- y[!pass]
      # rmean <- mean(y_right)

      g_left <- gini_impurity(y_left)
      g_right <- gini_impurity(y_right)
      impurity <- gnode <- 2 * max_impurity - ((g_left * i + g_right * (n - i)) / n)
      list(impurity=goodness, x=val)
    })
    sorted_by_val <- gini_val[order(sapply(gini_val, function(x) x$impurity))]
    list(goodness= sapply(sorted_by_val, function(x) x$impurity)[-n],
         direction = sapply(sorted_by_val, function(x) x$val))
  }
}

stempo <- function(y, wt, x, parms, continuous) {
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  #print(isSampledAttribute())
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)

    goodness <- rep(0.33, n - 1)
    goodness[(n - 1) %/% 2] <- 0.6
    list(goodness = goodness, direction = sign(lmean))
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
    # goodness <- rep(0.33, n - 1)
    # goodness[n %/% 2] <- 0.6
    # print(ux[ord])
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord])
  }
}


etemp <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)

  newSampling()

  list(label = wmean, deviance = rss)
}

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

  initSampling(parms)

  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

stemp <- function(y, wt, x, parms, continuous)
{

  n <- length(y)
  isNotSampled <- !isSampledAttribute()
  print(isNotSampled)
  # if is not sampled attribute
  if(isNotSampled)
    if(continuous)
      return(list(goodness=rep(0, n - 1), direction=rep(1, n - 1)))
  else {
    nu <- length(unique(x))
    return(list(goodness=rep(0, nu - 1), direction=rep(1, nu)))
  }
  # Center y
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    list(goodness = goodness, direction = sign(lmean))
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
         direction = ux[ord])
  }
}




