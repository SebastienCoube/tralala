
#groups = list(c("S", "R"), "D", "I")
#x = rnorm(4)
#names(x) = c("S", "I", "R", "D")
#mar_var = c(1,1,1)
#mean = rep(0, 3)
#DMVNGroup(x, mean, mar_var, groups)

DMVNGroup = function(
    x,
    mean,
    mar_var,
    groups
){
  if(length(mean)!=length(groups))stop("groups and mean must have the same length")
  if(length(mar_var)!=length(groups))stop("groups and mar_var must have the same length")
  if(!is.list(groups))stop("groups must be a list")
  if(any(is.na(match(do.call(c, groups), names(x)))))stop("The groups and the names in x do not match")
  if(any(is.na(match(names(x), do.call(c, groups)))))stop("The groups and the names in x do not match")
  if(any(mar_var<0))stop("mar_var must be positive")
  meanvec = x
  Sig = matrix(0, length(x), length(x)); row.names(Sig) = names(x); colnames(Sig) = names(x)
  for(i in seq(length(groups))){
    meanvec[groups[[i]]] = mean[i]
    Sig[groups[[i]], groups[[i]]] = mar_var[i]*.99
    Sig[cbind(groups[[i]], groups[[i]])] = mar_var[i]
  }
  return(-.5 * sum(((x - meanvec) %*% solve(Sig)) * (x - meanvec)))
}

#'plot(seq(-20, 20), softPlus(seq(-20, 20)))
#'abline(a=0, b=1)
#'abline(h=0)
softPlus = function(x) log(1 + exp(x))

#'plot(seq(-20, 20), softPlusSquare(seq(-20, 20)))
#'abline(a=0, b=1)
#'abline(h=0)
softPlusSquare = function(x) log(1 + exp(x))^2


