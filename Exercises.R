# Exercises

## Create list of all replacement functions in base package
ReplFuncsBase <- function() {
  baseFuncs <- ls("package:base")
  replaceFuncs <- grep("<-", baseFuncs, value = TRUE)
  replaceFuncs
}
## Get all replacement functions from base package
replaceF <- ReplFuncsBase()

## Get list of all replacement functions in base package which are also primitive
replacePrimitiveF <- names(Filter(is.primitive, mget(replaceF, inherits = TRUE)))

setdiff(replaceF, replacePrimitiveF)

## Infix "XOR" function

`%XOR%` <- function(a, b) {
  !(a && b) && (a || b)
}

TRUE %XOR% FALSE

## Infix function of intersect(), union(), and setdiff()

`%intersect%` <- function(a, b) intersect(a,b)
`%union%` <- function(a, b) union(a,b)
`%setdiff%` <- function(a, b) setdiff(a,b)

seta <- c(1,2,3,4)
setb <- c(3,4,5,6)

seta %intersect% setb
identical(seta %intersect% setb, setb %intersect% seta)
seta %union% setb
setb %union% seta
seta %setdiff% setb
setb %setdiff% seta


## replacement function that modifies a random location in a vector

`randomRepl<-` <- function(x, value) {
  x[sample(length(x), 1)] <- value
  x
  }

testVec <- 1:100
randomRepl(testVec) <- 200
testVec

## Always close graphics device
f <- function() {
  pdf(tempfile()) # tempfile() so example doesn't clutter up working directory.
  print(dev.list())
  on.exit(dev.off())
  plot(randomNonExistentObjectName)
}

f()
dev.list()

## undo library() function
library(dbplyr)
search()
detach("package:dbplyr", unload=TRUE)
search()

## Create search path with for-loop
checkSearchPath <- function(env = globalenv()) {
  envs <- c(env)
  while (!identical(emptyenv(), env)) {
    env <- parent.env(env)
    envs <- c(envs, env)
  }
  #identical(envs[[12]], emptyenv())
  all(c(baseenv(), emptyenv()) %in% envs)
}

## Own version of search

search2 <- function() {
  env <- globalenv()
  envs <- c(environmentName(env))
  while (!identical(emptyenv(), env)) {
    env <- parent.env(env)
    envs <- c(envs, environmentName(env))
  }
  envs
}

  
## Recursive implementation of where
where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
    
  } else if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    env
    
  } else {
    # Recursive case
    where(name, parent.env(env))
    
  }
}


## Modified where function to find all environments with binding name

where2 <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    c()
  } else if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    c(environmentName(env), where2(name, parent.env(env)))
  } else {
    c(where2(name, parent.env(env)))
  }
}

## Own version of get

get2 <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    env[[name]]
  } else {
    # Recursive case
    get2(name, parent.env(env))
  }
}

## Write a function called fget() that finds only function objects. 
## It should have two arguments, name and env, and should obey the 
## regular scoping rules for functions: if there’s an object with a 
## matching name that’s not a function, look in the parent. For an 
## added challenge, also add an inherits argument which controls 
## whether the function recurses up the parents or only looks in 
## one environment.

fget <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    stop("Can't find function", name, call. = FALSE)
  } else if (exists(name, envir = env, inherits = FALSE) && is.function(env[[name]])) {
    # Success case
    env[[name]]
  } else {
    # Recursive case
    fget(name, parent.env(env))
  }
}


## Write your own version of exists(inherits = FALSE) 
## (Hint: use ls().) Write a recursive version that behaves 
## like exists(inherits = TRUE)

exists2 <- function(name, env = parent.frame()) {
  workspace <- ls(env)
  c(name) %in% workspace
}

exists3 <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    FALSE
  } else if (exists2(name, env)) {
    # Success case
    TRUE
  } else {
    # Recursive case
    exists3(name, parent.env(env))
  }
}

## Create full search tree
startenvs <- NULL
for (p in (.packages())) {
  startenvs <- c(startenvs, asNamespace(p))
}

getPath <- function(env = parent.frame()) {
  path <- c()
  while (!identical(emptyenv(), env)) {
    path <- c(path, environmentName(env))
    env <- parent.env(env)
  }
  path
}

getPathOverview <- function(startenvs) {
  environments <- list()
  for (env in startenvs) {
    searchpath <- getPath(env)
    envName <- environmentName(env)
    print(envName)
    environments[[envName]] <- searchpath
  }
  environments
}

pathlist <- getPathOverview(startenvs)


## Write an enhanced version of str() that provides more 
## information about functions. Show where the function was 
## found and what environment it was defined in.

strEnhanced <- function(name) {
  print(name)
  print(environment(name))
  nameStr <- deparse(substitute(name))
  where2(nameStr)
}


## Change value in global environment

x <- 1
f3 <- function() {
  f4 <- function() {
    x <<- x + 1 ## Walks up all environments until it finds it
  }
  f4()
}

## Create a version of assign() that will only bind new names, never re-bind old names.

myAssign <- function(name, value) {
  if (!exists(name, inherits=TRUE)) assign(name, value, envir = parent.frame())
}

## Create function that summarizes multiple statistics for arbitrary columns
Summary <- function(x) {
  nms <- c("mean", "median", "sd","mad", "IQR")
  funs <- sapply(nms, get)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}

sapply(df, Summary)


## Use lapply() and an anonymous function to find the coefficient of 
## variation (the standard deviation divided by the mean) for all columns 
## in the mtcars dataset.

str(lapply(mtcars, function(x) sd(x)/mean(x)))


## Get names of all functions with str.
lapply(methods(str), function(x) strsplit(x, "str\\.")[[1]][2])

## Create integrate() function with anonymos function call
integrate(function(x) x ^ 2 - x, 0, 10)
integrate(function(x) sin(x) + cos(x), -pi, pi)
integrate(function(x) exp(x) / x, 10, 20)

## Create a function pick() that takes an index, i, as an argument 
## and returns a function with an argument x that subsets x with i

pick <- function(i) {
  function(x) x[[i]]
}

identical(lapply(mtcars, pick(5)), lapply(mtcars, function(x) x[[5]]))

## Create a function that creates functions that compute the ith 
## central moment of a numeric vector.

moment <- function(i) {
  function(v) 1/length(v) * sum((v-mean(v))^i)
}

m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

## Compare performance of different mean calculation (list of functions)
compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x) / length(x),
  manual = function(x) {
    total <- 0
    n <- length(x)
    for (i in seq_along(x)) {
      total <- total + x[i] / n
    }
    total
  }
)

x <- runif(1e5)
call_fun <- function(f, ...) f(...)
lapply(compute_mean, call_fun, x) ## Same result?
lapply(compute_mean, function(f) system.time(f(x))) # Time it

## HTML function factory with tags
simple_tag <- function(tag) {
  force(tag)
  function(...) {
    paste0("<", tag, ">", paste0(...), "</", tag, ">")
  }
}
tags <- c("p", "b", "i")
html <- lapply(setNames(tags, tags), simple_tag)
html$p("This is ", html$b("bold"), " text.")

## Implement a summary function that works like base::summary(), 
## but uses a list of functions. Modify the function so it returns 
## a closure, making it possible to use it as a function factory.



## Create a named list of all base functions. Use ls(), get() and 
## is.function(). Use that list of functions to answer the following 
## questions: Which base function has the most arguments?
## How many base functions have no arguments?

baseFuncs <- Filter(is.function, sapply(ls(baseenv(), all.names = TRUE), get, envir=baseenv()))
## Which base function has the most arguments? => scan
which.max(sapply(baseFuncs, function(x) length(formals(x))))
## How many base functions have no arguments? => 250
length(Filter(function(y) y==0, lapply(baseFuncs, function(x) length(formals(x)))))


## Instead of creating individual functions (e.g., midpoint(), trapezoid(), 
## simpson(), etc.), we could store them in a list. 

### If we did that, how would that change the code? 

integrationApprox <- list(
  midpoint = function(f, a, b) {
    (b - a) * f((a + b) / 2)
  },
  trapezoid = function(f, a, b) {
    (b - a) / 2 * (f(a) + f(b))
  },
  simpson = function(f, a, b) {
    (b - a) / 6 * (f(a) + 4 * f((a + b) / 2) + f(b))
  }
)

composite <- function(f, a, b, n = 10, rule) {
  points <- seq(a, b, length = n + 1)
  
  area <- 0
  for (i in seq_len(n)) {
    area <- area + rule(f, points[i], points[i + 1])
  }
  
  area
}

lapply(integrationApprox, function(x) composite(f=sin, a=0, b=pi, n = 10, rule=x))

### Can you create the list of functions from a
### list of coefficients for the Newton-Cotes formulae?





## The trade-off between integration rules is that more complex rules are
## slower to compute, but need fewer pieces. For sin() in the range [0, π], 
## determine the number of pieces needed so that each rule will be equally
## accurate. Illustrate your results with a graph. How do they change for 
## different functions? sin(1 / x^2) is particularly challenging.

precisionTab <- sapply(1:50, function(ns) lapply(integrationApprox, function(x) composite(f=sin, a=0, b=pi, n = ns, rule=x)))
results <- t(data.frame(precisionTab))
rownames(results) <- 1:50
colnames(results) <- names(integrationApprox)
plot(rownames(results), 
     type="l", 
     results[,"midpoint"], 
     col="red", 
     xlab="Number of pieces", 
     ylab="Value")
lines(rownames(results), 
      results[,"trapezoid"], 
      col="green")
lines(rownames(results), 
      results[,"simpson"], 
      col="blue")








