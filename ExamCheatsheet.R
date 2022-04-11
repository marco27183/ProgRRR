# Exam cheatsheet



########################################################################################################################


## Helpful functions for lists

## create list of letters (e.g. 5 first letters of alphabet):
n = 10
LETTERS[1:n]

## change names of list/atomic vector:
v <- 1:5
names(v) <- c("Hans", "Heiri")
names(v) # creates NA_character for missing names (typeof(names(v)[3]) returns "character")
v
is.na(names(v)) # check for each element if NA and returns boolean vector

## Whats the outer-most function that is called here?
# 1.
names(v) <- LETTERS[1:5]
### It is the asignement function `names<-`
`names<-` #identical to get("names<-"): identical(`names<-`, get("names<-"))

# 1.
names(v)
### It is the names function `names`
`names` #identical to get("names"): identical(`names`, get("names"))

# 1.
v
### It is the print function `print`
`print` #identical to get("print"): identical(`print`, get("print"))




## Get or set body of functions
body(lm)

## Get or set formals
formals(lm)
### Get names of formals
names(formals(lm)) ## Empty if no default value
str(formals(lm)) # Is not a regular list, but "Dotted pair list"

## Every non-primitive function has three parts: formals, body and environment



########################################################################################################################



# Environments
## Environments can contain themselves, elements in environment are not ordered (cannot say first object of environment)
## ls(env) sorts all elements in environment, while names(env) does not do that
ls.str(env) # does not return something but prints the structure of environment
## To list everything, including the functions/variables that start with ".", use all.names=TRUE

## Get loaded namespaces:
loadedNamespaces()
## Load namespace
loadNamespace()

## Get function in all environments:
getAnywhere(mean)

## Create environment
e.1 <- new.env()
e.1$a <- 1:3
e.1$b <- 1000

e.2 <- e.1

## expression/quote: keep syntax as text and only evaluate once eval is called
eval(expression(a+b), envir = e.1)
eval(quote(a+b), envir = e.2)
s <- function() a+b
s() # error
eval(quote(s()), envir=e.1) # error since environment(s) is not e.1 but .GlobalEnv

### Change environment of function
environment(s) <- e.1
s() # works

## get specific namespace
asNamespace("stats")

## Get import:<package> environment
parent.env(asNamespace("stats"))

## Get all currently loaded packages
(.packages())

## Get list of all packages
library()$results[,1]

## How to find all packages in current session?
search() # List is in same order as searchpaths() (meaning first is .GlobalEnv and last is package:base)
searchpaths()

## How to list all available objects in current environment?
ls() # objects and ls are identical
ls(2) # lists all objects in the second environment in search path
### it lists all functions, variables and datasets

## if we want to get overview of all R objects in search path, we cal ls() on each search() entry:
ls.srch <- sapply(grep("package:", search(), value=TRUE), ls, all.names=TRUE) # only in packages, all.names makes sure we also get hidden objects
### Quick note: sapply(x, f, simplify = FALSE, USE.NAMES = FALSE) is the same as lapply(x, f).

## if we want to get overview of all R functions in search path, we cal ls() on each search() entry:
fn.srch <- sapply(ls.srch, function(nm) {nm[sapply(lapply(nm, get), is.function)]})

### get overview table
res <- rbind(cbind(ls = (N1 <- sapply(ls.srch, length)), funs = (N2 <- sapply(fn.srch, length))), TOTAL = c(sum(N1), sum(N2)))

## Find a function in all packages/locations
`[<-` <- function(){}
find("[<-") # in two places
get("[<-", 1) # close to below 
get("[<-", "package:base")
rm("[<-") # to remove self-created function from gloabl-env (could also use rm("[<-", pos = ".GlobalEnv"))

## Find all definitions of function:
methods("+")

## Information about session
sessionInfo() # see all information about current session

## Check if function is S3 generic
isS3stdGeneric(coef) # isS3stdGeneric(get("coef"))

### Check if function is S3 method
isS3method("coef") # isS3method(deparse(substitute(coef)))

## Get all objects and functions in environments including their content/values:
as.list(environment())

# Sink: Send all console output to file
sink("testFile.R") # start sinking to file
getAnywhere("quantile.default")
sink() # finish sinking to file

### Same functionality: dump("quantile.default", file="testFile.R", envir = asNamespace())

# See envex2.R!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


## Assign name to environment
attr(e.1, "name") <- "Cool name"
environmentName(e.1)

## get object from environment
get("a", envir = e.1)
get("d", envir=e.1) # works because default is inherits=TRUE (d is in global env)
get("d", envir = e.1, inherits=FALSE) # cannot find object in e.1

## assign object to environment
assign("myTestLogic", c(TRUE, FALSE), envir = e.1)
ls(e.1)

### package:<packageName> only shows functions that are exported, the namespace:<packageName> contains more!
### The environment of all functions in package:<packageName> is the namespace:<packageName>
### Parent environment of namespace is import:<packageName>


# Check the paths where packages were installed
.libPaths()
.Library
library(lib.loc=.Library)



########################################################################################################################


## Profiling and documentation of package/vignette

library(proftools)
# get description of package
packageDescription("proftools")
## Exploring package _manually_: The ./doc/ sub-directory has vignettes
(doc.dir <- system.file(package = "proftools", "doc"))
list.files(doc.dir)

## Profiling a function/script
srcfile <- system.file("samples", "bootlmEx.R", package = "proftools")
srcfile
profout <- tempfile()
Rprof(file = profout, gc.profiling = TRUE, line.profiling = TRUE)
source(srcfile)
Rprof(NULL)
summaryRprof(profout, lines = "show")
pd <- readProfileData(profout)
unlink(profout)

pd <- profileExpr(source(srcfile))

## ------------------------------------------------------------------------
head(funSummary(pd), 10)

## ------------------------------------------------------------------------
head(funSummary(pd, srclines = FALSE), 10)

## ------------------------------------------------------------------------
head(callSummary(pd), 10)

## ------------------------------------------------------------------------
srcSummary(pd)

## ------------------------------------------------------------------------
hotPaths(pd, total.pct = 10.0)

## ------------------------------------------------------------------------
filteredPD <- filterProfileData(pd, select = "withVisible", skip = 4)
f.......PD <- filterProfileData(pd,                         skip = 4)
all.equal(filteredPD, f.......PD) # |-> TRUE  #--> select =".." here unneeded
## ------------------------------------------------------------------------
hotPaths(filteredPD, total.pct = 10.0)

## ------------------------------------------------------------------------
glmPD <- filterProfileData(filteredPD, focus = "glm")
hotPaths(glmPD, total.pct = 5.0)

## ----fullCallGraph, fig.cap = "Full call graph of profile data."---------
plotProfileCallGraph(pd)

## pdf version:
pdf("plotProfCallGraph.pdf")
plotProfileCallGraph(pd)
dev.off()
system("evince plotProfCallGraph.pdf &") # <-- on Linux; use "open" on Mac

## ----filteredCallGraph, fig.cap = "Call graph for \\code{glm.fit} call."----
plotProfileCallGraph(filterProfileData(pd, focus = "glm.fit"))

## ----printProfileCallGraph, eval=FALSE-----------------------------------
#  printProfileCallGraph(filterProfileData(pd, focus = "glm.fit"))

## ----echo = FALSE, comment = NA------------------------------------------
printProfileCallGraph(filterProfileData(pd, focus = "glm.fit"))

## ----flameGraph, out.width = "4in", fig.cap = "Flame graph visualizing hot paths for the full profile data."----
flameGraph(pd)

## ----filteredFlameGraph, out.width = "4in", fig.cap = "Flame graph of the filtered profile data."----
flameGraph(filteredPD)

## ----timeGraph, out.width = "4in", fig.cap = "Time graph of the full profile data."----
flameGraph(pd, order = "time")

## ----eval = FALSE--------------------------------------------------------
#  fg <- flameGraph(pd)
#  identify(fg)

## ----calleeTreeMap, out.width = "4in", fig.cap = "Call tree map of the full profile data."----
calleeTreeMap(pd)

### Open vignette:
"./proftools-vignette.R"

### Show pdf of doc:
shell(file.path(doc.dir, "proftools.pdf"))

# Optimize code
library(microbenchmark)
mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)
x <- runif(100)
stopifnot(all.equal(mean1(x), mean2(x)))
timings.mean <- microbenchmark(
  mean1(x),
  mean2(x)
)

## Get table of timings
summary(timings.mean)

## How to get the median:
summary(timings.mean)$median




########################################################################################################################


# Helpful commands:


## http://adv-r.had.co.nz/Vocabulary.html#undefined
## Script: https://stat.ethz.ch/CRAN/doc/contrib/Lam-IntroductionToR_LHL.pdf

## Is element in array?
any(1:20 == 10) # or 10 %in% 1:20 which is much slower

## Check if argument was provided in function call
missing(x)

## Check if variable exists in environment including full search path
exists("a")

## Find out where function/variable exists (which environment)
library(pryr)
where("a")

## Cut numeric array into x pieces
pcs <- 3
cut(1:40, pcs) # labels=FALSE would recode them as integers

## Get class of object
data.class(timings.mean)

## find methods of class
methods(class="lm")

## Find which classes have implementation of method
methods(generic.function="extractAIC")

## Assign class to object
testArray <- 1:20^4
class(testArray) <- c("LargeArray")

## Find variables that are not defined in function but must be 
## found outside (lists all the external dependencies of a function)
f <- function() x + 1
codetools::findGlobals(f)

## Send list of arguments to function:
args <- list(1:10, na.rm = TRUE)
do.call(mean, args)
identical(do.call(mean, args), mean(1:10, na.rm = TRUE))

## Define default arguments as variable from code:
h <- function(a = 1, b = d) {
  d <- (a + 1) ^ 2
  c(a, b)
}

## Substitute, quote, deparse
x <- runif(1e4)
microbenchmark(mean(x), mean.default(x))

## Apply vs. rowSums:
microbenchmark(apply(as.matrix(x), 1, sum), rowSums(as.matrix(x)))


# Which function is called for glm?
(ml <- methods(class = "lm"))
(mg <- methods(class = "glm"))
str(ml)

(ig <- attr(mg,"info"))
(il <- attr(ml,"info"))

str(n.l <- il[,"generic"])
str(n.g <- ig[,"generic"])

setdiff(n.l, n.g) # for these, <method>.lm is called, even if glm is used


## Functions testing my knowledge
y <- 10
z <- 20
f1 <- function() {
  q <- 100
  assign("y", y+1, envir=.GlobalEnv)
  z <<- z + 10 # equivalent to above
  f2 <- function() assign(y, y+1, pos = .GlobalEnv)
  f3 <- function() print(q <- q+10)
  environment(f3) <- environment()
  f4 <- function() print(z <- z + 10)
  environment(f4) <- parent.env(environment())
  f3()
  print(q)
  }
f1()
f1()
(y)
(z)


l <- function(x) x + 1
m <- function() {
  l <- function(x) x * 2
  print(l(10))
  get("l", pos=globalenv())(10) # get "outer" function l
}
m()


f1 <- function(x = {y <- 1;2}, y=0) x+y

## Dont show anything after function returns
### Function can be applied to list or environment
modify <- function(x) {
  x$a <- 2
  invisible()
}

## as.list is generic has many methods:
as.list ##-> UseMethod ===> it is a S3 generic function

##
match.call() # the function *call* (an object of class & type  "call")



