## A BRIEF INTRODUCTION TO R ##
#-------------------------------------
# We have seen a brief introduction to RStudio. Now let's use it.

## RUNNING CODE ##
#-------------------------------------
# You can run a single line of code by using Alt-Enter, or
# you can select multiple rows and run them in the same way.
# Commented code won't be executed (# at the begining of the line or '' over multiple lines)


## LIBRARIES ##
#-------------------------------------
# R comes with a base set of libraries preloaded. Let's see them.
search()
# Most other libraries/packages are available on the CRAN website, but can 
# be imported using RStudio.


## GETTING HELP ##
#----------------------------------

# R has very good built in documentation - try ? or help() function  
help(plot)
?mean

# R also has built in examples in the documentation
example(mean)

# R offers a great inbuilt tutorial called swirl
library("swirl")
swirl()
?swirl_options
help(swirl)

# There is also heaps of information on the internet


## INBUILT DATAFRAMES ##
#----------------------------------

# R comes with an inbuilt data frame that you can experiment with - mtcars
# Other datasets can be accessed using the datasets package
library(datasets)

# Calling the data() function will tell you more
data()

# More info can be found here https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html for more


## ASSIGNING AND DELETING VARIABLES ##
#-------------------------------------

# One of the most common things you will want to do is store values in variables
# this is done in two almost equivalent ways, but the first is the recommended way
x <- 5.2
y = "cat"

# You can see the value of the variable by using Alt-enter on the variable name
# or you can look in the environment tab.
x
y

## VARIABLE NAME RULES
#-------------------------------------
# Variables are case sensitive "Cat" is not the same as "cat". 
# When chosing varible names they can be a combination of letters, 
# digits, period (.) and underscore (_).
# They must start with a letter or a period. If they start with a period, 
# they cannot be followed by a digit.
# Reserved words in R cannot be used as identifiers. 
# e.g. for, while, Inf, FALSE, if, etc. For a full list and details
?reserved

# Variables can be deleted using the remove() function
remove(y)
y


## NA, NAN and NULL
#-------------------------------------

# Variables in R can be empty or null. These are treated differently to:
#    0   - the number 0
#    ""  - the empty string
# NA is a place holder for a missing value. It has a length of 1. 
# NAN stands for "Not A Number" - e.g. 0/0
# NULL represents the null object - it can be returned by functions to 
# indicate that the value is unassigned. It has a length of 0.


## R OBJECTS
#-------------------------------------

# Everything in R is an object
# All objects have a class, and other attributed depending on their class


# ATOMIC CLASSES
#-------------------------------------
# There only 5 'atomic' classes

# numerics (double floating point real numbers)
num <- 2
num <- 3.5
num <- 0.4e10

# characters strings
text <- "Hello world"

# logicals (aka booleans) - these are stored as FALSE = 0 and TRUE = 1
i_love_r <- T # or TRUE
i_love_brusselsprouts <- FALSE # or F

# complex/imaginary numbers in the form a + bi
cmplx <- 3+7i
cmplx

# integers
int <- 3L  # Note the L here just denotes an integer. 


## CASTING BETWEEN TYPES
#------------------------------------- 

class(int)
class(num)
x <- as.integer(num)
class(x)

class(num)
x <- as.character(num)
class(x)
x  # When you print the variable x it now has it has quotes

# Sometimes casting happens silently
# integer + numeric = numeric
x <- int + num
class(x)

# imaginary + numeric = imaginary
x <- cmplx + num
class(x)
x

# numeric + boolean = numeric! False = 0, True = 1
x <- num + i_love_r
class(x)
x

# sometimes it will give errors
x <- text + int



## NON ATOMIC CLASSES ##
#-------------------------------------

# There are plenty of objects in R that have non-atomic classes - i.e. they are
# made up of one or more objects. For example:


## VECTORS 
#-------------------------------------
# Vectors consist of zero or more objects of the same class
# A "numeric" vector
x1 <- c(26, 10, 4, 7, 41, 19)
x1

# A "character" vector of country names
x1 <- c("Peru", "Italy", "Cuba", "Ghana")
x1


## LISTS
#-------------------------------------
# these are vectors that have elements of different classes
x<- list(5, "rabbits", FALSE)
x

## MATRICIES
#-------------------------------------
# all the elements in a matrix need to be of the same class
M = matrix(   # Note here that = can be used in a similar fashion to <- 
  c(2, 4, 3, 1, 5, 7), # the data elements  
  nrow=2,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 
M

## DATAFRAMES
#-------------------------------------
# Like matricies, only the elements can be of different classes.
# This is useful for data that consists of variables of different types

## Rasters
## Shapefiles
## Many others 

## BASIC OPERATIONS IN R
#-------------------------------------
# We can use basic operations on variables in R


# COMPARISON OPERATORS
#-------------------------------------

#  equal: ==
x<-2
y<-3
x==y

#  not equal: !=
x!=y

#  greater/less than: > <
x < y

#  greater/less than or equal: >= <=
x<=y


## INFINITY
#-------------------------------------
# R has a built in concept for infinity
x<-1/Inf


## LOGICAL OPERATORS
#-------------------------------------
# AND & Returns TRUE if both comparisons return TRUE.
x <- 1:10    # This creates a numeric vector of the numbers 1-10. Well come to this later.
x
y <- 10:1
y
x > y & x > 5    # This can be read as  (x > y) AND (x > 5)

# OR | Returns TRUE where at least one comparison returns TRUE.
x == y | x != y  # This can be read as (x equal to y) OR (x not equal to y)

# NOT ! The ’!’ sign returns the negation (opposite) of a logical vector.
!x > y  # This can be read as NOT (x > y) - which from maths we know is the same as x <= y

# These return a logical vector of TRUE or FALSE and can be useful for filtering


## CONTROL STRUCTURES IN R
#-------------------------------------
# Like all programming languages R offers various ways to control the 
# flow of execution

## IF / ELSE STATEMENTS
#-------------------------------------
x <- 5
y <- 10
if (x > y) {
  print("x is bigger than")
} else {
  print("x is less than or equal to y")
}

## FOR LOOP
#-------------------------------------
# Iterates through code for a fixed number of times
x<-NA
for (i in 1:27) {
  x<-paste(x," NA")
  if (i == 27) {
    x<-paste(x," BATMAN!")
  }
} 
print(x)

## WHILE LOOP
#-------------------------------------
# Iterates through code while a condition is met. 
# Useful when we don't know how many loops to do - e.g. convergence
current_time<-Sys.time()
current_time
end_time <- current_time+ 5 # 5 seconds
end_time
while (current_time < end_time) {
  print(paste("The time is: ",current_time))
  Sys.sleep(1) # wait for one second
  current_time<-Sys.time()
}

## REPEAT LOOP
#-------------------------------------
# Executes an infinite loop

## NEXT STATEMENT
#-------------------------------------
# Skip to the next iteration of the loop
# print the numbers 1 divided by the integers -5 to 5
for (i in -5:5) {
  if (i==0){  # can't divide by 0
    next()
  }
  print(1/i)
}

## BREAK STATEMENT
#-------------------------------------
# Break the exectution of a loop
i <- 1
convergence_point=0
x<-10
while (x!=convergence_point){
  x <- x + sample(-1:10,1) # add a random number between -1 and 10
  print(paste(i,": ",x,sep = ""));
  i <- i+1;
  if (i>100){
    print("no convergence after 100 iterations")
    break()
  }
  }


## VECTORISED OPERATIONS
#-------------------------------------
# R offers simple ways to add/subtract/muliply/divide 
# vectors by other vecors or scalars
x<-c(1:4)
y<-c(5:8)
x
y

x + y
x * y

a<-10
a * x

# this works well when you are adding or multiplying vectors where the
# number elements in one is a multiple of the number of element in the other
z <- c(10, 100)
x + z
x * z

# What happens here though
z <- c(10, 100, 1000)
x + z  # It works, but note the warning message

x <- seq(-50, 50, by=10)
x
x / 10

x <- -10:10
y <- x %% 2 # here %% = modulus
y


## SUBSETTING IN R
#-------------------------------------
# Non-atomic objects in R can be subset in three basic ways - 
# Single bracket operators []
# this always returns an object the same class as the original object being subset
# double bracket operators [[]]
# this returns a single element from a list or dataframe
# $ operator
# this returns an object of a list or dataframe by name
# this is similar to [[]]
my_list<-list(-1:-5,
              6L,
              5e-4,
              c("cat","dog","mouse"),
              5+3i,
              "hello world",
              TRUE)
my_list
class(my_list)
x<-my_list[2]
x
class(x)  # we started with a list so this should return a list with one element
y<-my_list[[2]]
y
class(y)

# you can also give names to the elements of your list 
# and you can use the $ operator to refer to them
x<- list(num=5, pet="rabbit", eats_meat=FALSE)
my_pet<-x$pet
class(my_pet)

# Note this is not allowed
a<-"pet"
x$a


# SUBSETTING USING LOGICAL VECTORS
#-------------------------------------

x<-c(-10:10)
even_x<- (x %% 2)==0 # here %% = modulus 
even_x # this is a logical vector representing if the values of x are divisible by 2
z<-x[even_x]
z
odd_x<-x[!even_x]
odd_x

# two ways to get the positive numbers 
pos_x <- x[x>0]
pos_x
pos_no <- x>0 # this gives a logical vector
pos_no
pos_x <- x[pos_no]
pos_x


## VIEWING DATA
#-------------------------------------

# R offers a number of ways to summarise and view data
# the str() function gives the structure of an object
str(mtcars)

# the class() function we have seen
class(mtcars)

# the head() and tail() functions allow us to look at the top and bottom rows
head(mtcars)
tail(mtcars, 10)

# the table() function gives the frequency of any given variable
table(mtcars$cyl)

# the dim() function gives the dimentions of the variable
# for matricies and dataframes the dimensions are given as number of rows
# by number of columns
dim(mtcars)

# the summary() function gives 
summary(mtcars)

# the attributes will give more information on the object depending on what it is
attributes(mtcars)



# PLOTTING DATA
#-------------------------------------
# One of the great things about R is how easy it is to plot data

# Read in the dataframe into a variable
cw<-ChickWeight

# Let's take a quick look at the data
head(ChickWeight)
str(ChickWeight)
summary(ChickWeight)

# Now let's create some plots
plot(cw$Diet,cw$weight)

plot(cw$Diet,cw$weight
     , xlab="diet" # x label
     , ylab="weight in grams" # y label
     , main="Chick weight by diet") # title

# just diet 1 over time
diet_1<-cw[cw$Diet==1,]
plot(diet_1$Time,diet_1$weight
     , type="p" # the type of graph see help(plot)
     , xlab="time in days" # x label
     , ylab="weight in grams" # y label
     , main= "Chick weight over time for diet 1"
)

# with some more sophisticated libraries we can get prettier plots
library(ggplot2)
ggplot(cw, aes(Time, weight, colour=Diet)) +
  geom_point() +
  facet_wrap(~ Diet) +
  theme(legend.position = "bottom")

ggplot(cw, aes(Time, weight,
               group=Diet, colour=Diet)) +
  ggtitle("Average chick weight by diet over time")+
  stat_summary(fun.y="mean", geom="line")


## PLOTTING SHAPEFILES
#-------------------------------------
# R can also be used to plot complex spatial data such as polygons
require(rgdal)
project_dir<-getwd()
setwd("POA2016_shapefile")
dat<-readOGR(dsn = ".", layer = "POA_2016_AUST")
str(dat)
head(dat@data)
plot(dat)



## OTHER THINGS YOU CAN DO ##
#-------------------------------------
# This is really just the tip of the iceburg. R can do so much more. e.g.

#  write your own functions
#  execute code in other files
#  connect to databases
#  scrape web pages
#  create web pages or other documents (static or dynamic)
#  create a large variety of data visualisations
#  almost anything you want!

# Please have a go! It is fun.