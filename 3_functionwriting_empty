########LESSON 3 ###############
### -- Commonly used tools in R for more complicated tasks
### -- By - Evan Adams, Matt Boone, and Auriel Fournier
### -- https://github.com/aurielfournier/AOS2019AK


### Commonly used tools in R for more complicated tasks

# use install.packages() if you don't have these already
library(dplyr)
library(tidyr)
library(ggplot2) 

### Paste Functions

# Whatever you put in quotes will be printed to the console. This is useful because it will print out while code is running, including inside functions and for loops.

?paste
?paste0
x <- 'evan'
paste0('hello','my','name','is',x)
paste('hello','my','name','is',x)
#How these work is they 'paste' together whatever things you give it, exactly how you tell it to

paste0('hello')
paste('hello','world')
paste0('hello','world')
paste0('hello',' world')
paste('hello','world',sep='-')
paste('hello','world','my name is',sep=' ')

#This becomes useful when you add in dynamic variables
i <- 'the best'

paste0('i am ',i)

#Paste is most useful with two things. Reading/writing in files and in concert with the print function.
#For instance say you have a set of data but is seperated by state. You could input:

state <- 'AZ'

paste0('myfiles_',state,'.csv')

state <- c('AZ','TX')

paste0('myfiles_',state,'.csv')

#So you can use this in for loops to save/load multiple files at once, or decide what to save/load.

extension <- 'final1'

#saving
write.csv(data,paste0('myfiles_',extension,'.csv'))

#loading
read.csv(paste0(,,))


### For loops -- or how to write code in a way that you can do repeated actions

#the basics: what is a for loop? Using brackets you define a repeated set of actions for a variable
#here is a toy example where we are interested in printing out a series of numbers

for(i in 1:10){
  print(i)
}

#okay, that happened pretty fast, what did it actually do?
#well let's look at 1:10 see what that looks like
1:10

#so it created a vector of integers from 1 to 10. Since we told the loop that i was supposed to take on those values and that we should print them, the loop went through each value in sequence and executed the command
#you can break down what it is doing into component parts fairly easily
i<-1
print(i)
i<-2
print(i)
i<-3
print(i)

#so this allows us to go through actions repeatedly for a large amount of iterations. Can you think of any examples where this might be useful?

#Let's work an example of how for loops are useful, we'll use the eBird data from before to build an interesting situation
#let's say you wanted to make and save a series of ggplot figures of a core group of species for a project or manuscript, how would you go about doing that? 

#let's remind ourselves of what the ebird data look like from previous lessons:

head(abird)
str(abird)

#what species do we have?
spp <- levels(abird$species)
spp

#how do we decide which ones we want to graph? Let's pick the ones that were found in all a-states in all years
#what species do we want to select?
table(abird$species)

#okay, that's a lot of species, too many for us to deal with here. Let's use some data management tricks to select the species that are most common in these areas
#here we are summing the total numbers of states and years that we see each species in the data set
#I use rowSums to add all the combinations up
?rowSums
spp.sum <- rowSums(matrix(table(), ncol = , byrow = FALSE))
View(spp.sum)

#select species with 44 state/years of data
spp.select <- spp[ ]

#this is still 60 species, so let's just look at the first 10 so we can try something without it taking too long
#here are the species that we want to select
#with piping
spp.select <- spp.select %>%
  filter()

#or without piping using a which() command like you used filter() earlier
spp.select <- spp.select[which() ]

#and here is how we are going to select species
#with piping
abird.species <- abird %>%
  filter( )

#or without
abird.species <- abird[ ,]

#let's plot changes in presence over year by state for each species we selected using a for loop
#we can loop around a data select and ggplot function so we can do those things repeatedly

for(i in spp.select){
  #select the species and make a temporary data frame for the loop
  abird.tmp <- abird.species[which(),]
  
  #ggplot code (in slightly different syntax than before) that plots points and lines for each state
  p <- ggplot(abird.tmp, aes( ))
  p <- p + geom_point() + geom_line() + ggtitle(i)
  plot(p)
  
  #using paste from earlier, we will save the  file
  ggsave(file = paste0( , , ), device = 'png', dpi = 300)
}

#so using this tool, we just printed out and saved 70 figures (well, 10 because we didn't feel like waiting for all 70...) all at once
#let's dissect what happened here:
#we made a list of species that we wanted to make a figure for
#then we selected data from our ebird data frame for each of the species that we wanted
#we built a ggplot figure based upon the species data
#then we used the name of the species to build a file name so that we can save unique files for each

#for loops are a pretty powerful strategy in R and figuring out how to get your code to run in for loops can save you a ton of time and energy and is often well worth it

#let's add a bit more complexity, what if we want the loop to take an action only some of the time?
#using 'if' statements we can create conditional statementss that allow us to shape how the loop behaves
#let's only graph species with above average probability of presence

#let's look to see what the mean presence is:
summary(abird)

#now let's implement an if statement in the previous loop
for(i in spp.select){

  #select the species and make a temporary data frame for the loop
  abird.tmp <- abird.species[which( ),]
  
  #the conditional if statement that does something different for 'above average species'
  if(mean( ) ) > mean( ))
    
    #when the statement is true then we want this to happen:
  {
    #the same ggplot plot code as before
    p <- ggplot( )
    p <- p + # add your geom_ functions here...
    #plot it, notice that this plots to your RStudio 'Plots' tab
    plot(p)
    #now save it using the species name information that we collected
    ggsave( , , ), device = 'png', dpi = 300)
  } 
  
  #then the statement for what to do when the if statement is false:
  
  else{
    print('NOPE!')
  }
}

#well that cut down on the number of graphs by a fair amount
#note the if/else structure in the loop, if the if statement is TRUE, the first statement in the brackets if run. If it's FALSE then the else statement is run. The else statement isn't required, you can just use the if portion.
#the if statement must result in a boolean response (TRUE/FALSE)

#Trouble-shooting for loops. For loops can be difficult to troubleshoot when they hit a snag so I wanted to give you a couple of tips on troubleshooting them.

#1. Put in a line at the top that allows you to read in a dummy variable

for(i in 1:10){
  #i<-1
  print(i)
  
}

#2. Print the outputs, or save middle outputs
for(i in c(1,2,NA,4)){
  print(i)
  sub.data<-i+1
  print("heres the middle data set")
  print(sub.data)
  print('now printing mean')
  mean(sub.data)
}

#3. Write the for loop last
for(i in 1:10){
  #i<-1
  mean(i)+2
  print(paste0('the answer is: ', i))
}

## CHALLENGE
#create a loop that makes a presence over time graph for each species. For species with above median presence for our 10 species group let's use a smooth line, for below median species let's just use points with no line


#but, for loops can be kinda slow and clunky sometimes
#there are other ways of packaging r scripts and turning them into something that runs efficiently
#enter, functions

#Function writing, why learn it?
# There's a couple of reasons learning functions is helpful. One you can write your own common functions. two you can better understand how to troubleshoot your code and other peoples' functions. Three the principles of function writing teach us more about good coding tips to learn including reproduciability.  
# What is a function and why is it necessary in R
# A function lets you package code into one unit that doesn't need to be loaded multiple times, or rewritten.

sem <- function(numbers, na.rm = TRUE){
  se <- sd(numbers, na.rm = na.rm)/sqrt(length(numbers))
  return(se)
}

sem(abird$samplesize)

c(hi = mean(abird$samplesize) + sem(abird$samplesize),
  mean = mean(abird$samplesize),
  lo = mean(abird$samplesize) - sem(abird$samplesize))

#And we can create a list of all these functions, so we never have to write them again, just load them.

# A function is the functional unit in R. R is built around vectorization and function writing.
# Once you understand this you'll get better at understanding why R does things the way it does them. And be able to fix functions or use them for your own troubleshooting
# A function is anything that has two parenthesis next to it. 
# But a function is simply a way to package up code and run it in a confined space. For example lets look at sd

sd

# you see that? That's actually the underlying code in the function.

# but why do this?
# 1. It's much cleaner. Typing sd() and not having anyone worry about the underlying stuff can be good for readability.
# 2. It's reproduciable, and no one can mess with the function. This code will work everytime the way its supposed to, there's no accidently messing up the code. You can hand this to someone and it will always work. And then hark back to #1
# 3. Much easier to give finished product to collaborator or someone with lower R understanding. Including package building
# 4. It's more memory friendly.

### CHALLENGE 

# I want to create a function that converts farenheit measurements to celsius. Lets see if we can do that real quick

# (F - 32) * 5/9




# What if we want to add an extra layer of complexity? 
# nesting functions

# discuss default arguments



#of course you can make more complex functions with multiple inputs and that can produce other types of results

#data simulation example

#functions don't just have to conduct deterministic calculations for you, they can do a lot of things
#for example, they can make randomly generated data sets. These can be useful to test out analyses that you might want to do or validate a methodological approach
#To do this, we are going to use a similar framework as before but we will now use the rnorm() function that generates data points based upon a user-defined normal distribution.

dat.gen <- function(mu1, sd1,  nsamps, seed){
  set.seed(seed)
  dat <- rnorm(n = nsamps, mean = mu1, sd = sd1)
  return(dat)
}

#then we run the function using some parameters (mean1 = 4, sd1 = 1, n = 100, seed =3)
dat1 <- dat.gen( , , , )


#we can look at the data that we generated
summary(dat1)
hist(dat1)

#if we run it again, we get a different randomly generated data set if we change the seed
dat2 <- dat.gen( , , , )
summary(dat2)
hist(dat2)

#of course because we allowed mu, sd, and nsamps we can easily produce normal distributions of varying moments

hist(dat.gen(10, 10, 100, 3))
hist(dat.gen(-6, 20, 100, 3))
hist(dat.gen(0, 1, 100, 3))

#finally, you can use the apply commands to apply functions across a wide range of data types

#using our dat.gen function, we'll make a data table with some randomly generated data in it

dat <- data.frame(v1 = dat.gen(1, 1, 100, 3), v2 = dat.gen(5, 2, 100, 3), v3 = dat.gen(-2, 3, 100, 3))

#We can apply that function across either the rows or columns of the data.frame.
#I tell apply that we want to apply a function across rows (1) of our data.frame (dat)
#first let's use a function that is in base R already, sum

dat$sum <- apply(dat, 1, sum)

#this should add up each column in a row and add it as a new column in the data.frame, let's have a look at it
View(dat)

#what if we want to apply a function that we made? Let's use our sem() function from earlier and see how this works
dat$SE <- apply(dat, 1, sem)

#we added a new column to the data.frame so now we can have a look at how that works too
View(dat)

###################################################
# Summary of if and for loops, functions, and apply
#####################################################################

##-'for' loops are best to do tasks that can not be vectorized or when memory is an issue
##- 'for' loops are ideal for looping through files or starter values
##-'If' statements should only be used if vectorizing was not an option
##-If you have to make more than one 'if' statement you can probably vectorize it
##-Neither is ideal for math, since R is very good at doing this
##-Exceptions are when a function can not be vectorized or when referencing previous values
##-Also for lowering memory usage 
##(If your files exceed 1.5gb looping or other packages may be requried)
##-apply() is a great way to use functions over vectors or lists and can be thought of as more efficient for loops

#end of lesson
