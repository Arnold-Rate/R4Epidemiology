---
  title: "R4Epi Analysis"
author: "Arnold"
date: "`r Sys.Date()`"
output: html_document
---
  
  # We want to run R4Epi codes and commands
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

install.packages("dplyr")
# Now we load the package we installed.
library(dplyr)

class <- data.frame (names = c("John", "Sally", "Brad", "Anne"), heights = c(68, 63, 71, 72))

View(class)

mean(class$heights)

#Create an example vector
names <- c("John", "Sally", "Brad", "Anne")

# Our vector here contains 4 names
names

#Checking now the Vectro type
typeof(names)

my_numbers <- c(12.5,13.98765, pi)
typeof(my_numbers)

my_ints_1 <- c(1, 2, 3)
typeof(my_ints_1)

#Must put "L" behind the numer to make it an integrer.
my_ints_2 <- c(1L, 2L, 3L)
typeof(my_ints_2)

#NUmerical Description of Categorical Variables.
# A numeric vector of education categories
education_num <- c(3, 1, 4, 1)
education_num
# A character vecor of education categories
education_chr <- c("Some college", "Less than high school", "College graduate", "Less tha high school")
education_chr

#Coerce education_num to a factor
education_num_f <- factor(x=education_num, levels = 1:4, labels = c("Less than high school", "High school graduate", "Some college", "College graduate"))
education_num_f

typeof(education_num_f)
as.numeric(education_num_f)

#We can also coerce education_chr to a factor
education_chr_f <- factor(x = education_chr, levels = c("Less than high school", "High school graduate", "Some college", "College graduate"))
education_chr_f

#Data frames allows us combine vectors. Vectors contain only similar types of data.
#1. Create a vector of names
names <- c("John", "Sally", "Brad", "Anne")
#2. Create a vector of heights
heights <- c(68,63,71,72)
#3. Combine them into a data frame
class <- data.frame(names, heights)

#Print the data frame to the screen
class

#We can however create a data frame without creating individual ectors and combinng them, this can happen in a single code script.
#Create the class data frame
class <- data.frame(
  names = c("John", "Sally", "Brad", "Anne"),
  heights = c(68, 63, 71, 72)
)
class

#Both methods produce the same results

library(dplyr)

# We want to use tibbe function. Tibbles are data frames, they are just enhanced data frames.
# Create a data frame
my_df <- data.frame(
  name = c("john", "alexis", "Steph", "Quiera"),
  age = c(24, 44, 26, 25)
)

my_df #Print my_df to the screen
class(my_df) #View the class of my_df

#Use as_tibble() to turn my_df into a tibble
my_df <- as_tibble(my_df)
my_df #Print my_df to the screen
class(my_df) #View the class of my_df

#We can consequently use tibble() function in place of data.frame to create a tibble from scratch.

# We can use tribble() when creating it from scratch but it forms a different orientation.
# Create a data frame
my_df <- tribble(
  ~name,    ~age,
  "john",   24, 
  "alexis", 44, 
  "Steph",  26,
  "Quiera", 25
)

# Print my_df to the screen
my_df