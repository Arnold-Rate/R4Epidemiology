#Missing Data
#Missing Data is usuklly represented as NA 
# Create the class data frame
data.frame(
  names   = c("John", "Sally", "Brad", "Anne"),
  heights = c(68, 63, 71, NA) # Now we are missing Anne's height
)

#Let us now handle Data and run statistical tests.
#1. Mean 
# Create the class data frame
class <- data.frame(
  names   = c("John", "Sally", "Brad", "Anne"),
  heights = c(68, 63, 71, 72)
) # Closing parenthesis down here.

# Print the data frame to the screen
class

class$heights

#Bracket Annotation is used to access each value in a vector.
#First we create a vector
heights <- c(68,63,71,72)
heights[3]

sum(heights) #To calculate the sum of pur data in the heights vector.

sum(class$heights) / 4 #One way to calculate the mean

length(heights) #Return the number of individual values in heights

mean(class$heights)

#Common errors.
class$weight <- c(160, 170, 180, 190)
mean (weight) #Here R sees weight not as an object but as a column. We have to tell R that weight is a column in class

mean(class$weight)

#Issues also arise when there is an object and a column with the same name.
scores <- c(5, 9, 3) #Object called scores
class$scores <- c(95, 97, 93, 100) #We created a column called scores.

mean (scores)

mean(class$scores)
#Scores object and scores column are two different things to R
class


