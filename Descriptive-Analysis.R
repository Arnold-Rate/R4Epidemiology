##Numerical Descriptions for categorical Variables
#Load dplyr for tibble ()
library(dplyr)
demo <- tibble(
  id  = c("001", "002", "003", "004"),  #id is the participants study id
  age = c(30, 67, 52, 56),  #age is the age at enrollment of the study
  edu = c(3, 1, 4, 2)  #edu is the highest level of formal education.
)

demo

demo <- tibble(
  id       = c("001", "002", "003", "004"),
  age      = c(30, 67, 52, 56),
  edu      = c(3, 1, 4, 2),
  edu_char = c(
    "Some college", "Less than high school", "College graduate", 
    "High school graduate"
  )
)

demo

#Coerce a numeric variable
demo <- demo %>% 
  mutate( 
    edu_f = factor(
      x      = edu,
      levels = 1:4,
      labels = c(
        "Less than high school", "High school graduate", "Some college", 
        "College graduate"
      )
    )
  )

demo

#We used the factor() to create a factor vector.

as.numeric(demo$edu_char)

as.numeric(demo$edu_f)

table(demo$edu_char)

table(demo$edu_f)

#we used table() function to get a count of the number of times each unique value of edu_char appears in our data frame.
#We then used table() function to get a count of the number of times each unique value appears in our data frame.

demo <- demo %>% 
  mutate(
    edu_5cat_f = factor(
      x      = edu,
      levels = 1:5,
      labels = c(
        "Less than high school", "High school graduate", "Some college", 
        "College graduate", "Graduate school"
      )
    )
  )

demo

table(demo$edu_char)
table(demo$edu_5cat_f)


#Coerce a character variable
#It is possible to coarce character vectors to factors.
demo <- demo %>% 
  mutate(
    edu_f_from_char = factor(
      x      = edu_char,
      levels = c(
        "Less than high school", "High school graduate", "Some college", 
        "College graduate", "Graduate school"
      )
    )
  )

demo

table(demo$edu_f_from_char)
# We coerced a character vector (edu_char) to a factor using the factor() function.
#Because the levels are character strings, there was no need to pass any values to the labels argument this time. Keep in mind, though, that the order of the values passed to the levels argument matters. It will be the order that the factor levels will be displayed in your analyses.

#Height and Weight data
library(dplyr)

# Simulate some data
height_and_weight_20 <- tibble(
  id = c(
    "001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", 
    "012", "013", "014", "015", "016", "017", "018", "019", "020"
  ),
  sex = c(1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2),
  sex_f = factor(sex, 1:2, c("Male", "Female")),
  ht_in = c(
    71, 69, 64, 65, 73, 69, 68, 73, 71, 66, 71, 69, 66, 68, 75, 69, 66, 65, 65, 
    65
  ),
  wt_lbs = c(
    190, 176, 130, 154, 173, 182, 140, 185, 157, 155, 213, 151, 147, 196, 212, 
    190, 194, 176, 176, 102
  )
)

height_and_weight_20

#Calculating frequencies.
table(height_and_weight_20$sex)

#Additionaly, we can use the CrossTable() function from the gmodels package, which gives us a little more information.

install.packages("gmodels")
gmodels::CrossTable(height_and_weight_20$sex)


#The Tidyverse way
#First we summarise the data frame
height_and_weight_20 %>% 
  summarise()

height_and_weight_20 %>% 
  summarise(n())
#Summarise() fuction counts the number of rows in the data frame.
#Adding (named n()) and one row with a value of 20 is the result we get.


# If we want o count the number of rows that have the value "Female" for sex_f, and then separately count the value "Male" for sex_f is our target.
#Simply we want to break the data frames into similar data frames. We use dplyr's group_by() function.
height_and_weight_20 %>%
  group_by(sex_f) %>% 
  summarise(n())

height_and_weight_20 %>%
  group_by(sex_f) %>% 
  summarise(n = n())
# We added n= to our summarised function so that our count column in the resulting data frame would be named n instead of n()


##Calculating Percentages.
height_and_weight_20 %>% 
  count(sex_f) %>% 
  mutate(prop = n / 20)
#Count() functio retunrs a data frame just like any other data frame, we can manipulate it in the same ways we can any other data frame.
#we used dplyr’s mutate() function to create a new variable in the data frame named prop. Again, we could have given it any valid name.
#We then set the value of prop to be equal to the value of n divided by 20


height_and_weight_20 %>% 
  count(sex_f) %>% 
  mutate(prop = n / sum(n))


height_and_weight_20 %>% 
  count(sex_f) %>% 
  mutate(percent = n / sum(n) * 100) # We multiply our proortion by 100 to convert it to a percentage.


##Missing Data.
height_and_weight_20 <- height_and_weight_20 %>% 
  mutate(sex_f = replace(sex, c(2, 9), NA)) %>% 
  print()

height_and_weight_20 %>% 
  filter(!is.na(sex_f)) %>% 
  count(sex_f) %>% 
  mutate(percent = n / sum(n) * 100)
#! is not an operator, it means "do the opposite"
# filter() tells R which rows of a data frame to keep, and is.na(sex_f) tells R to find rows with an NA value for the variable sex_f. Together, filter(is.na(sex_f)) would tell R to keep rows with an NA value for the variable sex_f. Adding the NOT operator ! tells R to do the opposite – keep rows that do NOT have an NA value for the variable sex_f.

#Formatting results
height_and_weight_20 %>% 
  filter(!is.na(sex_f)) %>% 
  count(sex_f) %>% 
  mutate(percent = (n / sum(n) * 100) %>% round(2))
#We passed the calculated percentage values (n / sum(n) * 100) to the round() function to round our percentages to 2 decimal places.
#alternatively we could have written our R code this way: mutate(percent = round(n / sum(n) * 100, 2))


#Using Freqtables
# You may be asked if you want to update other packages on your computer that
# freqtables uses. Go ahead and do so.
install.packages("freqtables")
library(freqtables)
height_and_weight_20 %>%
  filter(!is.na(sex_f)) %>%  #We used filter() to keep only the rows that have a non-missing value for sex and passed the data frame on to the freq_table() function using a pipe
  freq_table(sex_f) #We told the freq_table() function to create a univariate frequency table for the variable sex_f. A “univariate frequency table” just means a table (data frame) of useful statistics about a single categorical variable.

#var: The name of the categorical variable (column) we are analyzing.
#cat: Each of the different categories the variable var contains – in this case “Male” and “Female”.
#n: The number of rows where var equals the value in cat. In this case, there are 7 rows where the value of sex_f is Male, and 11 rows where the value of sex_f is Female.
#n_total: The sum of all the n values. This is also to total number of rows in the data frame currently being analyzed.
#percent: The percent of rows where var equals the value in cat.
#se: The standard error of the percent. This value is not terribly useful on its own; however, it’s necessary for calculating the 95% confidence intervals.
#t_crit: The critical value from the t distribution. This value is not terribly useful on its own; however, it’s necessary for calculating the 95% confidence intervals.
#lcl: The lower (95%, by default) confidence limit for the percentage percent.
#ucl: The upper (95%, by default) confidence limit for the percentage percent.


##Measures of Central Tendency
#Mean, Median, Mode
# Load the dplyr package. We will need several of dplyr's functions in the 
# code below.
library(dplyr)
# Simulate some data
height_and_weight_20 <- tribble(
  ~id,   ~sex,     ~ht_in, ~wt_lbs,
  "001", "Male",   71,     190,
  "002", "Male",   69,     177,
  "003", "Female", 64,     130,
  "004", "Female", 65,     153,
  "005", NA,       73,     173,
  "006", "Male",   69,     182,
  "007", "Female", 68,     186,
  "008", NA,       73,     185,
  "009", "Female", 71,     157,
  "010", "Male",   66,     155,
  "011", "Male",   71,     213,
  "012", "Female", 69,     151,
  "013", "Female", 66,     147,
  "014", "Female", 68,     196,
  "015", "Male",   75,     212,
  "016", "Female", 69,     19000,
  "017", "Female", 66,     194,
  "018", "Female", 65,     176,
  "019", "Female", 65,     176,
  "020", "Female", 65,     102
)

#The tribble() function creates something called a tibble. A tibble is the tidyverse version of a data frame. 
mean(height_and_weight_20$ht_in)
median(height_and_weight_20$ht_in)
mode(height_and_weight_20$ht_in) #Gives us not what we want.

mode_val <- function(x) {
  
  # Count the number of occurrences for each value of x
  value_counts <- table(x)
  
  # Get the maximum number of times any value is observed
  max_count <- max(value_counts)
  
  # Create and index vector that identifies the positions that correspond to
  # count values that are the same as the maximum count value: TRUE if so
  # and false otherwise
  index <- value_counts == max_count
  
  # Use the index vector to get all values that are observed the same number 
  # of times as the maximum number of times that any value is observed
  unique_values <- names(value_counts)
  result <- unique_values[index]
  
  # If result is the same length as value counts that means that every value
  # occured the same number of times. If every value occurred the same number
  # of times, then there is no mode
  no_mode <- length(value_counts) == length(result)
  
  # If there is no mode then change the value of result to NA
  if (no_mode) {
    result <- NA
  }
  
  # Return result
  result
}

mode_val(height_and_weight_20$ht_in) #@mode_val() takes a vector (or data frame column) as a value to its “x” argument and returns the mode value(s) of that vector.

#Compare the Central Tendencies.
height_and_weight_20 %>% 
  summarise(
    min_weight    = min(wt_lbs),
    mean_weight   = mean(wt_lbs),
    median_weight = median(wt_lbs),
    mode_weight   = mode_val(wt_lbs) %>% as.double(),
    max_weight    = max(wt_lbs)
  )

#Central Tendencies with Missing data
mean(c(1, 2, 3)) #All is well.
mean(c(1, NA, 3)) #Mean can't be NA

mean(c(1, NA, 3), na.rm = TRUE) #This will not always be true.

height_and_weight_20 <- height_and_weight_20 %>% 
  mutate(ht_in = replace(ht_in, c(1, 2), NA)) %>% #Replaced the 1st and 2nd value of ht_in with NA (missing) using the replace() function.
  print()

height_and_weight_20 %>% 
  summarise(
    min_height    = min(ht_in),
    mean_height   = mean(ht_in),
    median_height = median(ht_in),
    mode_height   = mode_val(ht_in),
    max_height    = max(ht_in)
  )

height_and_weight_20 %>% 
  summarise(
    min_height    = min(ht_in, na.rm = TRUE),
    mean_height   = mean(ht_in, na.rm = TRUE),
    median_height = median(ht_in, na.rm = TRUE),
    mode_height   = mode_val(ht_in),
    max_height    = max(ht_in, na.rm = TRUE)
  )


# You may be asked if you want to update other packages on your computer that
# meantables uses. Go ahead and do so.
install.packages("meantables")
library(meantables)

height_and_weight_20 %>%
  filter(!is.na(ht_in)) %>%
  mean_table(ht_in)

#he summary statistics in the table above include:
#response_var: The name of the variable (column) we are analyzing.
#n: The number of non-missing values of response_var being analyzed in the current analysis.
#mean: The mean of all n values of response_var.
#sem: The standard error of the mean of all n values of response_var.
#lcl: The lower (95%, by default) confidence limit for the percentage mean.
#ucl: The upper (95%, by default) confidence limit for the percentage mean.
#min: The minimum value of response_var.
#max: The maximum value of response_var.


##Measures of Dispersion.
library(dplyr)

## Simulate some data
height_and_weight_20 <- tribble(
  ~id,   ~sex,     ~ht_in, ~wt_lbs,
  "001", "Male",   71,     190,
  "002", "Male",   69,     177,
  "003", "Female", 64,     130,
  "004", "Female", 65,     153,
  "005", NA,       73,     173,
  "006", "Male",   69,     182,
  "007", "Female", 68,     186,
  "008", NA,       73,     185,
  "009", "Female", 71,     157,
  "010", "Male",   66,     155,
  "011", "Male",   71,     213,
  "012", "Female", 69,     151,
  "013", "Female", 66,     147,
  "014", "Female", 68,     196,
  "015", "Male",   75,     212,
  "016", "Female", 69,     19000,
  "017", "Female", 66,     194,
  "018", "Female", 65,     176,
  "019", "Female", 65,     176,
  "020", "Female", 65,     102
)

## Recreate our mode function
mode_val <- function(x) {
  value_counts <- table(x)
  result <- names(value_counts)[value_counts == max(value_counts)]
  if (length(value_counts) == length(result)) {
    result <- NA
  }
  result
}

height_and_weight_20 %>% 
  summarise(
    min_height    = min(ht_in),
    mean_height   = mean(ht_in),
    median_height = median(ht_in),
    mode_height   = mode_val(ht_in) %>% paste(collapse = " , "),
    max_height    = max(ht_in)
  )

#Range
height_and_weight_20 %>% 
  summarise(
    min_height  = min(ht_in),
    mean_height = mean(ht_in),
    max_height  = max(ht_in),
    range       = max_height - min_height
  )

#Getting R to to this math for us is really straightforward. We use R's var() function.
var(c(rep(58, 3), rep(78, 3))) #Instead of typing c(58, 58, 58, 78, 78, 78) I chose to use the rep() function. rep(58, 3) is equivalent to typing c(58, 58, 58) and rep(78, 3) is equivalent to typing c(78, 78, 78)

#Comparing Distributions
sim_data <- tibble(
  all_68     = rep(68, 20),
  half_58_78 = c(rep(58, 10), rep(78, 10)),
  even_58_78 = seq(from = 58, to = 78, length.out = 20),
  half_48_88 = c(rep(48, 10), rep(88, 10)),
  even_48_88 = seq(from = 48, to = 88, length.out = 20)
)
sim_data

#We created a data frame with 5 simulated distributions:
#all_68 has a value of 68 repeated 20 times
#half_58_78 is made up of the values 58 and 78, each repeated 10 times (similar to our example above)
#even_58_78 is 20 evenly distributed numbers between 58 and 78
#half_48_88 is made up of the values 48 and 88, each repeated 10 times
#even_48_88 is 20 evenly distributed numbers between 48 and 88
