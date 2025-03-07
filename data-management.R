##DATA MANAGEMENT

#There are 3 primary packages that the vast majority of R users use for data management
#They are base R, data.table, and dplyr . 

# The dplyr package is the most predominant package for data management.

##The dplyr verbs
##The dplyr package includes five main functions for managing data: mutate(), select(), filter(), arrange(), and summarise(). These five functions are often referred to as the dplyr verbs.

library(dplyr)
# No problem
df <- tibble(
  id = c(1, 2, 3),
  x  = c(0, 1, 0)
)

df %>% 
  filter(x == 0)

# Problem
l <- list(
  id = c(1, 2, 3),
  x  = c(0, 1, 0)
) 

l %>% 
  filter(x == 0)

#The … argument
#The ... argument can be used to pass multiple arguments to a function without knowing exactly what those arguments will look like ahead of time – including entire expressions.
df %>% 
  filter(x == 0)
#x is an object (i.e. a column in the data frame), == is a function (remember that operators are functions in R), and 0 is a value.

#Non-standard evaluation
#For example,
df %>% 
  filter(df$x == 0)

df %>% 
  filter(x == 0)
#When we don’t tell a dplyr verb exactly which data frame a column lives in, then the dplyr verb will assume it lives in the data frame that is passed to the .data argument

##Creating Modifying Columns
#1. Un=sing name value pairs to add columns to a data frame during its initial creation.
#2. Dollar sign notation.
#3. Bracket notation
#4. The mutate() function from dplyr package.

##Creating Data frames.
class <- tibble(
  names   = c("John", "Sally", "Brad", "Anne"),
  heights = c(68, 63, 71, 72)
)
class

#Dollar sign Notation.
class$heights

class$heights <- class$heights / 12 #We use dollar sign notation to create and/or modify columns in our data frame.
class

#we converted the values in the heights column from inches to feet. by divin=ding the heights column by 12.
class$grades <- c(89, 92, 86, 98)
class
#We created a new column in our class data frame using dollar sign notation.


#Bracket notation
class$heights[3]

class[["heights"]] #we can also use bracket notation to access or “get” the entire column. 

class[["heights"]] <- class[["heights"]] * 12 #we can also create and/or modify columns in our data frame using bracket notation.
class

#let’s go ahead and add one more variable to our data frame using bracket notation.
class[["rank"]] <- c(3, 2, 4, 1)
class

#we can also access, create, and modify data frame columns using single brackets.
class["heights"] #however, that this returns a different result than class$heights and class[["heights]]


##Modify Individual values.
class$heights[3]

#we can also get the result above using only bracket notation.
class[["heights"]][3]

study_data$site[3] <- "TX"
study_data


##The mutate() function
#the first two arguments to mutate() are .data and ...
#The value passed to .data should always be a data frame. we will often pass data frames to the .data argument using the pipe operator (e.g., df %>% mutate())
#The value passed to the ... argument should be a name-value pair or multiple name value pairs separated by commas. 

set.seed(123)

drug_trial <- tibble(
  # Study id, there are 20 people enrolled in the trial.
  id = rep(1:20, each = 3),
  # Follow-up year, 0 = baseline, 1 = year one, 2 = year two.
  year = rep(0:2, times = 20),
  # Participant age a baseline. Must be between the ages of 35 and 75 at 
  # baseline to be eligible for the study
  age = sample(35:75, 20, TRUE) %>% rep(each = 3),
  # Drug the participant received, Placebo or active
  drug = sample(c("Placebo", "Active"), 20, TRUE) %>% 
    rep(each = 3),
  # Reported headaches side effect, Y/N
  se_headache = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.95,.05)), 
    sample(0:1, 60, TRUE, c(.10, .90))
  ),
  # Report diarrhea side effect, Y/N
  se_diarrhea = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.98,.02)), 
    sample(0:1, 60, TRUE, c(.20, .80))
  ),
  # Report dry mouth side effect, Y/N
  se_dry_mouth = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.97,.03)), 
    sample(0:1, 60, TRUE, c(.30, .70))
  ),
  # Participant had myocardial infarction in study year, Y/N
  mi = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.85, .15)), 
    sample(0:1, 60, TRUE, c(.80, .20))
  )
)

##Here's what we did above;
#id: Study id, there are 20 people enrolled in the trial.
#year: Follow-up year, 0 = baseline, 1 = year one, 2 = year two.
#age: Participant age a baseline. Must be between the ages of 35 and 75 at baseline to be eligible for the study.
#drug: Drug the participant received, Placebo or active.
#se_headache: Reported headaches side effect, Y/N.
#se_diarrhea: Report diarrhea side effect, Y/N.
#se_dry_mouth: Report dry mouth side effect, Y/N.
#mi: Participant had myocardial infarction in study year, Y/N.

#We used the tibble() function above to create our data frame instead of the data.frame() function. This allows us to pass the drug column as a value to the if_else() function when we create se_headache, se_diarrhea, se_dry_mouth, and mi. If we had used data.frame() instead, we would have had to create se_headache, se_diarrhea, se_dry_mouth, and mi in a separate step.
#We used a new function, if_else(), above to help us simulate this data.
#We used a new function, sample(), above to help us simulate this data. We used this function to randomly assign values to age, drug, se_headache, se_diarrhea, se_dry_mouth, and mi instead of manually assigning each value ourselves.

# No set.seed - Random values
sample(1:100, 10, TRUE)

# No set.seed - Different random values
sample(1:100, 10, TRUE)

# Use set.seed - Random values
set.seed(456)
sample(1:100, 10, TRUE)

# Use set.seed with different value - Different random values
set.seed(789)
sample(1:100, 10, TRUE)


##Adding and modifying a single column
drug_trial %>% 
  mutate(complete = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
#This is tidious and scalable at all.

drug_trial %>% 
  mutate(complete = 0)
#Easy. Just pass the value to the name-value pair once and R will use it in every row.


##Recycling Rules
drug_trial %>% 
  mutate(complete = c(0, 1))
#There is an error. tidyverse functions throw errors when you try to recycle anything other than a single number. They are designed this way to protect you from accidentally getting unexpected results.

drug_trial$complete <- c(0,1)

drug_trial

class(drug_trial)
# but this is telling us that drug_trial is a tibble – an enhanced data frame. Remember, we created drug_trial using the tibble() function instead of the tibble() function. Because tibbles are part of the tidyverse they throw the same recycling errors that the mutate() function did above.

drug_trial_df <- as.data.frame(drug_trial)
class(drug_trial_df)

drug_trial_df$complete <- c(0,1)
drug_trial_df

drug_trial_df$complete <- c(0, 1, 2, 3, 4, 5, 6) # 7 values
#60 / 7 = 8.571429 – not an integer. Because there is no integer value that we can multiply by 7 to get the number 60, R throws us an error telling us that it isn’t able to use the recycling rules.

nums <- 1:10
nums

nums + 1

nums + c(1, 2)
#R uses the recycling rules to change the length of the short vector to match the length of the longer vector, and then performs the operation – in this case, addition.


nums + c(1, 2, 3)

#Because there is no integer value that we can multiply by 3 to get the number 10, R throws us an error telling us that it isn’t able to use the recycling rules.


drug_trial %>% 
  mutate(complete = 0)

drug_trial %>% 
  mutate(complete = rep(0, 60))


##Using existing variables in name-value pairs
drug_trial %>% 
  mutate(mi_f = factor(mi, c(0, 1), c("No", "Yes")))
#we told R to go get the values of the column mi, do some stuff to those values, and then assign those modified values to a column in the data frame and name that column mi_f.

drug_trial %>% 
  mutate(age_center = age - mean(age))

#Adding or modifying columns
drug_trial %>% 
  mutate(
    se_headache_f  = factor(se_headache, c(0, 1), c("No", "Yes")),
    se_diarrhea_f  = factor(se_diarrhea, c(0, 1), c("N0", "Yes")),
    se_dry_mouth_f = factor(se_dry_mouth, c(0, 1), c("No", "Yes"))
  )

#We created three new factor columns in the drug_trial data called se_headache_f, se_diarrhea_f, and se_dry_mouth_f.
#We created all columns inside a single mutate() function.

# Create a vector of 0/1 levels that can be reused below.
yn_levs <- c(0, 1)
# Create a vector of "No"/"Yes" labels that can be reused below.
yn_labs <- c("No", "Yes")

drug_trial %>% 
  mutate(
    se_headache_f  = factor(se_headache, yn_levs, yn_labs),
    se_diarrhea_f  = factor(se_diarrhea, yn_levs, yn_labs),
    se_dry_mouth_f = factor(se_dry_mouth, yn_levs, yn_labs)
  )

#Rowwise mutations
#we have three variables in our drug_trial data that capture information about whether or not the participant reported side effects including headache, diarrhea, and dry mouth (sounds like every drug commercial that exists 😂). What if we want to know if our participants reported any side effect at each follow-up? That requires us to combine and transform data from across three different columns! This is one of those situations where there are many different ways we could accomplish this task, but I’m going to use dplyr’s rowwise() function to do so in the following code:
drug_trial %>% 
  rowwise() %>% 
  mutate(any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) > 0)
#We created a new column in the drug_trial data called any_se_year using the mutate() function.
#rowwise() tells R to do any calculations that follow across columns instead within columns.
#The value we passed to the name-value pair inside mutate() was actually the result of two calculations.
#1. First, R summed the values of se_headache, se_diarrhea, and se_dry_mouth (i.e., sum(se_headache, se_diarrhea, se_dry_mouth)).
#2. Next, R compared that the summed value to 0. If the summed value was greater than 0, then the value assigned to any_se_year was TRUE. Otherwise, the value assigned to any_se_year was FALSE.

#We'll start with rowwise()
drug_trial_sub <- drug_trial %>% 
  select(id, year, starts_with("se")) %>% 
  print()

#most built-in R functions are vectorized. They do things to entire vectors, and data frame columns are vectors. So, without using rowwise() the sum() function would have returned the value 54
drug_trial_sub %>% 
  mutate(any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth))

sum(c(0, 1, 0)) #hint as to why it returns 54
sum(c(1, 1, 0))
sum(
  c(0, 1, 0),
  c(1, 1, 0)
)

sum(drug_trial_sub$se_headache)

sum(drug_trial_sub$se_diarrhea)

sum(drug_trial_sub$se_dry_mouth)

sum(
  drug_trial_sub$se_headache,
  drug_trial_sub$se_diarrhea,
  drug_trial_sub$se_dry_mouth
)
#The sum() function is taking the total of all three vectors added together, which is a single number (54), and then using recycling rules to assign that value to every row of any_se_year.


#rowwise() tells R to add across the columns instead of within the columns.
# So, add the first value for se_headache to the first value for se_diarrhea to the first value for se_dry_mouth, assign that value to the first value of any_se_year, and then repeat for each subsequent row.

drug_trial_sub %>% 
  rowwise() %>% 
  mutate(any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth))

# Is 0 greater than 0?
0 > 0

# Is 2 greater than 0?
2 > 0

#Instead of using a number on the left side of the inequality, we can use our calculated n_se_year variable values on the left side of the inequality
drug_trial_sub %>% 
  rowwise() %>% 
  mutate(
    n_se_year   = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0
  )

drug_trial_sub %>% 
  rowwise() %>% 
  mutate(any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) > 0)
#any_se_year is TRUE if the participant reported any side effect in that year and false if they reported no side effects in that year.

drug_trial_sub %>% 
rowwise() %>% 
  mutate(
    any_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) > 0,
    all_se_year = sum(se_headache, se_diarrhea, se_dry_mouth) == 3
  )

drug_trial_sub %>% 
  rowwise() %>% 
  mutate(
    n_se_year   = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3
  )


drug_trial_sub %>% 
  mutate(
    se_headache  = factor(se_headache, yn_levs, yn_labs),
    se_diarrhea  = factor(se_diarrhea, yn_levs, yn_labs),
    se_dry_mouth = factor(se_dry_mouth, yn_levs, yn_labs)
  ) %>% 
  rowwise() %>% 
  mutate(
    n_se_year   = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3
  )
#The sum() function cannot add factors.

#Group_by mutations
drug_trial_sub %>% 
  rowwise() %>% 
  mutate(
    n_se_year   = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3
  ) %>% 
  group_by(id) %>% 
  mutate(any_se = sum(any_se_year) > 0)

#We created a new column in the drug_trial_sub data called any_se using the mutate() function.
#The any_se column is TRUE if the participant reported any side effect in any year and FALSE if they never reported a side effect in any year.
#We first grouped the data by id using the group_by() function. Note that grouping the data by id with group_by() overrides grouping the data by row with rowwise() as soon as R gets to that point in the code. In other words, the data is grouped by row from rowwise() %>% to group_by(id) %>% and grouped by id after.


drug_trial_sub %>% 
  rowwise() %>% 
  mutate(
    n_se_year   = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3
  ) %>% 
  mutate(any_se = sum(any_se_year) > 0)
#The drug_trial_sub data frame was split into twenty separate little data frames
#Because there are 3 rows for each study id, each of these 20 little data frames had three rows.
#Each of those 20 little data frames was then passed to the mutate() function
#The name-value pair inside the mutate() function any_se = sum(any_se_year) > 0 told R to add up all the values for the column any_se_year (i.e., sum(any_se_year)), compare that summed value to 0 (i.e., sum(any_se_year) > 0), and then assign TRUE to any_se if the summed value is greater than zero and FALSE otherwise.


any_se_year <- c(TRUE, TRUE, TRUE)
any_se_year

sum_any_se_year <- sum(any_se_year)
sum_any_se_year

any_se <- sum_any_se_year > 0
any_se


##SUBSETTING DATA FRAMES
# Load dplyr
library(dplyr)

set.seed(123)

drug_trial <- tibble(
  # Follow-up year, 0 = baseline, 1 = year one, 2 = year two.
  year = rep(0:2, times = 20),
  # Participant age a baseline. Must be between the ages of 35 and 75 at 
  # baseline to be eligible for the study
  age = sample(35:75, 20, TRUE) %>% rep(each = 3),
  # Drug the participant received, Placebo or active
  drug = sample(c("Placebo", "Active"), 20, TRUE) %>% 
    rep(each = 3),
  # Reported headaches side effect, Y/N
  se_headache = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.95,.05)), 
    sample(0:1, 60, TRUE, c(.10, .90))
  ),
  # Report diarrhea side effect, Y/N
  se_diarrhea = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.98,.02)), 
    sample(0:1, 60, TRUE, c(.20, .80))
  ),
  # Report dry mouth side effect, Y/N
  se_dry_mouth = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.97,.03)), 
    sample(0:1, 60, TRUE, c(.30, .70))
  ),
  # Participant had myocardial infarction in study year, Y/N
  mi = if_else(
    drug == "Placebo", 
    sample(0:1, 60, TRUE, c(.85, .15)), 
    sample(0:1, 60, TRUE, c(.80, .20))
  )
)

drug_trial

#we now know how to use the mutate() function to columns to our data frame.
drug_trial <- drug_trial %>% 
  mutate(
    # Study id, there are 20 people enrolled in the trial.
    id = rep(1:20, each = 3)
  ) %>% 
  print()

#The select() function.
drug_trial %>% 
  select(id, year, age, se_headache, se_diarrhea, se_dry_mouth, mi)
#We used the select() function to change the order of the columns in the drug_trial data frame so that id would be the first variable in the data frame when reading from left to right.

#we could have used the tidy-select everything() function to make our code easier to write and we wouldn’t have accidently missed the drug column.
drug_trial <- drug_trial %>% 
  select(id, everything()) %>% 
  print()
#everything() tells R to do X (in this keep) to all the other variables not explicitly mentioned.


drug_trial <- drug_trial %>% 
  mutate(age_center = age - mean(age)) %>% 
  print()

#I would use select() to get a clearer picture:
drug_trial %>% 
  select(age, age_center)  #We used the select() function to view the age and age_center columns only.

drug_trial %>% 
  select(id, year, starts_with("se"))  #We used the select() function to view the id year, se_headache, se_diarrhea, and se_dry_mouth columns only.
#We used the tidy-select starts_with() function to select all the side effect variables.

#Because all of the side effect columns are directly next to each other (i.e., no columns in between them) we could have also used the colon operator : like this:
drug_trial %>% 
  select(id, year, se_headache:se_dry_mouth)

# Add the side effect factor columns to our data frame again...
yn_levs <- c(0, 1)
yn_labs <- c("No", "Yes")

drug_trial <- drug_trial %>% 
  mutate(
    se_headache_f  = factor(se_headache, yn_levs, yn_labs),
    se_diarrhea_f  = factor(se_diarrhea, yn_levs, yn_labs),
    se_dry_mouth_f = factor(se_dry_mouth, yn_levs, yn_labs)
  )

drug_trial %>% 
  select(id, year, ends_with("_f"))


#We can also select columns we want to keep by position instead of name.
drug_trial %>% 
  select(1:2, 4)  #We passed column numbers to the select() function to keep the 1st, 2nd, and 4th columns from our drug_trial data frame.

##we can also use select() to explicitly drop columns from our data frame. To do so, we just need to use either the subtraction symbol (-) or the Not operator (!)
drug_trial_sub <- drug_trial %>% 
  rowwise() %>% 
  mutate(
    n_se_year   = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3
  ) %>% 
  group_by(id) %>% 
  mutate(any_se = sum(any_se_year) > 0) %>% 
  ungroup() %>% 
  select(id:year, n_se_year:any_se) %>% 
  print()


drug_trial_sub %>% 
  select(-n_se_year)

drug_trial_sub %>% 
  select(!n_se_year)


##The rename() function
nhanes <- tibble(
  SEQN = c(1:4),
  ALQ101 = c(1, 2, 1, 2),
  ALQ110 = c(2, 2, 2, 1)
) %>% 
  print()

#We previously learned how to change these column names on import (i.e., col_names), but let’s say we didn’t do that for whatever reason. We can rename columns in our data frame using the rename() function.
nhanes %>% 
  rename(
    id = SEQN,
    drinks_12_year = ALQ101,
    drinks_12_life = ALQ110
  ) #We used the rename() function to change the name of each column in the drug_trial data frame to be more informative.


#If we wanted to keep the original names – just coerce them to lowercase. We can do that using the rename_with() variation of the rename() function in combination with the tolower() function
nhanes %>% 
  rename_with(tolower)


##The filter() function
#We can keep and drop rows in our data frame using the filter() function or the slice() function.
drug_trial %>% 
  select(1:2, 4)

drug_trial %>% 
  slice(1:5) #We used the slice() function to keep only the first 5 rows in the drug_trial data frame.


## Subgroup analysis.
drug_trial %>% 
  filter(year == 0)

#We used the filter() function to keep only the rows in the drug_trial data frame that contain data from the baseline year.
drug_trial %>% 
  filter(year == 0) %>% 
  group_by(drug, se_headache_f) %>% 
  summarise(n = n())

#We can show this more explicitly by using passing the value FALSE to the .drop argument of group_by(). This tells R to keep all factor levels in the output.
drug_trial %>% 
  filter(year == 0) %>% 
  filter(age < 65) %>% 
  group_by(drug, se_headache_f, .drop = FALSE) %>% 
  summarise(n = n())

#we could make our code above more succinct by combining our two filter functions into one:
drug_trial %>% 
  filter(year == 0 & age < 65) %>% 
  group_by(drug, se_headache_f, .drop = FALSE) %>% 
  summarise(n = n())
#We used the filter() function to keep only the rows in the drug_trial data frame that contain data from the baseline year AND (&) contain data from rows with a value that is less than 65 in the age column.


##Complete case analysis.

drug_trial_short <- drug_trial %>%
  filter(year == 0) %>% 
  slice(1:10) %>% 
  mutate(
    age  = replace(age, 1, NA),
    drug = replace(drug, 4, NA)
  ) %>% 
  print()

#We used the filter() and slice() functions to create a new data frame that contains only a subset of our original drug_trial data frame. 
#We used the replace() function to replace the first value of age with NA and the fourth value of drug with NA.


drug_trial_short %>% 
  group_by(drug) %>% 
  summarise(mean_age = mean(age))

drug_trial_short %>% 
  group_by(drug) %>% 
  summarise(mean_age = mean(age, na.rm = TRUE)) # we can improve our result is by adding the na.rm argument to the mean() function.

#we previously saw how it can sometimes be more efficient to drop the row with missing data from the data frame explicitly. This is called a complete case analysis or list-wise deletion

#We can easily drop the row with the missing value by adding an additional value to the ... argument of our filter() function
drug_trial_short %>% 
  filter(!is.na(age) & !is.na(drug)) %>% 
  group_by(drug) %>% 
  summarise(mean_age = mean(age))

##Deduplication
df <- tribble(
  ~id, ~day, ~x,
  1, 1, 1,
  1, 2, 11,
  2, 1, 12,
  2, 2, 13,
  2, 2, 14,
  3, 1, 12,
  3, 1, 12,
  3, 2, 13,
  4, 1, 13,
  5, 1, 10,
  5, 2, 11,
  5, 1, 10
) %>% 
  print()

#ID 2 has row with duplicate values for id and day, but a non-duplicate value for x. These rows are partial duplicates.
#ID 3 has a row with duplicate values for all three columns (i.e., 3, 1, 12). These rows are complete duplicates.
#ID 5 has a row with duplicate values for all three columns (i.e., 5, 1, 10). These rows are complete duplicates. However, they are not in sequential order in the dataset.


##The distinct() function
df %>% 
  distinct() #We can use dplyr’s distinct() function to remove all complete duplicates from the data frame.
#We used the distinct() function to keep only one row from a group of complete duplicate rows in the df data frame.

#Complete duplicate row add tag
df %>% 
  mutate(dup = duplicated(df))

df %>% 
  group_by_all() %>%  #We used the group_by_all() function to split our data frame into multiple data frames grouped by all the columns in df.
  mutate(
    n_row = row_number(),
    dup   = n_row > 1
  )


df %>% 
  mutate(dup = duplicated(.) | duplicated(., fromLast = TRUE))

#Partial duplicate rows
df %>% 
  distinct(id, day, .keep_all = TRUE)
#We used the distinct() function to keep only one row from a group of duplicate rows in the df data frame

#Partial duplicate rows - add tag
df %>% 
  group_by(id, day) %>% 
  mutate(
    count = row_number(), # Counts rows by group
    dup   = count > 1     # TRUE if there is more than one row per group
  )


#Count the number of duplicates
df %>% 
  group_by(id, day) %>% 
  filter(n() > 1) %>% 
  count()
