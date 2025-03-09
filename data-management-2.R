##Working with Dates
#We have 2 different vector types that wee can use to store, and work with, dates.
#1.📅 date vectors for working with date values. By default, R will display dates in this format: 4-digit year, a dash, 2-digit month, a dash, and 2-digit day. 
#2. 📅🕓 POSIXct vectors for working with date-time values. Date-time values are just dates with time values added to them. By default, R will display date-times in this format: 4-digit year, a dash, 2-digit month, a dash, 2-digit day, a space, 2-digit hour value, a colon, 2-digit minute value, a colon, and 2-digit second value.

library(dplyr)
birth_dates <- readr::read_csv("~/Downloads/birth_dates.csv") #We used the read_csv() function to import a csv file containing simulated data into R.
birth_dates

#dob_actual is each person’s actual date of birth measured down to the second.
#dob_default is each person’s date of birth without their time of birth included.
#dob_typical is each person’s date of birth written in the format that is probably most often used in the United States: 2-digit month, a forward slash, 2-digit day, a forward slash, and 4-digit year.
#dob_long is each person’s date of birth written out in a sometimes-used long format.

##Dates under the hood
#Under the hood, R actually stores dates as numbers.

as.Date("2000-01-01")

unclass(as.Date("2000-01-01")) #January 1st, 2000 is apparently 10,957 days after January 1st, 1970.

unclass(as.Date("1970-01-01"))

unclass(as.Date("1970-01-02"))

unclass(as.Date("1969-12-31"))

#This numeric representation of dates also works in the other direction.
as.Date(10958, origin = "1970-01-01") #R will return a human-readable date.

from_sas <- tibble(
  date = c(10958, 10959, 10960)
)

from_sas %>% 
  mutate(new_date = as.Date(date, origin = "1960-01-01"))

#Coercing date-times to dates
birth_dates %>% 
  mutate(posix_to_date = as.Date(dob_actual)) %>% 
  select(dob_actual, posix_to_date)

#We created a new column in the birth_dates data frame called posix_to_date.
#We used the as.Date() function to coerce the date-time values in dob_actual to dates.
#We used the select() function to keep only the columns we are interested in comparing side-by-side in our output.
#Notice that dob_actual’s column type is still <S3: POSIXct>, but posix_to_date’s column type is <date>.


##Coercing character strings to dates
birth_dates %>% 
  mutate(dob_typical_to_date = as.Date(dob_typical)) %>% 
  select(dob_typical, dob_typical_to_date)

#all we have to do is tell R how to read this character string as a date using some of the symbols we learned about.

birth_dates %>% 
  mutate(dob_typical_to_date = as.Date(dob_typical, format = "%m %d %Y")) %>% 
  select(dob_typical, dob_typical_to_date)
#It didn’t work because we didn’t pass the forward slashes to the format argument.

birth_dates %>% 
  mutate(dob_typical_to_date = as.Date(dob_typical, format = "%m/%d/%Y")) %>% 
  select(dob_typical, dob_typical_to_date)
#We have to tell R that there are symbols mixed in with our date values in the character string we want to convert to a date.
#We did so by passing the value "%m/%d/%Y" to the format argument of the as.Date() function. These symbols tell R to read the character strings in dob_typical as 2-digit month (%m), a forward slash (/), 2-digit day (%d), a forward slash (/), and 4-digit year (%Y).


select(birth_dates, dob_long)

birth_dates %>% 
  mutate(dob_long_to_date = as.Date(dob_long, format = "%B %d, %Y")) %>% 
  select(dob_long, dob_long_to_date)
#We created a new column in the birth_dates data frame called dob_long_to_date.
#We used the as.Date() function to coerce the character string values in dob_long to dates.
#We did so by passing the value "%m/%d/%Y" to the format argument of the as.Date() function. These symbols tell R to read the character strings in dob_typical as 2-digit month (%m), a forward slash (/), 2-digit day (%d), a forward slash (/), and 4-digit year (%Y).

select(birth_dates, dob_long)

#The solution
birth_dates %>% 
  mutate(dob_long_to_date = as.Date(dob_long, format = "%B %d, %Y")) %>% 
  select(dob_long, dob_long_to_date)
# created a new column in the birth_dates data frame called dob_long_to_date
#We used the as.Date() function to coerce the character string values in dob_long to dates
#We did so by passing the value "%B %d, %Y" to the format argument of the as.Date() function. These symbols tell R to read the character strings in dob_long as full month name (%B), 2-digit day (%d), a comma (,), and 4-digit year (%Y).


##Change the appearance of dates with format()
birth_dates %>% 
  mutate(dob_abbreviated = format(dob_actual, "%d %b %y")) %>% 
  select(dob_actual, dob_abbreviated)
#We used the format() function to coerce the date values in dob_actual to character string values in dob_abbreviated.
#We did so by passing the value "%d %b %y" to the ... argument of the format() function. These symbols tell R to create a character string as 2-digit day (%d), a space (" "), abbreviated month name (%b), a space (" "), and 2-digit year (%y).
##Notice that dob_actual’s column type is still date_time (<S3: POSIXct>), but dob_abbreviated’s column type is character (<chr>). So, while dob_abbreviated looks like a date to us, it is no longer a date value to R. In other words, dob_abbreviated doesn’t have an integer representation under the hood. It is simply a character string.


#Some useful built-in dates
##Today’s date
Sys.Date()

lubridate::today() #These functions can be useful for calculating any length of time up to today.

#Today’s date-time
Sys.time()

lubridate::now()


set.seed(703)
rand_mill <- rnorm(1000000) #let’s generate 1,000,000 random numbers.

# Save the start time
start  <- lubridate::now()
sum    <- sum(rand_mill)
length <- length(rand_mill)
mean   <- sum / length
mean


# Save the stop time
stop   <- lubridate::now()

stop - start


# Save the start time
start  <- lubridate::now()
mean(rand_mill)


# Save the stop time
stop   <- lubridate::now()

stop - start


# Character vector of full month names
month.name

month.abb #Character vector of abbreviated month names

## Creating a vector containing a sequence of dates
seq.Date(
  from = as.Date("2020-01-01"),
  to   = as.Date("2020-01-15"),
  by   = "days"
)


##Calculating date intervals
ages <- birth_dates %>% 
  select(name_first, dob = dob_default) %>% 
  print()
#We created a new data frame called ages by subsetting the birth_dates data frame.
#We used the select() function to keep only the name_first and dob_default columns from birth_dates. We used a name-value pair (dob = dob_default) inside the select() function to rename dob_default to dob.

ages %>% 
  mutate(today = Sys.Date())

ages <- ages %>% 
  mutate(today = as.Date("2020-05-07")) %>% 
  print()
#We created a new column in the ages data frame called today.
#We made set the value of the today column to May 7th, 2020 by passing the value "2020-05-07" to the as.Date() function.


#Calculate age as the difference in time between dob and today
library(lubridate)

ages %>% 
  mutate(
    age_subtraction = today - dob,
    age_difftime    = difftime(today, dob),
    age_lubridate   = dob %--% today # lubridate's %--% operator creates a time interval
  )

#We created three new columns in the ages data frame called age_subtraction, age_difftime, and age_lubridate.
#We created age_subtraction using the subtraction operator (-). Remember, R stores dates values as numbers under the hood. So, we literally just asked R to subtract the value for dob from the value for today.
#We created age_difftime base R’s difftime() function.
#As you can see, the results returned by today - dob and difftime(today, dob) are identical.
#We created age_lubridate using lubridate’s time interval operator (%--%). Notice that the order of dob and today are switched here compared to the previous two methods. By itself, the %--% operator doesn’t return a time difference value. It returns a time interval value.

ages %>% 
  mutate(
    age_subtraction = as.numeric(today - dob) / 365.25,
    age_difftime    = as.numeric(difftime(today, dob)) / 365.25,
    age_lubridate   = (dob %--% today) / years(1)
  )

#We created three new columns in the ages data frame called age_subtraction, age_difftime, and age_lubridate.
#We used the as.numeric() function to convert the values of age_subtraction from a time differences to a number – the number of days. We then divided the number of days by 365.25 – roughly the number of days in a year. The result is age in years.
#We used the as.numeric() function to convert the values of age_difftime from a time differences to a number – the number of days. We then divided the number of days by 365.25 – roughly the number of days in a year. The result is age in years.
#We asked R to show us the time interval values we created age_lubridate using lubridate’s time interval operator (%--%) as years of time. We did so by dividing the time interval into years. Specifically, we used the division operator (/) and lubridate’s years() function. The value we passed to the years() function was 1. In other words, we asked R to tell us how many 1-year periods are in each time interval we created with dob %--% today.


years(1)
##lubridate’s method gives us a more precise answer than the first two methods do because it accounts for date complexities in a different way.


start <- as.Date("2017-03-01")
end   <- as.Date("2018-03-01") #Say we want to calculate the number of years between “2017-03-01” and “2018-03-01”.

# The base R way
as.numeric(difftime(end, start)) / 365.25

# The lubridate way
(start %--% end) / years(1)

##Notice that lubridate’s method returns exactly one year, but the base R method returns an approximation of a year.


start <- as.Date("2019-03-01")
end   <- as.Date("2020-03-01")

# The base R way
as.numeric(difftime(end, start)) / 365.25

# The lubridate way
(start %--% end) / years(1)


##Rounding time intervals
ages %>% 
  mutate(
    age_years = (dob %--% today) / years(1),
    # If you want the age (in years) as of the person's last birthday
    age_last  = trunc(age_years),
    # If you want to round the age to the nearest year
    age_near  = round(age_years)
  )
#We created age_last using the trunc() (for truncate) function. The value returned by the trunc() function can be interpreted as each person’s age in years at their last birthday.
#We created age_near using the round() function. The value returned by the round() function can be interpreted as each person’s age in years at their nearest birthday – which may not have occurred yet. 


ages %>% 
  mutate(
    # If you want the age (in years) as of the person's last birthday
    age_years = (dob %--% today) %/% years(1)
  ) #we can use the integer division operator (%/%) to calculate each person’s age in years at their nearest birthday without the trunc() function.


##Extracting out date parts
ages <- ages %>% 
  select(-today) %>% 
  print()

ages %>% 
  mutate(
    day   = day(dob),
    month = month(dob),
    year  = year(dob)
  )
#We created three new columns in the ages data frame called day, month, and year. We created them by passing the dob column to the x argument of lubridate’s day(), month(), and year() functions respectively.

ages %>% 
  mutate(
    wday         = wday(dob),
    day_full     = wday(dob, label = TRUE, abbr = FALSE),
    day_abb      = wday(dob, label = TRUE, abbr = TRUE),
    week_of_year = week(dob),
    week_cdc     = epiweek(dob)
  )

#We created five new columns in the ages data frame called wday, day_abb,day_full, week_of_year, and week_cdc. We created them by passing the dob column to the x argument of lubridate’s wday(), week(), and epiweek() functions respectively.
#The wday() function returns the day of the week the given date falls on. By default, the wday() returns an integer value between 1 and 7. We can adjust the values passed to wday()’s label and abbr arguments to return full day names (day_full) and abbreviated day names (day_abb).
#The week() function returns the week of the year the given date falls in. More formally, the week() function “returns the number of complete seven-day periods that have occurred between the date and January 1st, plus one.”
#The epiweek() function also returns the week of the year the given date falls in. However, it calculates the week in a slightly different way. Specifically, “it uses the US CDC version of epidemiological week.


##Sorting dates
# Oldest (top) to most recent (bottom)
# Ascending order
ages %>% 
  arrange(dob)


##If we want to sort our dates in descending order (i.e., most recent to oldest), we just pass the date column to the desc() function before passing it to the ... argument of the arrange() function.
# Most recent (top) to oldest (bottom)
# Descending order
ages %>% 
  arrange(desc(dob))


##Working with Character Strings
library(readr)
library(dplyr)
library(stringr) # All stringr functions begin with "str_"

ehr <- read_rds("~/Downloads/ehr.Rds")
ehr 
#We used the read_csv() function to import a .Rds file containing simulated data into R.
#The simulated data contains admission date (admit_date), the patient’s name (name), the patient’s date of birth (dob), the patient’s address (address), the city the patient lives in (city), and column that contains the symptoms each patient was experiencing at admission (symptoms).


ehr %>% 
  group_by(name) %>% 
  mutate(dup = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, dup, dob, address, city)

ehr %>% 
  arrange(name) %>% 
  pull(name)

#We dplyr’s pull() function to return the name column as a character vector. Doing so makes it easier to see some of the discrepancies in the way the patient’s names were entered into the ehr.

##Coerce to lowercase
#Lowercase
ehr %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_lower()

#Upper case
ehr %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_upper()

#Title case
ehr %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_title()

#Sentence case
ehr %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_sentence()

ehr <- ehr %>% 
  mutate(name = str_to_lower(name)) %>% 
  print()

#Lets check for unique data
ehr %>% 
  group_by(name) %>% 
  mutate(dup = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, dup, dob, address, city)

#Trim white space
str_trim("Ryan Edwards  ") #We can use stringr’s str_trim() function to “trim” white space from the beginning and end of character strings.

ehr <- ehr %>% 
  mutate(name = str_trim(name))

ehr %>% 
  group_by(name) %>% 
  mutate(dup = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, dup, dob, address, city)
#R has identified 2 rows with a duplicate name (dup == TRUE), which results in a count of 13 unique people

##Regular expressions (regex or regexps)
#Remove the comma
str_replace(
  string      = "weston fox,", 
  pattern     = ",",
  replacement = ""
)
#We used stringr’s str_replace() function remove the comma from the character string “weston fox,”.


#Let’s go ahead and use the str_replace() function now as the next step in cleaning our data
ehr <- ehr %>% 
  mutate(name = str_replace(name, ",", ""))

ehr %>% 
  group_by(name) %>% 
  mutate(dup = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, dup, dob, address, city)
#R has identified 3 rows with a duplicate name (dup == TRUE), which results in a count of 12 unique people.

#Remove middle initial
str_replace(
  string      = "tatum s chavez", #The first argument to the str_replace() function is string. The value passed the string argument should be the character string, or vector of character strings, we want to manipulate.
  pattern     = " \\w ", #The second argument to the str_replace() function is pattern. The value passed the pattern argument should be regular expression. It should tell the str_replace() function what part of the character string we want to replace. In this case, it is " \\w ". That is a space, two backslashes, a “w,” and a space. This regular expression looks a little stranger than the last one we saw.
  replacement = " " #The third argument to the str_replace() function is replacement. The value passed the replacement argument should also be regular expression. It should tell the str_replace() function what to replace the value identified in the pattern argument with. In this case, it is a single space (" ").
)

#Let’s go ahead and use the str_replace() function now as the next step in cleaning our data:
ehr <- ehr %>% 
  mutate(name = str_replace(name, " \\w ", " "))

ehr %>% 
  group_by(name) %>% 
  mutate(dup = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, dup, dob, address, city)

##Remove double spaces
str_replace(
  string      = "Ivy   Mccann", #The first argument to the str_replace() function is string. The value passed the string argument should be the character string, or vector of character strings, we want to manipulate.
  pattern     = "\\s{2,}", #The second argument to the str_replace() function is pattern. The value passed the pattern argument should be regular expression.
  replacement = " " #The third argument to the str_replace() function is replacement. The value passed the replacement argument should also be regular expression.
)

ehr <- ehr %>% 
mutate(name = str_replace(name, "\\s{2,}", " "))

ehr %>% 
  group_by(name) %>% 
  mutate(dup = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, dup, dob, address, city)

ehr %>% 
  group_by(name) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  summarise(`Unique People` = n())

#We saw the row_number() function used before inside of mutate() to sequentially count the number of rows that belong to each group created with group_by(). We could have done that in the code above. The filter(row_number() == 1) code is really just a shorthand way to write mutate(row = row_number()) %>% filter(row == 1). It has the effect of telling R to just keep the first row for each group created by group_by(). In this case, just keep the first row for each name in the data frame.

ehr_unique <- ehr %>% 
  group_by(name) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  print()

ehr %>% 
  group_by(city) %>% 
  summarise(n = n())

ehr <- ehr %>% 
  mutate(
    address = tolower(address),
    city    = tolower(city)
  )

ehr %>% 
  group_by(city) %>% 
  summarise(n = n())

str_replace(
  string      = "city of fort worth",
  pattern     = "city of ",
  replacement = ""
)

eehr <- ehr %>% 
  mutate(city = str_replace(city, "city of ", ""))

ehr %>% 
  group_by(city) %>% 
  summarise(n = n())

#Separate values into component parts
str_extract("zariah hernandez", "^\\w+") #The second argument to the str_extract() function is pattern. The value passed the pattern argument should be regular expression. It should tell the str_extract() function what part of the character string we want to pull out of the character string. In this case, it is ^\\w+.

str_extract("zariah hernandez", "\\w+$") #The second argument to the str_extract() function is pattern. The value passed the pattern argument should be regular expression. It should tell the str_extract() function what part of the character string we want to pull out of the character string. In this case, it is \\w+$.

##let’s use str_extract() to separate full name into name_first and name_last.
ehr <- ehr %>% 
  mutate(
    # Separate name into first name and last name
    name_first = str_extract(name, "^\\w+"),
    name_last  = str_extract(name, "\\w+$")
  ) 

ehr %>% 
  select(name, name_first, name_last)


##Dummy variables
ehr %>% 
  select(name_first, name_last, symptoms)

ehr %>% 
  group_by(symptoms) %>% 
  summarise(n = n())

ehr <- ehr %>% 
  mutate(
    pain     = str_detect(symptoms, "Pain"),
    headache = str_detect(symptoms, "Headache"),
    nausea   = str_detect(symptoms, "Nausea")
  )

ehr %>% 
  select(symptoms, pain, headache, nausea)

table(ehr$headache)

ehr %>% 
  select(pain) %>% 
  mutate(pain_01 = as.numeric(pain))


##Conditional Operations
library(dplyr)

rainy_days <- tibble(
  day     = 1:5,
  weather = c("rain", "rain", "no rain", "rain", "no rain")
) %>% 
  print()

rainy_days %>% 
  mutate(
    raincoat = if_else(
      condition = weather == "rain", 
      true      = "wear", 
      false     = "no wear"
    )
  )

if_else(TRUE, "wear", "no wear")

rainy_days %>% 
  mutate(
    raincoat = if_else(TRUE, "wear", "no wear")
  )
#Because the value passed to the condition argument is TRUE (in this case, literally), the if_else() function returns the value wear.

#the if_else() function returns the value wear because the value passed to the condition argument is TRUE. Then, R uses its recycling rules to copy the value wear to every row of the raincoat column.
rainy_days %>% 
  mutate(
    raincoat = if_else(FALSE, "wear", "no wear")
  )

#The if_else() function returns the value no wear because the value passed to the condition argument is FALSE. Then, R uses its recycling rules to copy the value no wear to every row of the raincoat column.

rainy_days %>% 
  mutate(
    raincoat = if_else(c(TRUE, TRUE, FALSE, TRUE, FALSE), "wear", "no wear")
  ) #We can take this a step further and actually pass a vector of logical (TRUE/FALSE) values to the condition argument.

rainy_days$weather == "rain"

#Operands and operators
df <- tibble(
  id        = c(1, 1, 2, 2),
  outcome   = c(0, 1, 1, 1)
) %>% 
  print()

df %>% 
  mutate(
    # Odd rows are treatment A
    # Even rows are treatment B
    treatment = if_else(row_number() %% 2 == 1, "A", "B")
  )

df <- tibble(
  name1 = c("Jon", "John", NA),
  name2 = c("Jon", "Jon", "Jon")
)

df %>% 
  mutate(
    name_match = name1 == name2
  )

df %>% 
  mutate(
    name_match = name1 == name2,
    name_match = if_else(is.na(name_match), FALSE, name_match)
  )
#The value we passed to the condition argument was is.na(name_match). In doing so, we asked R to check each value of the name_match column and see if it was NA.
#If it was NA, then we wanted to return the value that we passed to the true argument. Somewhat confusingly, the value we passed to the true argument was FALSE. All that means is that we wanted if_else() to return the literal value FALSE when the value for name_match was NA.
#If the value in name_match was NOT NA, then we wanted to return the value that we passed to the false argument. In this case, we asked R to return the value that already exists in the name_match column.


##Testing multiple conditions simultaneously
blood_pressure <- tibble(
  id     = 1:10,
  sysbp  = c(152, 120, 119, 123, 135, 83, 191, 147, 209, 166),
  diasbp = c(78, 60, 88, 76, 85, 54, 116, 95, 100, 106)
) %>% 
  print()


blood_pressure %>% 
  mutate(bp = if_else(sysbp < 120 & diasbp < 80, "Normal", "Not Normal"))

#We used dplyr’s if_else() function to create a new column in our data frame (bp) that contains information about whether each person has normal blood pressure or not.
#We actually passed two conditions to the condition argument. The first condition was that the value of sysbp had to be less than 120. The second condition was that the value of diasbp had to be less than 80.
#Because we separated these conditions with the AND operator (&), both conditions had to be true in order for the if_else() function to return the value we passed to the true argument – Normal. Otherwise, the if_else() function returned the value we passed to the false argument – Not Normal.
#Participant 2 had a systolic blood pressure of 120 and a diastolic blood pressure of 60. Although 60 is less than 80 (condition number 2), 120 is not less than 120 (condition number 1). So, the value returned by the if_else() function was Not Normal.
#Participant 3 had a systolic blood pressure of 119 and a diastolic blood pressure of 88 Although 119 is less than 120 (condition number 1), 88 is not less than 80 (condition number 2). So, the value returned by the if_else() function was Not Normal.
#Participant 6 had a systolic blood pressure of 83 and a diastolic blood pressure of 54. In this case, conditions 1 and 2 were met. So, the value returned by the if_else() function was Normal.


##Testing a sequence of conditions
blood_pressure %>% 
  mutate(
    bp = case_when(
      sysbp < 120 & diasbp < 80                               ~ "Normal",
      sysbp >= 120 & sysbp < 130 & diasbp < 80                ~ "Elevated",
      sysbp >= 130 & sysbp < 140 | diasbp >= 80 & diasbp < 90 ~ "Hypertension Stage 1",
      sysbp >= 140 | diasbp >= 90                             ~ "Hypertension Stage 2"
    )
  )
#The case_when() function only has a single argument – the ... argument. You should pass one or more two-sided formulas separated by commas to this argument.
#When the help documentation refers to a two-sided formula, it means this: LHS ~ RHS. Here, LHS means left-hand side and RHS means right-hand side.
#The LHS should be the condition or conditions that we want to test. You can think of this as being equivalent to the condition argument of the if_else() function.
#The RHS should be the value we want the case_when() function to return when the condition on the left-hand side is met. You can think of this as being equivalent to the true argument of the if_else() function.

#The case_when() function doesn’t have a direct equivalent to the if_else() function’s false argument. Instead, it evaluates each two-sided formula sequentially until if finds a condition that is met. If it never finds a condition that is met, then it returns an NA. 
#Finally, we assigned all the values returned by the case_when() function to a new column that we named bp.


##Recoding variables
#1 = child when the participant is less than 12 years old
#2 = adolescent when the participant is between the ages of 12 and less than 18
#3 = adult when the participant is 18 years old or older

# Simulate some age data
set.seed(123)
ages <- tibble(
  id  = 1:10,
  age = c(sample(1:30, 9, TRUE), NA)
) %>% 
  print()


ages %>% 
  mutate(
    age_3cat = case_when(
      age < 12 ~ 1
    )
  )


ages %>% 
  mutate(
    age_3cat = case_when(
      age < 12             ~ 1,
      age >= 12 & age < 18 ~ 2
    )
  )


ages %>% 
  mutate(
    age_3cat = case_when(
      age < 12             ~ 1,
      age >= 12 & < 18     ~ 2
    )
  )


ages %>% 
  mutate(
    age_3cat = case_when(
      age < 12             ~ 1,
      age >= 12 & age < 18 ~ 2,
      age >= 18            ~ 3
    )
  )

#case_when() is lazy
df <- tibble(
  number = c(1, 2, 3)
) %>% 
  print()


df %>% 
  mutate(
    size = case_when(
      number < 2 ~ "Small",
      number < 3 ~ "Medium",
      number < 4 ~ "Large"
    )
  )

#So, case_when() immediately returns the value on the right-hand side (Small) and does not continue checking two-sided formulas. It moves on to the next value of number.
ages %>% 
  mutate(
    age_3cat = case_when(
      age < 12  ~ 1,
      age < 18  ~ 2,
      age >= 18 ~ 3
    )
  )


##Recode missing
demographics <- ages %>% 
  mutate(
    race     = c(1, 2, 1, 4, 7, 1, 2, 9, 1, 3),
    hispanic = c(7, 0, 1, 0, 1, 0, 1, 9, 0, 1)
  ) %>% 
  print()


demographics %>% 
  mutate(
    # Recode 7 and 9 to missing
    race_recode = if_else(race == 7 | race == 9, NA, race),
    hispanic_recode = if_else(hispanic == 7 | hispanic == 9, NA, hispanic)
  )

demographics %>% 
  mutate(
    # Recode 7 and 9 to missing
    race_recode = if_else(race == 7 | race == 9, NA_real_, race),
    hispanic_recode = if_else(hispanic == 7 | hispanic == 9, NA_real_, hispanic)
  )

demographics %>% 
  mutate(
    # Recode 7 and 9 to missing
    race_recode     = if_else(race == 7 | race == 9, NA_real_, race),
    hispanic_recode = if_else(hispanic == 7 | hispanic == 9, NA_real_, hispanic),
    race_eth_4cat   = case_when(
      # White, non-Hispanic
      race_recode == 1 & hispanic_recode == 0 ~ 1,
      # Black, non-Hispanic
      race_recode == 2 & hispanic_recode == 0 ~ 2,
      # American Indian or Alaskan Native to Other race, non-Hispanic
      race_recode == 3 & hispanic_recode == 0 ~ 4,
      # Asian to Other race, non-Hispanic
      race_recode == 4 & hispanic_recode == 0 ~ 4,
      # Pacific Islander to Other race, non-Hispanic
      race_recode == 4 & hispanic_recode == 0 ~ 4,
      # Hispanic, any race
      hispanic_recode == 1                    ~ 3
    )
  )

demographics %>% 
  mutate(
    race_eth_4cat = case_when(
      is.na(hispanic) | hispanic %in% c(7, 9) ~ NA_real_, # Unknown ethnicity
      hispanic == 1                           ~ 3,        # Hispanic, any race
      is.na(race) | race %in% c(7, 9)         ~ NA_real_, # non-Hispanic, unknown race
      race == 1                               ~ 1,        # White, non-Hispanic
      race == 2                               ~ 2,        # Black, non-Hispanic
      TRUE                                    ~ 4         # Other race, non-Hispanic
    )
  )


demographics %>% 
  # Recode variables
  mutate(
    # Collapse continuous age into 3 categories
    age_3cat = case_when(
      age < 12  ~ 1, # child
      age < 18  ~ 2, # adolescent
      age >= 18 ~ 3  # adult
    ),
    age_3cat_f = factor(
      age_3cat, 
      labels = c("child", "adolescent", "adult")
    ),
    # Combine race and ethnicity
    race_eth_4cat = case_when(
      is.na(hispanic) | hispanic %in% c(7, 9) ~ NA_real_, # Unknown ethnicity
      hispanic == 1                           ~ 3,        # Hispanic, any race
      is.na(race) | race %in% c(7, 9)         ~ NA_real_, # non-Hispanic, unknown race
      race == 1                               ~ 1,        # White, non-Hispanic
      race == 2                               ~ 2,        # Black, non-Hispanic
      TRUE                                    ~ 4         # Other race, non-Hispanic
    ),
    race_eth_4cat_f = factor(
      race_eth_4cat,
      labels = c(
        "White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race",
        "Other race, non-Hispanic"
      )
    )
  )


##Working with Multiple Data Frames
library(dplyr)
trial <- tibble(
  year    = c(2016, 2017, 2018, 2019),
  n       = c(501, 499, 498, 502),
  outcome = c(51, 52, 49, 50) 
) %>% 
  print()


trial_2020 <- tibble(
  year    = 2020,
  n       = 500,
  outcome = 48 
) %>% 
  print()

trial %>% 
  bind_rows(trial_2020) #We can see above that column names and types in both data frames are identical. In this case, we can easily bind them together vertically with dplyr’s bind_rows() function:

#Combining more than 2 data frames
#Thankfully, bind_rows() lets us pass as many data frames as we want to the ... argument.
trial_2021 <- tibble(
  year      = 2021,
  n         = 598,
  outcome   = 57
) %>% 
  print()

trial %>% 
  bind_rows(trial_2020, trial_2021)

#Adding rows with differing columns
trial_2020 <- tibble(
  year      = 2020,
  n         = 500,
  outcome   = 48,
  adv_event = 3 # Here is the new column
) %>% 
  print()

trial %>% 
  bind_rows(trial_2020)

#Differing column positions
trial_2020 <- tibble(
  year      = 2020,
  n         = 500,
  adv_event = 3, # This was previously the fourth column
  outcome   = 48 # This is the thrid column in trial
) %>% 
  print()

trial %>% 
  bind_rows(trial_2020)


#Differing column names
trial_2020 <- tibble(
  year      = 2020,
  count     = 500,
  adv_event = 3,
  outcomes  = 48
) %>% 
  print()


trial %>% 
  bind_rows(trial_2020)


trial_2020_rename <- trial_2020 %>% 
  rename(
    n = count,
    outcome = outcomes
  )

trial %>% 
  bind_rows(trial_2020_rename)


trial %>% 
  bind_rows(
    trial_2020 %>% 
      rename(
        n = count,
        outcome = outcomes
      )
  )


#Combining data frames horizontally: Adding columns
df1 <- tibble(
  color = c("red", "green", "blue"),
  size  = c("small", "medium", "large")
) %>% 
  print()


df2 <- tibble(
  amount = c(1, 4, 3),
  dose   = c(10, 20, 30)
) %>% 
  print()


df1 %>% 
  bind_cols(df2)


##Combining data frames horizontally by key values
#The four mutating join functions are:
#1. left_join(). This is probably the join function that you will use the most. It’s important to remember that left_join() keeps all the rows from the x data frame in the resulting combined data frame. 
#2. right_join(). This is just the mirror opposite of left_join(). Accordingly, right_join() keeps all the rows from the y data frame in the resulting combined data frame, and only keep the rows from the x data frame that have a key value match in the y data frame. 
#3. full_join(). Full join keeps all the rows from both data frames in the resulting combined data frame. The values for columns with no key value match in the opposite data frame are set to NA.
#4. inner_join(). Inner join keeps only the rows from both data frames that have a key value match in the opposite data frame in the resulting combined data frame.

demographics <- tibble(
  id       = c("1001", "1002", "1003", "1004"),
  dob      = as.Date(c("1968-12-14", "1952-08-03", "1949-05-27", "1955-03-12")),
  race_eth = c(1, 2, 2, 4)
) %>% 
  print()

#The functional tests included measuring grip strength in their right hand (grip_r) and grip strength in their left hand (grip_l).
grip_strength <- tibble(
  id     = c("1002", "1001", "1003", "1004"),
  grip_r = c(32, 28, 32, 22),
  grip_l = c(30, 30, 28, 22)
) %>% 
  print()


#One-to-one relationship merge
demographics %>% 
  bind_cols(grip_strength)

demographics %>% 
  left_join(grip_strength, by = "id")

# Right join
demographics %>% 
  right_join(grip_strength, by = "id")

# Full join
demographics %>% 
  full_join(grip_strength, by = "id")

# Inner join
demographics %>% 
  inner_join(grip_strength, by = "id")

# Switching order
grip_strength %>% 
  left_join(demographics, by = "id")

## Differing rows
demographics <- tibble(
  id       = c("1001", "1002", "1003", "1004", "1005"),
  dob      = as.Date(c(
    "1968-12-14", "1952-08-03", "1949-05-27", "1955-03-12", "1942-06-07"
  )),
  race_eth = c(1, 2, 2, 4, 3)
) %>% 
  print()

demographics %>% 
  left_join(grip_strength, by = "id")

# Right join
demographics %>% 
  right_join(grip_strength, by = "id")

# Full join
demographics %>% 
  full_join(grip_strength, by = "id")

# Inner join
demographics %>% 
  inner_join(grip_strength, by = "id")

# Switching order
grip_strength %>% 
  left_join(demographics, by = "id")

#Differing key column names
grip_strength <- tibble(
  pid    = c("1002", "1001", "1003", "1004"),
  grip_r = c(32, 28, 32, 22),
  grip_l = c(30, 30, 28, 22)
) %>% 
  print()

demographics %>% 
  left_join(grip_strength, by = "id")

demographics %>% 
  left_join(grip_strength, by = c("id" = "pid"))

#One-to-many relationship merge
demographics

grip_strength <- tibble(
  id     = rep(c("1001", "1002", "1003", "1004"), each = 2),
  visit  = rep(c("pre", "post"), 4),
  grip_r = c(32, 33, 28, 27, 32, 34, 22, 27),
  grip_l = c(30, 32, 30, 30, 28, 30, 22, 26)
) %>% 
  print()

demographics %>% 
  left_join(grip_strength, by = "id")

#Multiple key columns
emr <- tibble(
  id     = rep(c("1001", "1002", "1003", "1004"), each = 2),
  visit  = rep(c("pre", "post"), 4),
  weight = c(105, 99, 200, 201, 136, 133, 170, 175)
) %>% 
  print()

demographics %>% 
  left_join(grip_strength, emr, by = "id")

demographics %>% 
  left_join(grip_strength, by = "id") %>% 
  left_join(emr, by = "id")


demographics %>% 
  left_join(grip_strength, by = "id") %>% 
  left_join(emr, by = c("id", "visit"))
