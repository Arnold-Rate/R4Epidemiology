install.packages("car")  # Contains Anova functions
install.packages("stats") #Built-in ANOVA package
install.packages("readr")
single_space <- read_delim(
  file = "data/single_delimited.txt",
  delim = " "
)

# Rows: 4 Columns: 4

# Delimiter: " "
# chr (3): id, sex, ht_in
# dbl (1): wgt_lbs
 
#Use `spec()` to retrieve the full column specification for this data.
#Specify the column types or set `show_col_types = FALSE` to quiet this message.

#read_delim() imports a data set with values that are delimited by a single space
#First arguement is the ##File arguement. This tells R where to find the data set.
#The second arguement is the ##delim arguement. this tells R which character separates each value in the data set.

single_space <- read_delim(
  file = "data/single_delimited.txt",
  delim = " ",
  na = "."
)

tab <- read_delim(
  file = "data/tab.txt",
  delim = "\t"
)

#the "\t" means "tab" to R. This tells R that the values are now separated by tabs.

tab <- read_tsv("data/tab.txt")

#Importing the width data.
fixed <- read_table("data/fixed_width.txt")
fixed <- read_table("data/fixed_width_no_space.txt")


##Vector of Column Widths
fixed <- read_fwf(
  file = "data/fixed_width_no_space.txt",
  col_positions = fwf_widths(
    widths    = c(3, 6, 5, 3),
    col_names = c("id", "sex", "ht_in", "wgt_lbs")
  ),
  skip = 1
)

##Paired vector of Start and end positions
fixed <- read_fwf(
  file = "data/fixed_width_no_space.txt",
  col_positions = fwf_positions(
    start     = c(1, 4, 10, 15),
    end       = c(3, 9, 11, 17),
    col_names = c("id", "sex", "ht_in", "wgt_lbs")
  ),
  skip = 1
)

##Using Named Arguements
read_fwf(
  file = "data/fixed_width_no_space.txt",
  col_positions = fwf_cols(
    id      = 3,
    sex     = 6,
    ht_in   = 5,
    wgt_lbs = 3
  ),
  skip = 1
)

##Column Positions:
read_fwf(
  file = "data/fixed_width_no_space.txt",
  col_positions = fwf_cols(
    id      = c(1, 3),
    sex     = c(4, 9),
    ht_in   = c(10, 11),
    wgt_lbs = c(15, 17)
  ),
  skip = 1
)

##Importing .csv files
csv <- read_csv("data/comma.csv")
#We used ##readr's read_csv() function to import the data set with values that are delimited by commas (csv)

csv <- read_csv(
  file = "data/comma_complex.csv",
  col_names = c("id", "sex", "ht_in", "wgt_lbs"),  #This tells R to only use words in the character vector as column instead of the default values in raw data.
  col_types = cols(
    col_character(), 
    col_character(),
    col_integer(),
    col_integer(),
    col_skip()
  ),
  skip = 3
)

csv



##IMPORTING BINARY FILES

#We use the ##readxl package to import Microsoft Excel files.
#We use ##haven package to import SAS and Stata data sets.
library(readxl)
library(haven)

excel <- read_excel("data/excel.xlsx")
excel

excel <- read_excel("data/excel_complex.xlsx")
excel

# A tibble: 8 × 3
#   `Height and Weight Study\r\nData Dictionary` ...2                                                   ...3                     
#   <chr>                                        <chr>                                                  <chr>                    
# 1 <NA>                                         <NA>                                                   <NA>                     
# 2 Variable                                     Definition                                             Type                     
# 3 Study ID                                     Randomly assigned participant id                       Continuous               
# 4 Assigned Sex at Birth                        Sex the participant was assigned at birth              Dichotomous (Female/Male)
# 5 Height (inches)                              Participant's height in inches                         Continuous               
# 6 Weight (lbs)                                 Participant's weight in pounds                         Continuous               
# 7 Date of Birth                                Participant's date of birth                            Date                     
# 8 Annual Household Income                      Participant's annual household income from all sources Continuous (Currency)

#we can get the result we wanted by making a few tweaks to the default values of the sheet, col_names, col_types, skip, and na arguments of the read_excel() function.
excel <- read_excel(
  path = "data/excel_complex.xlsx",
  sheet = "Study Phase 1",
  col_names = c("id", "sex", "ht_in", "wgt_lbs", "dob", "income"),
  col_types = c(
    "text",
    "text",
    "numeric",
    "numeric",
    "date",
    "numeric",
    "skip"
  ),
  skip = 3,
  na = c("", "NA", "Missing")
)

excel

#First arguement to the read_excel() function is the path arguement. It serves the file arguement to read_csv(), just with a different name.
#Sheet arguement here tells R which sheet of the excel workbook contains the data we want to import.
#Values we pass to the col_types arguement is now a vector of character strings instead of a list of functions nested in the col() function.
#-values that the col_types function will accept are:
#1. "skip" for telling R to ignore a column in the spreadsheet,
#2. "guess" tels R to guess the variable type,
#3. "logical" for logical variables
#4. "numeric" for numeric variables
#5. "date" for date variables
#6. "text" for character variables
#7. "list" for everything else


#Importing SAS data sets
sas <- read_sas("data/height_and_weight.sas7bdat")
sas

brfss_2018 <- read_xpt("data/LLCP2018XPT.zip")
head(brfss_2018)

nhanes_demo <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/DEMO_J.xpt")
head(nhanes_demo)

#Importing Stata data sets
stata <- read_stata("data/height_and_weight.dta")
stata

#We used haven's read_stata() function to import Stata data set