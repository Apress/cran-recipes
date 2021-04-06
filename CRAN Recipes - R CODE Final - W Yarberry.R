#R code for CRAN Recipes by William Yarberry
# May, 2021
# Apress
install.packages("tidyverse")
library(tidyverse)


# dplyr -------------------------------------------------------------------


data("mtcars")
six.cyl.only <- filter(mtcars, cyl == 6)
six.cyl.only 
six.cylinders.and.110.horse.power <- filter(mtcars, cyl == 6,
 hp == 110)
six.cylinders.and.110.horse.power

# OR logic ----------------------------------------------------------------


gear.eq.4.or.more.than.8 <- filter(mtcars, gear == 4|cyl > 6)
gear.eq.4.or.more.than.8

# filter  -----------------------------------------------------------------


smallest.engine.displacement <- filter(mtcars, disp == 
     min(disp))
smallest.engine.displacement
data("ChickWeight")
chick.subset <- filter(ChickWeight, Time < 3, weight > 53)
chick.subset

# filter out missing values -----------------------------------------------

data("airquality")
head(airquality,10) #before filter 
no.missing.ozone = filter(airquality, !is.na(Ozone))
head(no.missing.ozone,8) #after filter 


# filter rows with NAs anywhere in the dataset ----------------------------

airqual.no.NA.anywhere <- filter(airquality[1:10,],    
  complete.cases(airquality[1:10,]))
airqual.no.NA.anywhere


# filter by %in%    (contains) --------------------------------------------

data("iris")
table(iris$Species) #counts of species in the dataset
iris.two.species <- filter(iris,
Species %in% c("setosa", "virginica"))
table(iris.two.species$Species)
nrow(iris); nrow(iris.two.species)


# filter for ozone > 29 and include only 3 columns ------------------------

data("airquality")
airqual.3.columns <- filter(airquality, Ozone > 29)[,1:3]
head(airqual.3.columns)


# filter by tot frequency of value across all rows -------------------------

table(mtcars$gear)
more.frequent.no.of.gears <- mtcars %>%
  group_by(gear) %>%
  filter(n() > 10)  #
table(more.frequent.no.of.gears$gear)
more.frequent.no.of.gears.and.low.horsepower <- mtcars %>%
group_by(gear) %>%
  filter(n() > 10, hp < 105)
  table(more.frequent.no.of.gears.and.low.horsepower$gear)
  

# filter by column name "starts with" -------------------------------------

names(iris)  #show the column names
iris.display <- iris %>% dplyr::select(starts_with("S")) 
head(iris.display)  #use head to reduce number of rows output


# filter rows: columns meet criteria (filter_at) --------------------------


new.mtcars <- mtcars %>% filter_at(vars(cyl, hp),
   all_vars(. == max(.)))
new.mtcars
msleep <- ggplot2::msleep 
msleep
msleep.over.5 <- msleep %>%
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>%
  filter_at(vars(contains("sleep")), all_vars(.>5))
msleep.over.5

# arrange (in English "sort") ---------------------------------------------


msleep <- ggplot2::msleep 
msleep[,1:4] 
animal.name.sequence <- arrange(msleep, vore, order)
animal.name.sequence[,1:4]
animal.name.sequence.desc <- arrange(msleep, vore, desc(order))
head(animal.name.sequence.desc[,1:4])


# rename ------------------------------------------------------------------


names(iris) 
renamed.iris <- rename(iris, width.of.petals = Petal.Width,
various.plants.and.animals = Species)
names(renamed.iris) 


# mutate ------------------------------------------------------------------


data("ChickWeight")
ChickWeight[1:2,]  #first two rows
Chickweight.with.log <- mutate(ChickWeight,
log.of.weight = log10(weight))
Chickweight.with.log[1:2,] 


# mutate_all --------------------------------------------------------------


msleep <- ggplot2::msleep 
names(msleep)
msleep.with.square.roots <- mutate_all(msleep[,6:11],
  funs("square root" = sqrt( . )))
names(msleep.with.square.roots) 
msleep.with.square.roots


# mutate_at ---------------------------------------------------------------


data("Titanic")
Titanic <- as.data.frame(Titanic)
head(Titanic)
titanic.with.ranks <- mutate_at(Titanic, vars(Class,Age,Survived), funs(Rank = min_rank(desc(.))))
head(titanic.with.ranks)


# mutate_if ---------------------------------------------------------------


divide.by.10 <- function (a.number) (a.number / 10)
head(CO2)
new.df <- CO2 %>%
  mutate_if(is.numeric, divide.by.10)
head(new.df)
df <- data.frame(
alpha = c(22, 1, NA),
almond = c(0, 5, 10),
grape = c(0, 2, 2),
apple = c(NA, 5, 10))
df
df.fix.alpha <- df %>% mutate_if(is.numeric, coalesce, ... = 0)
df.fix.alpha


# string detect & T/F dup indicator ---------------------------------------


msleep <- ggplot2::msleep 
table(msleep$vore)
msleep.no.c.or.a <- filter(msleep, !str_detect(vore, 
   paste(c("c","a"), collapse = "|")))
table(msleep.no.c.or.a$vore)
msleep.with.dup.indicator <- mutate(msleep, duplicate.indicator = duplicated(conservation))
msleep.with.dup.indicator[1:6,]
msleep.with.dup.indicator <- mutate(msleep, 
  duplicate.indicator = duplicated(conservation))
msleep.with.dup.indicator[1:6,c(1,2,3,12)]
msleep.with.dup.indicator2 <- mutate(msleep, 
  duplicate.indicator =  duplicated(conservation, genus)) %>%
   arrange(conservation,genus)
msleep.with.dup.indicator2
fruit <- c("apple","pear","orange","grape", "orange","orange")
x <- c(1,2,4,9,4,6)
y <- c(22,3,4,55,15,9)
z <- c(3,1,4,10,12,8)
w <- c(2,2,2,4,5,6)
df <- data.frame(fruit,x,y,z,w)
df
df.show.single.dup <- mutate(df, duplicate.indicator = duplicated(fruit))
df.show.single.dup

# drop variables using NULL -----------------------------------------------


fruit <- c("apple","pear","orange","grape", "orange","orange")
x <- c(1,2,4,9,4,6)
y <- c(22,3,4,55,15,9)
z <- c(3,1,4,10,12,8)
df <- data.frame(fruit,x,y,z)
df <- mutate(df, z = NULL)
df
if (!require("nycflights13")) install.packages("nycflights13")   
library(nycflights13)  
mutate(flights,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours,
  gain_per_minute = 60 * gain_per_hour)
if (!require("nycflights13")) install.packages("nycflights13")   
newfield.flights <- flights %>%
  mutate(gain = arr_delay - dep_delay,
  hours = air_time / 60) %>%
  mutate(gain_per_hour = gain / hours) %>%
  mutate(gain_per_minute = 60 * gain_per_hour)
newfield.flights[1:6,c(1:2,20:23)] 


# transmute - keep only variables created ---------------------------------


fruit <- c("apple","pear","orange","grape", "orange","orange")
x <- c(1,2,4,9,4,6)
y <- c(22,3,4,55,15,9)
z <- c(3,1,4,10,12,8)
df <- data.frame(fruit,x,y,z)
df #before transmute
df <- transmute(df, new.variable = x + y + z)
df 


# Across to apply function over multiple cols -----------------------------

double.it <- function(x) x*2  
head(iris)
iris %>%
  mutate(across(where(is.numeric), double.it)) %>%
  head()  


# case_when  conditional mutating -----------------------------------------

row1 <- c("a","b","c","d","e","f","column.to.be.changed")
row2 <- c(1,1,1,6,6,1,2)
row3 <- c(3,4,4,6,4,4,4)
row4 <- c(4,6,25,5,5,2,9)
row5 <- c(5,3,6,3,3,6,2)
df <- as.data.frame(rbind(row2,row3,row4,row5))
names(df) <- row1
df 
new.df <- df %>%
  mutate(column.to.be.changed = case_when(a == 2 | a == 5 | 
  a == 7 | (a == 1 & b == 4) ~ 2, a == 0 | a == 1 | a == 4 |
  a == 3 | c == 4 ~ 3, TRUE ~ NA_real_))
new.df 


# select - choose variables/columns ---------------------------------------


# delete a column ---------------------------------------------------------

fruit <- c("apple","pear","orange","grape", "orange","orange")
x <- c(1,2,4,9,4,6)
y <- c(22,3,4,55,15,9)
z <- c(3,1,4,10,12,8)
df <- data.frame(fruit,x,y,z) #before select
df
new.df.no.fruit <- dplyr::select(df, -fruit)
new.df.no.fruit #after select


# del col by name, using starts_with or ends_with -------------------------

data("mtcars")
names(mtcars)
mtcars.no.col.names.start.with.d <- select(mtcars, -starts_with("d"))
names(mtcars.no.col.names.start.with.d)
mtcars.no.col.names.ends.with <- select(mtcars,
   - ends_with("t"))
names(mtcars.no.col.names.ends.with)


# rearrange column order --------------------------------------------------


fruit <- c("apple","pear","orange","grape", "orange","orange")
x <- c(1,2,4,9,4,6)
y <- c(22,3,4,55,15,9)
z <- c(3,1,4,10,12,8)
df <- data.frame(fruit,x,y,z)
df 


# select_all to apply a function to all cols ------------------------------


state <- c("Maryland", "Alaska", "New Jersey")
income <- c(76067,74444,73702)
median.us <- c(61372,61372,61372)
life.expectancy <- c(78.8,78.3,80.3)
top.3.states <- data.frame(state, income, median.us,  
  life.expectancy)
top.3.states #before - column names are not capitalized 
new.top.3.states <- select_all(top.3.states, toupper)
new.top.3.states #after function "toupper" applied 


# select columns using the pull function ----------------------------------

state <- c("Maryland", "Alaska", "New Jersey")
income <- c(76067,74444,73702)
median.us <- c(61372,61372,61372)
life.expectancy <- c(78.8,78.3,80.3)
top.3.states <- data.frame(state, income, median.us, life.expectancy)
top.3.states #display dataframe values 
pull.first.column <- pull(top.3.states,1) 
pull.first.column 
pull.last.column <- pull(top.3.states,-1) 
pull.last.column


# select rows - any variable meets some condition -------------------------

nrow(mtcars) 
mtcars.more.than.200 <- filter_all(mtcars, any_vars(. > 200))
nrow(mtcars.more.than.200)


# select cols - omit if col name contains specific chars ------------------

names(mtcars) 
cars.with.no.p <- mtcars %>% 
  dplyr::select(-contains("p"))  #note that "dplyr::" is used to ensure the
                                 #correct package is used for the select function
names(cars.with.no.p)

# select using wildcard matching ------------------------------------------

names(mtcars) 
subset.mtcars <- select(mtcars,
  matches("pg|gea")) 
names(subset.mtcars)


# left join (most common) -------------------------------------------------


us.state.areas <- as.data.frame(cbind(state.abb, state.area))
us.state.areas[1:3,]
us.state.abbreviation.and.name <- as.data.frame(cbind(state.abb, 
  state.name))
us.state.abbreviation.and.name[1:3,]
state.info.abb.area.name <- us.state.areas %>% 
  left_join(us.state.abbreviation.and.name, by = "state.abb")
head(state.info.abb.area.name)  


# inner join --------------------------------------------------------------

names <- c("Sally","Tom","Frieda","Alfonzo")
team.scores <- c(3,5,2,7)
team.league <- c("alpha","beta","gamma", "omicron")
team.info <- data.frame(names, team.scores, team.league)
names = c("Sally","Tom", "Bill", "Alfonzo")
school.grades <- c("A","B","C","B")
school.info <- data.frame(names, school.grades)
school.and.team <- inner_join(team.info, school.info, by = "names")
school.and.team


# anti-join ---------------------------------------------------------------

names <- c("Sally","Tom","Frieda","Alfonzo")
team.scores <- c(3,5,2,7)
team.league <- c("alpha","beta","gamma", "omicron")
team.info <- data.frame(names, team.scores, team.league)
team.info
names <- c("Sally","Tom", "Bill", "Alfonzo")
school.grades <- c("A","B","C","B")
school.info <- data.frame(names, school.grades)
school.info
team.info.but.no.grades <- anti_join(team.info, school.info,
   by = "names")
team.info.but.no.grades


# full join ---------------------------------------------------------------

names = c("Sally","Tom","Frieda","Alfonzo")
team.scores = c(3,5,2,7)
team.league = c("alpha","beta","gamma", "omicron")
team.info = data.frame(names, team.scores, team.league)
names = c("Sally","Tom", "Bill", "Alfonzo")
school.grades = c("A","B","C","B")
school.info = data.frame(names, school.grades)
team.info.and.or.grades <- full_join(team.info, school.info, by = "names")
team.info.and.or.grades


# semi-join ---------------------------------------------------------------


team.info.with.grades <- semi_join(team.info, school.info)
team.info.with.grades


# right join --------------------------------------------------------------

us.state.areas <- as.data.frame(cbind(state.abb, state.area))
us.state.areas[1:3,]
us.state.abbreviation.and.name <- as.data.frame(cbind(state.abb,
   state.name))
us.state.abbreviation.and.name[1:3,]
us.state.abbreviation.and.name[1,1] <- "Intentional Mismatch"
us.state.with.abbreviation.and.name.and.area <- right_join(us.state.areas,
     us.state.abbreviation.and.name, by = "state.abb")
us.state.with.abbreviation.and.name.and.area[1:3,]


# slice -------------------------------------------------------------------


msleep <- ggplot2::msleep
nrow(msleep) #initially 83 rows
msleep.only.first.5 <- slice(msleep, -6:-n())
nrow(msleep.only.first.5)
msleep.20.rows <- msleep %>%
   slice(20:39)
nrow(msleep.20.rows)
nrow(msleep) - nrow(msleep.20.rows)


# summarize ---------------------------------------------------------------

library(MASS) 
data(gehan)
gehan2 <- gehan  
library(tidyverse)
gehan2 %>% summarise( kount = n())
gehan2 %>%
  group_by(treat) %>%
  summarise(kount = n())
gehan2 %>%
  group_by(treat) %>%
  summarise(average.remiss.time = mean(time), 
     median.remiss.time = median(time),
     std.dev.remiss.time = sd(time),
     median.abs.deviation = mad(time),
      IQR.remiss.time = IQR(time))
gehan2 %>%
  group_by(treat) %>%
  summarise(minimum.remission = min(time),
        max.remission = max(time))


# summarize across --------------------------------------------------------

library(MASS)
subset.survey <- survey[1:10,]  
library(dplyr) 
head(subset.survey)
subset.survey %>%
    na.omit() %>%  #remove any NAs
    group_by(Sex) %>%
    summarize(across(where(is.numeric), mean,
    .names = "mean_{col}")) %>%
     head()  
new.sleep <- msleep %>%
  group_by(vore, order)
s <- summarise(new.sleep, n()) 
S
new.sleep.totals <- msleep %>%
  group_by(vore, order) %>%
  summarise(n())
new.sleep.totals


# gathering - convert multiple columns into one ---------------------------

state <- c("Maryland", "Alaska", "New Jersey")
income <- c(76067,74444,73702)
median.us <- c(61372,61372,61372)
life.expectancy <- c(78.8,78.3,80.3)
teen.birth.rate.2015 <- c(17,29.3,12.1)
teen.birth.rate.2007 <- c(34.3,42.9,24.9)
teen.birth.rate.1991 <- c(54.1, 66, 41.3)
top.3.states <- data.frame(state, income, median.us, 
  life.expectancy,
  teen.birth.rate.2015, teen.birth.rate.2007,
  teen.birth.rate.1991)
names(top.3.states) <- c("state", "income", "median.us",
   "life.expectancy","2015","2007","1991")
top.3.states 
new.top.3.states <- top.3.states %>%
  gather("2015", "2007", "1991", key = "year", value = "cases")
new.top.3.states


# spreading - consolidation of multiple rows into one ---------------------

df_1 <- data_frame(Type = c("TypeA", "TypeA", "TypeB", "TypeB"),
  Answer = c("Yes", "No", NA, "No"), n = 1:4)
df_1 #before
df_2 <- df_1 %>%
filter(!is.na(Answer)) %>%
    spread(key=Answer, value=n)
df_2 #after


# separate - divide single col into multiple cols -------------------------

state <- c("Maryland", "Alaska", "New Jersey")
income <- c(76067,74444,73702)
median.us <- c(61372,61372,61372)
life.expectancy <- c(78.8,78.3,80.3)
teen.birth <- c("17//34.3//54.1", "29.0//42.9//66.0", "12.1//24.9//41.3")
top.3.states <- data.frame(state, income, median.us,  
  life.expectancy,teen.birth)
top.3.states 
top.3.states.separated.years <- top.3.states %>%
    separate(teen.birth, 
    into = c("2015", "2007","1991"), sep = "//")
top.3.states.separated.years

# basic counts  -----------------------------------------------------------

m <- mutate(new.sleep, kount = n()) #new variable, kount added to extreme right
m[1:5,c(1:4,10:12)] #limit number of columns to fit on page
f <- filter(new.sleep, n() > 14)
f[1:5,c(1:4,10:11)]


# Nth functions -----------------------------------------------------------

salary.description <- c("Golden parachute type","Well to do",
      "Average", "Below average", "brings date seeds instead of flowers")
first(salary.description) 
last(salary.description) 
nth(salary.description, -3) 
nth(salary.description,2) 


# count distinct values ---------------------------------------------------

a.vector <- c(22,33,44,1,2,3,3,3,4) 
original.length <- length(a.vector) 
original.length
distinct.a.vector <- n_distinct(a.vector)
distinct.a.vector 
test1 <- if_else(original.length == distinct.a.vector, "all values unique","some duplicate values in vector")
test1
b.vector <- c(1,2,3,4,5,6)
length(b.vector)
distinct.b.vector <- n_distinct(b.vector)
distinct.b.vector #show count (length) of distinct numbers
test2 <- if_else(length(b.vector) == distinct.b.vector, "all values unique", "duplicates")
test2


# na_if -------------------------------------------------------------------

test <- c(100, 0, 999) 
x <- 5000/test 
x
x <- 5000/na_if(test,0) # if any zero occurs in test,
x 
class(x) #use class to show the type of variable


# use coalesce to replace missing values ----------------------------------

x <- c(33,4,11,NA,9)
x
x <- coalesce(x,0)
x

# ranking via index -------------------------------------------------------

y <- c(100,4,12,6,8,3)
rank1 <-row_number(y)
rank1
y[rank1[1]] #lowest rank; in this case, rank1[1] points to # y[6] which is 3 (lowest)
y[rank1[6]] #highest ranking number, in this case the first # number, 100



# minimum, dense, and percent rank ----------------------------------------


rank2 <- min_rank(y) #in this specific case (for y), gives same results as #row_number
rank2

rank3 <- dense_rank(y)
rank3

rank4 <- percent_rank(y) 
rank4


# cumulative distribution function ----------------------------------------

y <- c(100,4,12,6,8,3)
rank5 <- cume_dist(y)
rank5
rank6 = ntile(y, 3) #in this case, choose 3 buckets
rank6
test.vector <- c(2,22,33,44,77,89,99)
quantile(test.vector, prob = seq(0,1,length = 11),type = 5)


# sampling ----------------------------------------------------------------


data("ChickWeight")
my.sample <- sample_n(ChickWeight, 5) 
my.sample
set.seed(833)
my.sample <- sample_n(ChickWeight, 10, replace = TRUE)
my.sample
my.sample <- sample_n(mtcars, 12, weight = cyl)
my.sample[,1:5]
test1 <- sample_frac(ChickWeight, 0.02)
test1
by_hair_color <- starwars %>% group_by(hair_color)
my.sample <- sample_frac(by_hair_color, .07, replace = TRUE) 
my.sample[,1:5]
row.kount.only <- ChickWeight %>% tally() 
row.kount.only
diet.kount <- ChickWeight %>% count(Diet) 
diet.kount


# misc dplyr functions ----------------------------------------------------



# add_count for groupwise filtering ---------------------------------------

single.species.kount <- starwars %>%
  add_count(species) %>%
  filter(n == 1)
single.species.kount[,1:6]  #only species with a single member are included


# rename ------------------------------------------------------------------

mtcars <- rename(mtcars, spam_mpg = mpg)
data(mtcars)
names(mtcars)
mtcars <- rename(mtcars, spam_mpg = mpg)
names(mtcars)


# case_when ---------------------------------------------------------------

data(starwars)
new.starwars <- starwars %>% 
  dplyr::select(name, mass, gender, species, height) %>%
  mutate(type = case_when(height > 200 | mass > 200 ~ "large",
  species == "Droid" ~ "robot", TRUE ~ "other"))
new.starwars


# stringr functions -------------------------------------------------------



# find, count, extract ----------------------------------------------------


bk.fruit <- c("apQple", "banana", "pear", "pinQeapple")
bk.where.is.Q <-  str_locate(bk.fruit,"Q")
bk.where.is.Q
bk.fruit <- c("apQple", "bananaQ", "pear", "pinQeapple")
bk.where.is.Q <-  str_locate(bk.fruit,"Q")
bk.where.is.Q
class(bk.where.is.Q)

# string detect -----------------------------------------------------------

bk.pattern <- "a.b"
bk.strings <- c("abb", "a.b")
bk.detect.strings <- str_detect(bk.strings, bk.pattern)
bk.detect.strings
bk.detect.strings.fixed <- str_detect(bk.strings, fixed(bk.pattern))
bk.detect.strings.fixed
bk.detect.strings.collation.rules <-  str_detect(bk.strings, coll(bk.pattern)) 
bk.detect.strings.collation.rules
bk.i <- c("I", "\u0130", "i")
bk.i
bk.detect.fixed.i <- str_detect(bk.i, fixed("i", TRUE))
bk.detect.fixed.i
bk.detect.collation <- str_detect(bk.i, coll("i", TRUE))
bk.detect.collation
bk.detect.using.turkey <- str_detect(bk.i, coll("i", TRUE, locale = "tr"))
bk.detect.using.turkey

# string count ------------------------------------------------------------

# note:  a period matches any character except a new line
bk.fruit <- c("apple", "banana", "pear", "pineapple")
bk.fruit.count <- str_count(bk.fruit, "a")
bk.fruit.count
bk.count.with.p <-str_count(bk.fruit, "p")
bk.count.with.p
bk.count.with.e <- str_count(bk.fruit, "e")
bk.count.with.e
bk.count.multiple <- str_count(bk.fruit, c("a", "b", "p", "p"))
bk.count.multiple
bk.count <- str_count(c("a.", "...", ".a.a"), ".")
bk.count
bk.fixed <- str_count(c("a.", "...", ".a.a"), fixed("."))  
bk.fixed
fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")
str_detect("aecfg", letters)
str_detect(fruit, "^p", negate = TRUE)
fruit <- c("apple", "pear", "banana")
str_dup(fruit, 2)
str_dup(fruit, 1:3)
str_c("ba", str_dup("na", 0:5)) 


# string remove -----------------------------------------------------------


fruits <- c("one apple", "two pears", "three bananas")
str_remove(fruits, "[aeiou]")
bk.speech <- "While we are thus unconstrained in our private business, a spirit of reverence pervades our public acts"
bk.speech.words <- c("While", "we", "are", "thus", "unconstrained", "in", "our", "private", "business,", 
  "a", "spirit", "of", "reverence", "pervades", "our", "public", "acts")
str_remove_all(fruits, "[aeiou]")
bk.speech <- "While we are thus unconstrained in our private business, a spirit of reverence pervades our public acts"
bk.speech.words <- c("While", "we", "are", "thus", "unconstrained", "in", "our", "private", "business,", 
  "a", "spirit", "of", "reverence", "pervades", "our", "public", "acts")
bk.no.vowels <- str_remove_all(bk.speech, "[aeiou]")
bk.no.vowels


# string replace ----------------------------------------------------------

#ignore warning messages
bk.vowels.replaced <- str_replace(bk.speech, "[aeiou]", "zzz")
bk.vowels.replaced
bk.all.vowels.replaced <- str_replace_all(bk.speech, "[aeiou]", "zzz")
bk.all.vowels.replaced
bk.all.vowels.capitalized <- str_replace_all(bk.speech, "[aeiou]", toupper)
bk.all.vowels.capitalized
bk.in.replace.with.na <- str_replace_all(bk.speech.words, "in", NA_character_)
bk.in.replace.with.na
bk.dup.first.vowel <- str_replace(bk.speech.words, 
  "([aeiou])", "\\1\\1") 
bk.dup.first.vowel
bk.replace.vowel.w.number <- str_replace(bk.speech.words, "[aeiou]", c("7", "8", "9"))
bk.replace.vowel.w.number
bk.replace.ou <- str_replace(bk.speech.words, "ou", 
   c("7", "8", "9"))
bk.replace.ou
bk.make.one.altered.string <- bk.speech.words %>% 
  str_c(collapse = "$$$$") %>% 
  str_replace_all(c("while" = "____WHILE____", 
  "thus" = "____THEREFORE____"))
bk.make.one.altered.string


# string starts -----------------------------------------------------------

bk.starts.with.p <- str_starts(bk.speech.words, "p")
bk.starts.with.p  #logical output
bk.starts.with.no.p <- str_starts(bk.speech.words, 
  "p", negate = TRUE)
bk.starts.with.no.p


# string ends -------------------------------------------------------------

bk.ends.with <- str_ends(bk.speech.words, "s")
bk.ends.with
bk.ends.with.no.s <- str_ends(bk.speech.words, "s",
   negate = TRUE)   
bk.ends.with.no.s


# string subset -----------------------------------------------------------


bk.subset <- str_subset(bk.speech.words, "t")
bk.subset 
bk.subset.no.match <- str_subset(bk.speech.words,
   "un", negate = TRUE)
bk.subset.no.match
bk.which.match <- str_which(bk.speech.words, "sp") 
bk.which.match  
bk.starts.with.and.includes.entire.string <-
     str_subset(bk.speech.words,  "^a")
bk.starts.with.and.includes.entire.string
bk.ends.with.and.includes.entire.string <-  
   str_subset(bk.speech.words, "e$")
bk.ends.with.and.includes.entire.string 
bk.test.na <- str_subset(c("qqq", NA, "mmm", "9999"), ".") 
bk.test.na
bk.test.na <- str_which(c("qqq", NA, "mmm", "9999"), ".") 
bk.test.na


# string which ------------------------------------------------------------


bk.lorem <- c("Lorem ipsum dolor sit amet, consectetur  
 adipiscing elit, sed do eiusmod tempor incididunt ut labore et 
 dolore magna aliqua.", "Ut enim ad minim veniam, quis nostrud  
 exercitation ullamco laboris nisi ut aliquip ex ea commodo 
 consequat.", "Duis aute irure dolor in reprehenderit in  
 voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
 "Excepteur sint occaecat cupidatat non proident, sunt in culpa 
 qui officia deserunt mollit anim id est laborum.")
length(bk.lorem)
str_which(bk.lorem, "dolor")

# string extraction using regular expressions -----------------------------

bk.goddard.facts <- "He and his team launched 34 rockets between 1926 and 1941, achieving altitudes as high as 2.6 km (1.6 mi) and speeds as fast as 885 km/h (550 mph)."
bk.goddard.words <- c("He and his team launched ", "34 rockets ", "between   
  1926 and 1941", ", achieving altitudes as high as ", "2.6 km (1.6 mi) ", 
  "and speeds as fast as ", "885 km/h (550 mph)")
bk.goddard.facts.numbers <- str_extract(bk.goddard.facts, "\\d")
bk.goddard.facts.numbers  #first number encountered
bk.goddard.facts.numbers <- str_extract(bk.goddard.facts, 
  "\\d+")
bk.goddard.facts.numbers  #entire (all digits) first number encountered 
bk.goddard.all.alphas <- str_extract(bk.goddard.words, "[a-z]")
bk.goddard.all.alphas   
bk.goddard.all.alphas <- str_extract(bk.goddard.words, "[a-z]+")
bk.goddard.all.alphas  
z <- gregexpr("[0-9]+",bk.goddard.words) 
bk.goddard.all.numbers <- regmatches(bk.goddard.words,z)
bk.goddard.all.numbers 
class(bk.goddard.all.numbers)
bk.goddard.all.numbers <- as.data.frame(matrix(unlist(bk.goddard.all.numbers),
    nrow=length(bk.goddard.all.numbers),byrow=TRUE))
bk.goddard.all.numbers
bk.split.into.words <- str_split(bk.goddard.words, "\\W")
bk.split.into.words

# string extract all ------------------------------------------------------

bk.all.words.extraction <- str_extract_all("Though it be madness, yet there is method to it",
                                           boundary("word"))
bk.all.words.extraction #list
shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
str_extract(shopping_list, "\\d")
str_extract(shopping_list, "[a-z]+")
str_extract(shopping_list, "[a-z]{1,4}")
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")
str_extract_all(shopping_list, "[a-z]+")
str_extract_all(shopping_list, "\\b[a-z]+\\b")
str_extract_all(shopping_list, "\\d")
str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE)
str_extract_all(shopping_list, "\\d", simplify = TRUE)
str_extract_all("This is, suprisingly, a sentence.", boundary("word"))
letters
str_flatten(letters)
str_flatten(letters, "-")


# string glue -------------------------------------------------------------

name <- "Fred"
age <- 50
anniversary <- as.Date("1991-10-12")
str_glue(
  "My name is {name}, ",
  "my age next year is {age + 1}, ",
  "and my anniversary is {format(anniversary, '%A, %B %d, %Y')}."
)


str_glue("My name is {name}, not {{name}}.")

## My name is Fred, not {name}.

str_glue(
  "My name is {name}, ",
  "and my age next year is {age + 1}.",
  name = "Joe",
  age = 40
)
mtcars %>% str_glue_data("{rownames(.)} has {hp} hp")


# string order ------------------------------------------------------------

str_order(letters)
str_sort(letters)
str_order(letters, locale = "haw") 
str_sort(letters, locale = "haw")
bk.x <- c("100a10", "100a5", "2b", "2a")
bk.x
sort(bk.x)    
str_sort(bk.x, numeric = TRUE)


# get or modify string information ----------------------------------------

bk.words <- c("These are some words.")
bk.boundary.count <- str_count(bk.words, boundary("word"))
bk.boundary.count
bk.string.split.blank <- str_split(bk.words, " ")[[1]]
bk.string.split.blank
class(bk.string.split.blank)
bk.string.split.words <- str_split(bk.words, boundary("word"))[[1]]
bk.string.split.words
length(bk.string.split.words)
bk.length <- str_length(letters)
bk.length
length(bk.length)
str_length(NA)
bk.factor.length <- str_length(factor("abc"))
bk.factor.length
str_length(c("i", "like", "programming", NA))
u1 <- "\u00fc"         
u2 <- stringi::stri_trans_nfd(u1)
u1
u2
str_length(u1)
str_length(u2)
str_count(u1)
str_count(u2)
fruit <- c("apple", "banana", "pear", "pineapple")
str_locate(fruit, "$")
str_locate(fruit, "a")  
str_locate(fruit, "e")
str_locate(fruit, c("a", "b", "p", "p"))
bk.locate.a <-  str_locate_all(fruit, "a")
bk.locate.a  
class(bk.locate.a)
str_locate_all(fruit, "e")
str_locate_all(fruit, c("a", "b", "p", "p"))
str_locate_all(fruit, "")
strings <- c(" 219 733 8965", "329-293-8753 ", "banana",
 "595 794 7569", "387 287 6718", "apple", "233.398.9187 ", 
 "482 952 3315", "239 923 8115 and 842 566 4692",
 "Work: 579-499-7527", "$1000","Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_extract(strings, phone)
bk.string.match <-  str_match(strings, phone)
bk.string.match


# extract/match all -------------------------------------------------------

str_extract_all(strings, phone)
bk.x <- c("<a> <b>", "<a> <>", "<a>", "", NA)   
bk.x.wildcard <- str_match(bk.x, "<(.*?)> <(.*?)>") 
bk.x.wildcard
str_match("2PIN Yield: 0.9933,1,1,0.9933,1,1,1,1","2PIN Yield: (.+?),")[,1]
str_match("2PIN Yield: 0.9933,1,1,0.9933,1,1,1,1","2PIN Yield: (.+?),")[,2]
str_match_all(bk.x, "<(.*?)>")
str_extract(bk.x, "<.*?>")
str_extract_all(bk.x, "<.*?>")

# case functions ----------------------------------------------------------

bk.text <- "Athenian citizen"
bk.text.upper.case <- str_to_upper(bk.text)
bk.text.upper.case
bk.text.lower.case <- str_to_lower(bk.text)
bk.text.lower.case
bk.text.title <- str_to_title(bk.text)
bk.text.title
bk.default.english.locale <- str_to_upper(bk.text,
    locale = "en")
bk.default.english.locale
bk.using.not.English.locale <- str_to_upper(bk.text,
   locale = "ky-KG")
bk.using.not.English.locale


# geographically aware functions ------------------------------------------

bk.i <- c("I", "\u0130", "i")    
length(bk.i)
bk.i
str_detect(bk.i, fixed("i", TRUE))  
str_detect(bk.i, coll("i", TRUE))
str_detect(bk.i, coll("i", TRUE, locale = "tr"))  #now test using locale = "tr" (Turkish).  


# combine multiple strings ------------------------------------------------

bk.letters <- str_c("Letter: ", letters)
bk.letters.with.separator <- str_c("Letter", letters,
   sep = ": ")
bk.letters.with.separator
bk.combine.characters <- str_c(letters, " is for", "...")
bk.combine.characters
str_c(letters, " is for", "...")
bk.sequence <- str_c(letters[-26], " comes before ",
  letters[-1])
bk.sequence   
bk.collapsed.letters <- str_c(letters, collapse = "")
bk.collapsed.letters   
bk.comma.collapsed <- str_c(letters, collapse = ", ")
bk.comma.collapsed
bk.missing <- str_c(c("a", NA, "b"), "-d")
bk.missing 
bk.replace.na <- str_c(str_replace_na(c("a", NA, "b")), "-d")
bk.replace.na
bk.x <- rawToChar(as.raw(177))
bk.x #note: on a Mac, you may get a different code “\xb1”
bk.polish <- str_conv(bk.x, "ISO-8859-2") 
bk.polish  #on a Mac you will get a “q”
bk.plus.minus <- str_conv(bk.x, "ISO-8859-1") # Plus-minus
bk.plus.minus


# invert match ------------------------------------------------------------

bk.numbers <- "1 and 2 and 4 and 456"
bk.num.loc <- str_locate(bk.numbers, "[0-9]+")[[1]]
bk.num.loc   #output - integer
class(bk.num.loc)
bk.numbers <- "1 and 2 and 4 and 456"
bk.num.loc <- str_locate_all(bk.numbers, "[0-9]+")[[1]]
bk.num.loc
bk.numbers.as.they.occur.in.the.string <- str_sub(bk.numbers,
     bk.num.loc[, "start"], bk.num.loc[, "end"])
bk.numbers.as.they.occur.in.the.string
bk.text.but.no.numbers <- invert_match(bk.num.loc)
bk.show.text <- str_sub(bk.numbers,
   bk.text.but.no.numbers[, "start"],
   bk.text.but.no.numbers[, "end"])
bk.show.text
if (!require("htmlwidgets")) install.packages("htmlwidgets")   
bk.view1 <- str_view(c("abc", "def", "fgh"), "[aeiou]") 
bk.view1 
class(bk.view1)
print(bk.view1)
bk.view2 <- str_view(c("abc", "def", "fgh"), "^") 
print(bk.view2)
bk.view3 <- str_view(c("abc", "def", "fgh"), "..") 
print(bk.view3)  
bk.view4 <- str_view_all(c("abc", "def", "fgh"), "d|e")
print(bk.view4)
str_view(c("abc", "def", "fgh"), "d|e")
str_view(c("abc", "def", "fgh"), "d|e", match = TRUE)
str_view(c("abc", "def", "fgh"), "d|e", match = FALSE)
str_view(c("hair", "haematic"), "ha(i|e)") 


# word wrapping -----------------------------------------------------------


thanks_path <- file.path(R.home("doc"), "THANKS")
thanks <- str_c(readLines(thanks_path), collapse = "\n")
thanks <- word(thanks, 1, 3, fixed("\n\n"))
cat(str_wrap(thanks), "\n")
cat(str_wrap(thanks, width = 40), "\n")
cat(str_wrap(thanks, width = 60, indent = 2), "\n")
cat(str_wrap(thanks, width = 60, exdent = 2), "\n")
cat(str_wrap(thanks, width = 0, exdent = 2), "\n")
cat(str_wrap(thanks, width = 40), "\n")
cat(str_wrap(thanks, width = 60, indent = 2), "\n")
cat(str_wrap(thanks, width = 0, exdent = 2), "\n")


# string padding ----------------------------------------------------------

bk.row.bind <-  rbind(
  str_pad("hadley", 30, "left"),
  str_pad("hadley", 30, "right"),
  str_pad("hadley", 30, "both"))
bk.row.bind
str_pad(c("a", "abc", "abcdef"), 10)
str_pad("a", c(5, 10, 20))
str_pad("a", 10, pad = c("-", "_", " "))
str_pad("hadley", 3)


# str_trim  -- trim whitespace from a string ------------------------------


str_trim(" String with trailing and leading white space\t")  
str_trim("\n\nString with trailing and leading white space\n\n")  
str_squish(" String with trailing, middle, and leading white space\t")


# remove whitespace -------------------------------------------------------


str_squish("\n\nString with excess, trailing and leading white   space\n\n")


# truncate a string -------------------------------------------------------


bk.x <- "The stock market can remain irrational longer than you can remain solvent. Keynes"
bk.right.left.center <- rbind(
  str_trunc(bk.x, 10, "right"),
  str_trunc(bk.x, 10, "left"),
  str_trunc(bk.x, 10, "center"))
bk.right.left.center
nchar(bk.right.left.center)


# regular expressions with stringr ----------------------------------------


bk.string <- c("Hiphopopotamus", "Rhymenoceros", "time for bottomless lyrics")
bk.pattern <- "t.m"
bk.string.extract.first.match <- str_extract(bk.string,bk.pattern)
bk.string.extract.first.match


# reg ex variations -------------------------------------------------------


str_extract_all("The Cat in the Hat", "[a-z]+ ")
str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))
str_extract_all("a\nb\nc", "^.")
str_extract_all("a\nb\nc", regex("^.", multiline = TRUE))
str_extract_all("a\nb\nc", "a.")
str_extract_all("a\nb\nc", regex("a.", dotall = TRUE)) 
sentences[1:5] 
fruit[1: 5] 
length(words)
words[1:5]
head(sentences ,10)


# lubridate - date and time processing ------------------------------------


if (!require("lubridate")) install.packages("lubridate")   
bk.coffee.time <- as.POSIXct("080418 10:11", 
  format = "%y%m%d %H:%M")
am(bk.coffee.time)
bk.time.span <- interval(ymd("2019-01-01"), ymd("2019-06-30"))
bk.time.span.value <- as.duration(bk.time.span)
bk.time.span.value


# duration calculations ---------------------------------------------------


bk.how.long.in.hours.and.minutes <- duration(hours = 4,
   minutes = 10)
bk.how.long.in.hours.and.minutes
bk.hours.numeric <-  
  as.numeric(bk.how.long.in.hours.and.minutes,"hours")
bk.hours.numeric
bk.minutes.numeric <- 
  as.numeric(bk.how.long.in.hours.and.minutes,"minutes")
bk.minutes.numeric
bk.five.whole.days <- as.period(5, unit = "day") #five days
bk.five.whole.days


# spanning two dates using interval ---------------------------------------


bk.span.of.two.dates <- interval(ymd_hms("2009-01-01 00:00:00"), 
  ymd_hms("2010-02-02 01:01:01")) 
as.period(bk.span.of.two.dates)
bk.span.of.two.dates <- interval(ymd_hms("2009-01-01 00:00:00"), 
  ymd_hms("2010-01-01 00:00:00"))  
as.period(bk.span.of.two.dates, unit = "day")
bk.leap.year.span <- interval(ymd_hms("2016-01-01 00:00:00"), 
  ymd_hms("2017-01-01 01:00:00"))      
as.period(bk.leap.year.span, unit = "day")


# work with time zones ----------------------------------------------------


bk.chicago.time.interval <- interval(ymd("2016/11/06",
 tz = "America/Chicago"),ymd("2016/11/09"))
bk.hours <- as.period(bk.chicago.time.interval, unit = "hours")
bk.hours
bk.europe.belfast.time.interval <- interval(ymd("2016/11/06",
  tz = "Europe/Belfast"),ymd("2016/11/09"))
bk.hours <- as.period(bk.europe.belfast.time.interval,
  unit = "hours")
bk.hours
bk.start.date.time <- mdy_hm("8-11-2017 5:21",
  tz = "US/Eastern")
bk.end.date.time  <- mdy_hm("8-12-2018 6:21", 
  tz = "US/Eastern")
bk.interval <- bk.start.date.time %--% bk.end.date.time 
bk.interval
class(bk.interval) 


# calculate seconds between two date/times --------------------------------

bk.duration <- as.duration(bk.interval)  
bk.duration
bk.prussia.started <- ymd("1701-01-18")  
bk.prussia.ended <- ymd("1947-02-25")  
bk.interval <- bk.prussia.started %--% bk.prussia.ended
bk.prussia.in.existence <- as.duration(bk.interval)
bk.prussia.in.existence
bk.seventeen.seconds <- as.duration(17) 
bk.seventeen.seconds
as.numeric(bk.seventeen.seconds, "hours")
as.numeric(bk.seventeen.seconds, "minutes")
bk.hours <- 25
bk.minutes <- 8
bk.twenty.five.hours <- duration(hours = bk.hours)  
bk.twenty.five.hours
bk.twenty.five.hours.and.eight.minutes <- 
   duration(hours = bk.hours, minutes = bk.minutes)
bk.twenty.five.hours.and.eight.minutes
span <- interval(ymd("2009-01-01"), ymd("2009-08-01"))     
bk.time.span <- interval(ymd("2019-01-01"), ymd("2019-06-30"))
bk.time.span.value <- as.duration(bk.time.span) 
bk.time.span.value
dur <- duration(hours = 10, minutes = 6)
as.numeric(dur, "hours")
as.numeric(dur, "minutes")


# more interval calculations ----------------------------------------------


interval(ymd(20090201), ymd(20090101)) 
bk.date1 <- ymd_hms("2009-03-08 01:59:59")      
bk.date2 <- ymd_hms("2000-02-29 12:00:00")      
bk.interval <- interval(bk.date2, bk.date1)     
bk.interval
bk.interval.alternate.method <- 
  lubridate::`%--%`(lubridate::ymd("2009-03-08"),
  lubridate::ymd("2000-02-29")) 
bk.interval.alternate.method
# Switch the beginning and end dates:
bk.interval.negative <-interval(bk.date1, bk.date2)  
bk.interval.negative
bk.span <- interval(ymd(20090101), ymd(20090201))
bk.span
bk.interval.length.in.seconds <- int_length(bk.span)
bk.interval.length.in.seconds
bk.comparison <- 31*24*60*60
bk.comparison
bk.start <- int_start(bk.span)
bk.start
bk.stop <- int_end(bk.span)
bk.stop
bk.span.flipped <- int_flip(bk.span)
bk.span.flipped


# interval overlaps -------------------------------------------------------

bk.date1.second.range <- ymd_hms("2010-06-08 01:59:59")      
bk.date2.second.range <- ymd_hms("2010-04-29 12:00:00")       
bk.another.span <- interval(bk.date1.second.range, 
  bk.date2.second.range)
bk.another.span
bk.overlap.or.not <- int_overlaps(bk.another.span,bk.span)
bk.overlap.or.not
bk.date1.second.range <- ymd_hms("2009-06-08 01:59:59")
bk.date2.second.range <- ymd_hms("2008-04-29 12:00:00")       
bk.another.span <- interval(bk.date1.second.range, 
  bk.date2.second.range)
bk.another.span
bk.overlap.or.not <- int_overlaps(bk.another.span,bk.span)
bk.overlap.or.not


# interval shift ----------------------------------------------------------


int <- interval(ymd("2001-01-01"), ymd("2002-01-01"))
int_shift(int, duration(days = 11))
int_shift(int, duration(hours = -1))
bk.interval <- interval(ymd("2001-01-01"), ymd("2002-11-11"))
bk.interval
bk.interval.flip <- int_flip(bk.interval)
bk.interval.flip


# alignment ---------------------------------------------------------------


bk.date1 <- ymd_hms("2009-06-08 01:59:59")  #end date
bk.date2 <- ymd_hms("2008-04-29 12:00:00")  #beginning date
bk.span1 <- interval(bk.date1,bk.date2)
bk.date3 <- ymd_hms("2011-06-08 01:59:59")  #end date
bk.date4 <- ymd_hms("2008-04-29 12:00:00")  #beginning date
bk.span2 <- interval(bk.date1,bk.date2)
bk.aligns.true.or.false <- int_aligns(bk.span1, bk.span2)
bk.aligns.true.or.false
bk.multiple.dates <- bk.date4 + days(1:5)
bk.multiple.dates
bk.int.diff <- int_diff(bk.multiple.dates)
bk.int.diff


# periods -----------------------------------------------------------------


bk.ninety.seconds.and.five.minutes <- period(c(90, 5), 
    c("second", "minute"))
bk.ninety.seconds.and.five.minutes   
class(bk.ninety.seconds.and.five.minutes)
bk.ninety.seconds.and.five.minutes
period(-1, "days")
period(c(3, 1, 2, 13, 1), 
  c("second", "minute", "hour", "day", "week"))
period(c(1, -60), c("hour", "minute"))
period(0, "second")


# sequencing --------------------------------------------------------------


bk.sequence.start <- ymd(190102)   #January 2, 2019
bk.next.12.months <- bk.sequence.start + months(1:12)
bk.next.12.months
length(bk.next.12.months)
start1 <- mdy("06/01/20")
end1 <- mdy("06/10/20")
end2 <- mdy("09/30/25")
x <- seq(start1, end1,"days")
x
y <- seq(start1, by = "month", length.out = 24)
y
q <- seq(start1, end2,by = "quarter")
q
period("2M 1sec")
period("2hours 2minutes 1second")
period("2d 2H 2M 2S")
period("2days 2hours 2mins 2secs")
duration("day day")


# period versus duration --------------------------------------------------


bk.boundary <- ymd_hms("2009-03-08 01:59:59", 
  tz="America/Chicago")
bk.boundary
bk.boundary2 <- bk.boundary + days(1) # period
bk.boundary2
bk.boundary3 <- bk.boundary + ddays(1) # duration
bk.boundary3
bk.true.false <- is.period(as.Date("2009-08-03"))
bk.true.false
bk.period.from.seconds <- seconds_to_period(172800)   
bk.period.from.seconds
bk.period.to.seconds <- period_to_seconds(years(1) + months(1))  
bk.period.to.seconds
bk.period.to.seconds.in.months <- bk.period.to.seconds / 
   (60*60*24*30.4375)
bk.period.to.seconds.in.months


# timespan ----------------------------------------------------------------


bk.duration <-duration(3690, "seconds")
bk.duration
bk.period <- period(3690, "seconds")
bk.period
bk.define.second.minute.hour <- period(second = 30,
   minute = 1, hour = 1)
bk.define.second.minute.hour
bk.interval <- interval(ymd_hms("2009-08-09 13:01:30"), 
   ymd_hms("2009-08-09 12:00:00")) 
bk.interval
date1 <- ymd_hms("2009-03-08 01:59:59")
date2 <- ymd_hms("2000-02-29 12:00:00")
interval(date2, date1)   
interval(date1, date2)
bk.date <- ymd_hms("2009-03-08 01:59:59") 
bk.one.more.day <- bk.date + days(1)
bk.date
bk.one.more.day
bk.date2 <- ymd_hms("2000-02-29 12:00:00")
bk.one.year.later <- bk.date2 + years(1)
bk.one.year.later
bk.date2 <- ymd_hms("2000-02-29 12:00:00")
bk.one.year.later <- bk.date2 + dyears(1)
bk.one.year.later
bk.date3 <- ymd_hms("2009-01-31 01:00:00")
bk.date3 + c(0:11) * months(1)
bk.span <- bk.date2 %--% bk.date 
bk.span
bk.date4 <- ymd_hms("2009-01-01 00:00:00")
bk.date5  <-bk.date4 + years(1)
bk.date5
bk.date7  <- ymd_hms("2009-03-08 01:59:59") # DST boundary
bk.date8 <- bk.date7 - days(3) + hours(6)
bk.date8
bk.date <- ymd_hms("2009-03-08 01:59:59") 
bk.date9 <- bk.date + 3 * seconds(10)
bk.date  #start
bk.date9 #finish
bk.date10 <- months(6) + days(1) 
bk.date10
date.in.future.6mo.1day <- ymd("2021-01-22") + bk.date10
date.in.future.6mo.1day
bk.int <- interval(ymd("1980-01-01"), ymd("2014-09-18"))
bk.time.length.in.weeks <- time_length(bk.int, "week")
bk.time.length.in.weeks
bk.year.age.at.last.whole.year <- time_length(bk.int, "year")
bk.year.age.at.last.whole.year
bk.truncated.time.length <- trunc(time_length(bk.int, "year"))
bk.truncated.time.length


# contrast of intervals and durations -------------------------------------


bk.int <- interval(ymd("1900-01-01"), ymd("1999-12-31"))
bk.int
bk.time.interval.length <-  time_length(bk.int, "year")
bk.time.interval.length
bk.time.interval.as.year <- time_length(as.duration(bk.int), 
  "year")
bk.time.interval.as.year
bk.date <- as_date(44)  
bk.date
bk.date <- as_date(21, origin = lubridate::ymd("2019-02-01"))   
bk.date
bk.date <- as_datetime("2019-01-02")  
bk.date
class(bk.date)
bk.date.only.number.input <- as_datetime(20190102) 
bk.date.only.number.input


# coordinated universal time zone (UTC) -----------------------------------


bk.date_utc <- ymd_hms("2010-08-03 00:50:50")  
bk.date_europe <- ymd_hms("2010-08-03 00:50:50", 
   tz="Europe/London")
bk.date_europe

# as_date versus as.Date --------------------------------------------------

bk.as_date_versus_as.Date <- c(as_date(bk.date_utc), 
   as.Date(bk.date_utc))
bk.as_date_versus_as.Date
bk.as_date_versus_as.Date <- c(as_date(bk.date_europe), as.Date(bk.date_europe))
bk.as_date_versus_as.Date


# create date/time via hard coding ----------------------------------------


bk.date.and.time <- ymd_hms("2010-12-03 00:56:50")
bk.date.and.time
class(bk.date.and.time)
bk.date.only <- date(bk.date.and.time)
bk.date.only
bk.date.only.with.time.zone <- as.Date(bk.date.and.time, 
  tz = "America/New_York")
bk.date.only.with.time.zone


# revise date by individually changing mon, day, yr -----------------------


bk.date <- ymd("2018-12-31")
bk.date.revised <- update(bk.date, month = 01, 
  mday = 01, year = 2019) 
bk.date.revised


# fractional year ---------------------------------------------------------


bk.date <- ymd("2017-06-30")  
bk.decimal <- decimal_date (bk.date)
bk.decimal
bk.show.difference <- bk.decimal - 2017
bk.show.difference
bk.date.again <- date_decimal(bk.decimal)
bk.date.again  
class(bk.date.again)


# work day ----------------------------------------------------------------


bk.date <- as.Date("2019-01-15")
bk.working.day <- wday(bk.date)
bk.working.day


# is daylight savings time in effect? -------------------------------------


bk.date.time <- ymd("2019-03-10",tz = "America/New_York")   
bk.daylight.savings.time <- dst(bk.date.time) 
bk.daylight.savings.time   


# guess formats -----------------------------------------------------------


bk.dates.in.different.formats <- sampleDates <- 
  c("4/6/2004","4/6/2004","4/6/2004","4/7/2004", 
  "4/6/2004","4/7/2004","2014-06-28","2014-06-30","2014-07-12",
  "2014-07-29","2014-07-29","2014-08-12")
bk.simple.parsed.dates <- 
  parse_date_time(bk.dates.in.different.formats,
  c("Ymd", "mdY"))
bk.simple.parsed.dates
bk.simple.parsed.dates
length(bk.simple.parsed.dates)
class(bk.simple.parsed.dates)


# hour function -----------------------------------------------------------


bk.x <- ymd_hms(c("2009-08-07 01:01:01"))
bk.x
bk.hour <- hour(bk.x)
bk.hour
hour(bk.x) <- 5
bk.x
hour(bk.x) <- 25
bk.x
hour(bk.x) > 2


# extract names from date -------------------------------------------------


bk.date <- ymd("2019-01-15")
month(bk.date, label = TRUE)  
the.months = 1:12
names(the.months) = month.abb    
the.months[month(bk.date)]
month(bk.date, label = TRUE, abbr = FALSE)   
month(bk.date + months(0:5), label = F)  
month(bk.date + months(0:5), label = T)  


# parse periods with hr, min, and second components -----------------------

ms(c("09:10", "09:02", "1:10"))
bk.ms.example <-  ms(c("09:10", "09:02", "1:10"))
bk.ms.example
length(bk.ms.example)   
bk.ms.example
bk.ms.example <-  hm(c("09:10", "09:02", "11:01"))
bk.ms.example
# hms assumes hours, minutes, and seconds:
hms("7 6 5")
hms("7 65 5", roll = TRUE)


# parse date time: lubridate workhorse ------------------------------------


bk.two.dates <- parse_date_time(c("20-01-19", "20 January 2019"), orders = c("d m y", "d B Y"))
bk.two.dates
class(bk.two.dates)
length(bk.two.dates)      
bk.two.dates
bk.date <- parse_date_time("21-02-19 20-13-06",
   orders = "dmy HMS")
bk.date
bk.date <- mdy("09/5/2016")
bk.date
bk.date2 <- "2015-11-7 10:11:05"  
bk.date3 <- ymd_hms(bk.date2)  
bk.date3


# date validation ---------------------------------------------------------


bk.is.this.a.date <- is.Date(as.Date("2009-08-03"))
bk.is.this.a.date
bk.is.this.a.date <-  is.Date(9999999)
bk.is.this.a.date


# calculate time difference -----------------------------------------------


bk.date1 <- ymd_hms("2009-06-08 01:59:59") #end date
bk.date2 <- ymd_hms("2008-04-29 12:00:00") #beginning date
bk.diff.in.dates <- difftime(bk.date1,bk.date2)
bk.diff.in.dates
bk.is.difftime <- is.Date(bk.diff.in.dates)
bk.is.difftime
bk.is.a.difftime <- is.difftime(bk.diff.in.dates)
bk.is.a.difftime
bk.posix.query <- is.POSIXt(now())
bk.posix.query
is.timespan(as.Date("2009-08-03"))
is.timespan(duration(second = 1))
bk.not.leap.year <- as.Date("2019-01-01")
bk.query.leap.year <- leap_year(bk.not.leap.year)
bk.query.leap.year


# time zones --------------------------------------------------------------


my.time.zones <- OlsonNames()
length(my.time.zones)
my.sample.of.time.zones <- sample_n(as.data.frame(my.time.zones), 25) 
my.sample.of.time.zones  #note: since this is a sample, your 
                        #output may look different from mine


# shorthand to designate dates/times --------------------------------------


bk.easy.time1 <- make_datetime(year = 1999, month = 12,
   day = 22, sec = 10)
bk.easy.time1
bk.easy.time2 <- make_datetime(year = 1999, month = 12, 
  day = 22, sec = c(10, 11))
bk.easy.time2

# work with weeks ---------------------------------------------------------

bk.x <- ymd("2012-03-26")
week(bk.x)
bk.week <- week(bk.x) <- 1
bk.week
bk.week.set <- week(bk.x) <- 54
bk.week.set
bk.week.logical.question <- week(bk.x) > 3
bk.week.logical.question


# test interval or date - within another interval? ------------------------


bk.int <- interval(ymd("2001-01-01"), ymd("2002-01-01"))
bk.int
bk.int2 <- interval(ymd("2001-06-01"), ymd("2002-01-01"))
bk.int2
bk.within.or.without <- ymd("2001-05-03") %within% bk.int 
bk.within.or.without
bk.within.or.without.2 <- bk.int2 %within% bk.int 
bk.within.or.without.2
bk.within.or.without.3 <- ymd("1999-01-01") %within% bk.int   #is 1/1/1999 within 1/1/2001-1/1/2002
bk.within.or.without.3
bk.dates <- ymd(c("2014-12-20", "2014-12-30", "2015-01-01",
   "2015-01-03"))
bk.blackouts<- c(interval(ymd("2014-12-30"), ymd("2014-12-31")),
   interval(ymd("2014-12-30"), ymd("2015-01-03")))
bk.blackouts
bk.are.dates.within.blackouts <-  bk.dates %within% bk.blackouts
bk.are.dates.within.blackouts
bk.dates <- ymd(c("2014-12-20", "2014-12-30", 
  "2015-01-01", "2015-01-03"))
bk.blackouts<- list(interval(ymd("2014-12-30"),
  ymd("2014-12-31")), interval(ymd("2014-12-30"),
  ymd("2015-01-03")))
bk.are.dates.within.blackouts <- bk.dates %within% bk.blackouts
bk.are.dates.within.blackouts   


# misc functions - create a specified time difference ---------------------


make_difftime(1)
bk.difftime1sec <- make_difftime(1)
bk.difftime1sec
make_difftime(60)
make_difftime(3600)
make_difftime(3600, units = "minute")
make_difftime(second = 90)
make_difftime(minute = 1.5)
make_difftime(second = 3, minute = 1.5, hour = 2, day = 6,
   week = 1)
make_difftime(hour = 1, minute = -60)
make_difftime(day = -1)
make_difftime(120, day = -1, units = "minute")
bk.date <- (ymd("2019-01-12"))
minute(bk.date)
minute(ymd_hms("2009-06-08 00:59:59"))
minute(bk.date)  <- 5
minute(bk.date)
minute(bk.date) > 3   #logical test
bk.date  #show date
month(bk.date)


# force date/time to be in a different time zone --------------------------


bk.date <- ymd_hms("2019-01-07 00:00:01", tz = "Japan")
bk.date
bk.date2 <- force_tz(bk.date, "America/Chicago")
bk.date2
bk.date <- ymd_hms("2010-03-14 02:05:05 UTC")
bk.date
bk.date.with.no.roll.to.valid.time <- force_tz(bk.date, "America/New_York", roll=FALSE)
bk.date.with.no.roll.to.valid.time
bk.date.with.roll.to.valid.time <- force_tz(bk.date, 
  "America/New_York", roll=TRUE)
bk.date.with.roll.to.valid.time


# work with diff time zones in same calculation ---------------------------


bk.date <- ymd_hms(c("2009-08-07 00:00:01", 
  "2009-08-07 01:02:03"))
bk.date
two.dates <- c("2009-08-07 00:00:01", "2009-08-07 01:02:03")
bk.date <- ymd_hms(two.dates)
bk.date
bk.assign.time.zones <- force_tzs(bk.date, tzones = 
  c("America/New_York", "Europe/Amsterdam")) 
bk.assign.time.zones

bk.date.values <- ymd_hms(c("2009-08-07 00:00:01",
   "2009-08-07 01:02:03","2009-03-03 11:02:03",
   "2009-04-01 05:02:03" ))  
bk.assign.time.zones.with.tzone_out <-force_tzs(bk.date.values, 
  tzones = c("America/New_York", "Europe/Amsterdam", "Japan", 
  "libya"), tzone_out = "America/New_York")
bk.assign.time.zones.with.tzone_out


# eastern daylight savings and other time examples ------------------------


bk.x <- ymd_hms(c("2009-08-07 00:00:01", "2009-08-07 01:02:03"))   
bk.y <- force_tzs(bk.x, tzones = c("America/New_York", 
  "Europe/Amsterdam"))
bk.y
bk.x <- ymd("2012-03-26")
tz(bk.x)
tz(bk.x) <- "GMT"
bk.x
tz(bk.x) <- "America/New_York"
bk.x
bk.x <- ymd_hms("2009-08-07 00:00:01", tz = "America/New_York")
bk.with.time.zone <- with_tz(bk.x, "GMT")
bk.with.time.zone
bk.x.singapore <- ymd_hms("2009-08-07 00:00:01",
   tz = "Singapore")
bk.x.singapore
bk.with.time.zone <- with_tz(bk.x.singapore, "GMT")
bk.with.time.zone
bk.date <- ymd("2012-03-26")
bk.year <- year(bk.date)
bk.year
bk.year.2001 <-  year(bk.date) <- 2001   
bk.year.2001
bk.year.1995 <- year(bk.date) > 1995
bk.year.1995


# internationalization ----------------------------------------------------


# Sys.getlocale("LC_TIME")  #your response may vary slightly
bk.date1 <-  "Ma 2012 august 14 11:28:30 "
bk.german.date <- ymd_hms(bk.date1, locale = "German")
bk.german.date 
bk.date.new <-  "Ma 2012 august 14 11:28:30 "
bk.texas.date <- ymd_hms(bk.date.new, 
  locale = "English_United States.1252")
bk.texas.date 


# Is there any good locale news? ------------------------------------------

bk.date1 <-  "Ma 2012 august 14 11:28:30 "
# ymd_hms(bk.date1, locale = "LANG_LANG_BELARUSIAN")  
ymd_hms(bk.date1, 
  locale = "BELARUSIAN") # as modified by guessing 


# now, rollback, and rounding ---------------------------------------------


bk.current.date.time <- now()
bk.current.date.time
bk.current.date.time <- now(tzone = "Pacific/Midway")
bk.current.date.time
origin
bk.today <- today()
bk.today
bk.today.greenwich.time <- today("GMT")
bk.today.greenwich.time
bk.today.have.not.had.dinner.on.mars.yet <- 
  as.Date("2035-01-01")
bk.true.or.false <- today() <
   bk.today.have.not.had.dinner.on.mars.yet
bk.true.or.false


# rollback ----------------------------------------------------------------

bk.date <- ymd("2019-05-20")
bk.rolled.back <- rollback(bk.date)
bk.rolled.back
bk.dates <- bk.date + months(0:2)
bk.dates
bk.multiple.dates <- rollback(bk.dates)
bk.multiple.dates
bk.date <- ymd_hms("2010-03-03 12:44:22")
bk.date.rolled.back <- rollback(bk.date)
bk.date.rolled.back
bk.roll.to.first <- rollback(bk.date, roll_to_first = TRUE)
bk.roll.to.first
bk.roll.back.drop.hours.minutes.seconds <- rollback(bk.date, preserve_hms = FALSE)
bk.roll.back.drop.hours.minutes.seconds
bk.roll.back.no.keep.hours.minutes.seconds <- rollback(bk.date, 
  roll_to_first = TRUE, preserve_hms = FALSE)
bk.roll.back.no.keep.hours.minutes.seconds


# rounding ----------------------------------------------------------------


date <- as.POSIXct("2009-03-08 01:59:59") # DST boundary
options(digits.secs=6)
x <- ymd_hms("2009-08-03 12:01:59.23")
round_date(x, ".5s")
round_date(x, "sec")
round_date(x, "second")
round_date(x, "minute")
round_date(x, "5 mins")
round_date(x, "hour")
round_date(x, "2 hours")
round_date(x, "day")
round_date(x, "week")
round_date(x, "month")
round_date(x, "bimonth")
round_date(x, "quarter") == round_date(x, "3 months")
round_date(x, "halfyear")
round_date(x, "year")
x <- ymd_hms("2009-08-03 12:01:59.23")
floor_date(x, ".1s")
floor_date(x, "second")
floor_date(x, "minute")
floor_date(x, "hour")
floor_date(x, "day")
floor_date(x, "week")
floor_date(x, "month")
floor_date(x, "bimonth")
floor_date(x, "quarter")
floor_date(x, "season")
floor_date(x, "halfyear")
floor_date(x, "year")
x <- ymd_hms("2009-08-03 12:01:59.23")
ceiling_date(x, ".1 sec") 
ceiling_date(x, "second")
ceiling_date(x, "minute")
ceiling_date(x, "5 mins")
ceiling_date(x, "hour")
ceiling_date(x, "day")
ceiling_date(x, "week")
ceiling_date(x, "month")
ceiling_date(x, "bimonth") == ceiling_date(x, "2 months")
ceiling_date(x, "quarter")
ceiling_date(x, "season")
ceiling_date(x, "halfyear")
ceiling_date(x, "year")
x <- ymd("2000-01-01")
ceiling_date(x, "month")
ceiling_date(x, "month", change_on_boundary = TRUE)
bk.date <- ymd("2012-03-26")
second(bk.date)
second(bk.date) <- 1   
bk.date
second(bk.date) <- 61
bk.date
second(bk.date) > 2  
bk.x <- c("2011-12-31 12:59:59", "2010-01-01 12:11",
   "2010-01-01 12", "2010-01-01")
bk.x.truncated <- ymd_hms(bk.x, truncated = 3)
bk.x.truncated
bk.x <- c("2011-12-31 12:59", "2010-01-01 12", 
  "2010-01-01")
bk.x.truncated.2 <- ymd_hm(bk.x, truncated = 2)
bk.x.truncated.2


# automatic roll over arithmetic ------------------------------------------


bk.jan <- ymd_hms("2010-01-31 03:04:05")
bk.three.months.out <- bk.jan + months(1:3) 
bk.three.months.out  
bk.jan.do.the.right.thing <- bk.jan %m+% months(1:3) 
bk.jan.do.the.right.thing   
leap <- ymd("2012-02-29")
leap %m+% years(1)  #jumps to last day in February
leap %m+% years(-1) #same date, previous year
leap %m-% years(1)  # the minus sign means it goes back a year


# REGEX REGEX REGEX REGEX REGEX -------------------------------------------


# test for a match ---------------------------------------------------------

x <- c("apple", "banana", "pear")
str_extract(x, "an")
bananas <- c("banana", "Banana", "BANANA") 
str_detect(bananas, "banana")
str_detect(bananas, regex("banana", ignore_case = TRUE))
str_extract(x, ".a.")
str_detect("\nX\n", ".X.")
str_detect("\nX\n", regex(".X.", dotall = TRUE))


# validation (e.g., passwords) --------------------------------------------

is.validpw <- function(x) {
  grepl('(?x)                 # free-spacing mode
          ^                   # assert position = beginning of string
           (?=.*[[:upper:]])  # match one uppercase letter
           (?=.*[[:lower:]])  # match one lowercase letter
           (?=.*[[:punct:]])  # match one special character
           [ -~]{8,32}        # match printable ascii characters
          $                   # assert position = end of string', x, perl = TRUE)
}

is.validpw(c('#Password', '_PASSWORD', 'Password', 'password', 
             'passwor', 'pAsswords', 'Pa&sword', 'Pa&s word'))



# find all numbers and periods in a string --------------------------------

test <- c("desk#82", "chair weight $33.20")
my.regex <- "[^0-9\\.]"
gsub(my.regex, "", test)


# change characters -------------------------------------------------------


sub("(a+)", "z\\1z", c("abc", "def", "cba a", "aa"), perl=TRUE)
gsub("(a+)", "z\\1z", c("abc", "def", "cba a", "aa"), perl=TRUE)


# format strings ----------------------------------------------------------

string1 <- "234.456.100"
gsub("\\.", ",", string1)


# email -------------------------------------------------------------------


my.regex <- "^\\w+[\\w-\\.]*\\@\\w+((-\\w+)|(\\w*))\\.[a-z]{2,3}$"
my.emails <- c("john.smith@anything.com","___fred@x.com","123Sally@Y.net","not.an.email")
str_match (my.emails, my.regex)


# AMEX card number validation (syntax) ------------------------------------

test.amex.good <- "348282246310005"
test.amex.bad <-     "381449635398431"  
grepl("^3[47][0-9]{13}$",test.amex.good)
grepl("^3[47][0-9]{13}$",test.amex.bad)


# another email -----------------------------------------------------------


# [47] = the character string must include either a 4 OR a 7  (not a range)
# [0-9] = includes all single digit numbers
# $ = end of the string.
pattern1 <- "^[[:alnum:].-_]+@[[:alnum:].-]+$"
test.good.email <-  "byarberry@iccmconsulting.net"
test.bad.email  <-  "mr-got-in-a-hurry@@bargain.com"  
grepl(pattern1,test.good.email)
grepl("pattern1",test.bad.email)


# ip4 address -------------------------------------------------------------


pattern1 <- "\\b(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\b"
test.ip4.address.good <- "172.16.254.1"
test.ip4.address.bad <- "1723.16.254.2"
grepl(pattern1, test.ip4.address.good)
grepl(pattern1, test.ip4.address.bad)


# United States social security number validation -------------------------


pattern1 = "\\b(?!000)(?!666)[0-8][0-9]{2}[- ](?!00)[0-9]{2}[- 
  ](?!0000)[0-9]{4}\\b"
test.ssn.good <- "419-75-4971"
test.ssn.bad <- "418-833-4971"
grepl(pattern1, test.ssn.good, perl = TRUE)
grepl(pattern1, test.ssn.bad, perl = TRUE)


# remove path information from string, show only file name ----------------

file.name <- "C:\budget\\second.quarter.xlsx"
y <- gsub(pattern = "C:\budget",replacement = "", x = file.name)
z <- sub(pattern = "^.{1}",replacement = "",x = y)
z


# remove non-digits -------------------------------------------------------


x <- "The corona virus is exceedingly worrisome 3.14159"
my.regex <- "[^0-9]+"   
gsub(my.regex, "",x)  


# extract file type from URL ----------------------------------------------


my.URL <- "http://www.retro51.com/_notes/Disney_Booklet_sm.pdf"
tools::file_ext(sub("\\?.+", "", my.URL))


# replace string with same, adjacent letters ------------------------------

x <- c("appleeee","corn","ccccheze", "food")
gsub("\\s*\\b(?=\\w*(\\w)\\1)\\w+\\b", x, replacement = " ", perl = TRUE)


# find adjacent dups but not repeats of 3 or more -------------------------

x <- "cat cat dog frog hog horse horse horse cat"
my.regex <- "\\b(\\w+)\\s+\\1\\b"
gsub(my.regex,x,replacement = " ", perl = TRUE)


# extract lowest level subdirectory from file path ------------------------

x <- "C:/temp/budget/spreadsheet.xlsx"
gsub(".*/(.*)/[^/]+$","\\1",x)
x <- "C:/temp/budget/spreadsheet.xlsx"
basename(dirname(x))

# find URL in a string of text --------------------------------------------

text1 <- c("s@1212a www.abcd.com www.cats.com", 
           "www.boo.com", 
           "asdf",
           "blargwww.test.comasdf")
pattern <- "www\\..*?\\.com"
m <- gregexpr(pattern, text1)
regmatches(text1, m)


# find zip codes within a string ------------------------------------------

zips <- data.frame(id = seq(1, 5),
 address = c("Company, 18540 Main Ave., City, ST 12345",  
 "Company 18540 Main Ave. City ST 12345-0000",
 "Company 18540 Main Ave. City State 12345",
 "Company, 18540 Main Ave., City, ST 12345 USA", 
 "Company, One Main Ave Suite 18540, City, ST 12345")) 
regmatches(zips$address,
 gregexpr ('[0-9]{5}(-[0-9]{4})?(?!.*[0-9]{5}(-[0-9]{4})?)',  
 zips$address, perl = TRUE))

# DOT - any character -----------------------------------------------------


str_detect("\nX\n", ".X.")
str_detect("\nX\n", regex(".X.", dotall = TRUE))
my.regex <- ".Z."
x <- "aZa"
str_detect(x, regex(my.regex, dotall = TRUE))  
x <- "Za"
str_detect(x, regex(my.regex, dotall = TRUE)) 


# multiline specification -------------------------------------------------


x <- "Line 1\nLine 2\nLine 3\n"   
str_extract_all(x, "^Line..")[[1]]
str_extract_all(x, regex("^Line..", multiline = TRUE))[[1]]
str_extract_all(x, regex("\\ALine..", multiline = TRUE))[[1]]


# word boundaries ---------------------------------------------------------


x <- "My timex shows the correct time   "
str_extract(x, "\\btime\\b") 
str_extract (x, "\\Btimex\\B") #wrong
string1 <- "Mytimexshows the correct time   "
str_extract(string1, "\\Btimex\\B")
string2 <- "Mytimexisagenuinetimexforsure!"
str_extract (string2, "\\Btimex\\B")

# whitespace --------------------------------------------------------------

test.vector <- "I pressed the space bar          too many times"
gsub("\\s+"," ",test.vector)
 # \\s+ will match any space character or repeats of space characters, and will replace it with a single space " ".
some.text <- c("tree","frog  ","House99",
   "Claudia_Carol_Cathy")
i <- grep("[[:space:][:digit:]]+", some.text)
some.text[i]


# how to set locale -------------------------------------------------------


locale()
locale ("fr")
locale ("es", decimal_mark = ",")


# elements of regular expressions -----------------------------------------


string1 <- "This is elementary Watson. 1+1=2"
my.regex <- "1\\+1=2"
my.regex.replacement.value <- "two plus two equals four "
sub(pattern = my.regex,replacement = 
  my.regex.replacement.value,x = string1)
string1  <- "Why do Java Programmers have to wear glasses?  
  Because they can't C."
my.regex <- "[a]"
my.regex.replacement.value <- "dog"
sub(pattern = my.regex,replacement = 
  my.regex.replacement.value,x = string1)


# numeric ranges ----------------------------------------------------------


x <- c("a","99","89x", "2","333","123", "123456","367")
grepl("^[0-9]{1,3}$",x) 
grepl("^[0-9]{3}$",x) 
grepl("[0-5]",x) 
my.regex <- "^([012]?[0-9]?[0-9]|3[0-5][0-9]|36[0-6])$"  #0-366, only digits
grepl(my.regex,x)


# alpha ranges ------------------------------------------------------------


my.regex <- "^([01]?[0-9]?[0-9]|2[0-4][0-9]|25[0-5])$"
grepl(my.regex,x)
x <- c("a","99","89x", "2","333","qqq123", "123456z","367")
grepl("^[a-z]",x)


# case sensitivity --------------------------------------------------------

stringa <- "OX"
stringb <- "Post Box"
grep(stringa,stringb)
grep(stringa, stringb, ignore.case = TRUE)

# repetition --------------------------------------------------------------

x <- c("a", "aa", "aaa", "aaaa", "aaaaa")
my.regex <- "a{3}"
grepl(my.regex, x)
x <- c("max", "head head", "HEAD HEAD", "max Max max max")
x <- c("max", "head head", "HEAD HEAD", "max Max max max")
my.regex <- "\\b([A-Z,a-z]+)\\s+\\1\\b"
grepl(my.regex, x, perl = TRUE)


# negations - NOT syntax --------------------------------------------------


string = c("business_metric_one","business_metric_one_dk","business_metric_one_none","business_metric_two","business_metric_two_dk","business_metric_two_none")
string[!grepl("business_metric_[[:alpha:]]+_(dk|none)", string)]
my_strings <- c("a non-rheumatic fever",
   "a nonrheumatic fever", "a rheumatic fever", 
   "a not rheumatic fever")
grep("no[nt][- ]?rheumatic", my_strings, invert = TRUE, 
  value = TRUE)


# grouping ----------------------------------------------------------------


s <- "xy1234wz98xy567"
r <- "xy(\\d+)"
gsub(r, "\\1", regmatches(s,gregexpr(r,s))[[1]])
strings <- c(" 219 733 8965", "329-293-8753 ", "banana",
  "595 794 7569","387 287 6718", "apple", "233.398.9187  ",
  "482 952 3315","239 923 8115 and 842 566 4692",
  "Work: 579-499-7527", "$1000","Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_extract (strings, phone)
str_match (strings, phone)
str_extract_all(strings, phone)
str_match_all(strings, phone)
x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
str_match (x, "<(.*?)> <(.*?)>")
str_match_all(x, "<(.*?)>")
str_extract(x, "<.*?>")
str_extract_all(x, "<.*?>")

# quantifiers -------------------------------------------------------------


x <- c("aaa","one space","multiple      spaces", " bbbb ")
my.regex <- "^\\S+(?: \\S+)*$"  
grepl(my.regex,x)
my.regex <- "\\s"  
grepl(my.regex, x)
my.regex <- "\\s{2,100}"
grepl(my.regex,x)
my.regex <- "\\s{2,100}"
grepl(my.regex,x)


# case sensitivity --------------------------------------------------------


x <- c("aaa","BBB","ccCC")
my.regex <- "A"    
grepl(my.regex,x)
my.regex <- "(?i)A"  
grepl(my.regex,x)


# partial match -----------------------------------------------------------


x <- c("miles", "smiles", "milo","smite", "mi lovely vacation")
my.regex <- "mi*l"
grepl(my.regex, x)


# the alternation "|" metacharacter ---------------------------------------


x <- c("aaa","one space","multiple      spaces", " bbbb ")
my.regex <- "a|p"
grepl(my.regex,x)
my.regex <- "\\s+"
grepl(my.regex,x)

# look ahead/behind - starter example ------------------------------------


x <- c("2 pencils", "3 pencils", "4")
str_extract (x, "\\d+(?= pencils?)") 
y <- c("900", "$500")
str_extract(y, "(?<=\\$)\\d+") 


# grep pattern matching and replacement -----------------------------------

text <- "abc2"
grep("\\d+", text, value = TRUE)
text <- "abc"
grep("\\d+", text, value = TRUE)
grep("apple", c("crab apple", "Apple jack", "apple sauce", "apple_store"))
stringa <- "ox"
stringb <- "Post Box"
answer <- grep(stringa, stringb) 
answer
stringb <- "Post Boxox"
answer <- grep(stringa, stringb) 
answer
stringb <- "Post Bxx"
answer <- grep(stringa,stringb)
answer  
stringa <- "zz"
answer <- grep(stringa, stringb, invert = TRUE) 
answer
stringa <- "OX"
stringb <- "Post Box"
answer <- grep(stringa,stringb)   
answer
stringa <- "ox"
stringb.as.vector <- c( "Roxan", "bird", "frog", "moxy","MOXY")
answer <- grep(stringa, stringb.as.vector, value = TRUE)
answer


# grepl -------------------------------------------------------------------


# grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)
test <- "the earth has a radius of 3,986 miles"
digits <- grepl("\\d+", test)  
digits  
test.vector <- c("London","Boston","888Knoxville99")
grepl("*on",test.vector)   
grepl("\\d+",test.vector)
grepl("^([^0-9])",test.vector)  


# whitespace --------------------------------------------------------------


x <- "wwwMarywww"
grepl("Mary|Jane|Sue",x) 
grepl("\b(Mary|Jane|Sue)\b",x) 
grepl("\\S(Mary|jane|Sue)",x) 
date1 <- "2008-08-08"
pattern1 <- "\\b(\\d{4})-(\\d{2})-(\\d{2})\\b"
grepl(pattern1, date1)
grepl( "\\b(\\d{4})-(\\d{2})-(\\d{2})\\b", date1)


# sub ---------------------------------------------------------------------


sub("(q+)", "z\\1z", c("qbc", "def", "cbq q", "qq"), perl=TRUE)
vector1 <- c("department 123", "Think ahea", "99 bottles of 
  beverage on the wall")
new.vector1 <- sub("[[:digit:]]","7",vector1)                                                                      
new.vector1
new.vector1 <- sub("[[:digit:]]+","7",vector1)
new.vector1


# gsub --------------------------------------------------------------------


# gsub: Works exactly as sub, except that all occurrences of the pattern are replaced, within a string. 
test.vector <- c("ppp - 82", "g - 67.4", "w = 12.544")
scrubbed.var <- gsub("[^0-9]", "", test.vector)  
scrubbed.var
test.vector.with.period <- gsub("[^0-9\\.]", "", test.vector)
test.vector.with.period
test.vector.lower.case.alpha <- gsub("[^a-z]", "", test.vector)
test.vector.lower.case.alpha
test.vector.no.numbers <- gsub("\\d", "",test.vector)
test.vector.no.numbers
test.vector.no.spaces <- gsub("\\s","",test.vector) 
test.vector.no.spaces 


# more gsub examples ------------------------------------------------------


x <- "Lawrence of Arabia"
gsub("Arabia","Chicken scratch Tennessee",x)
gsub("arabia", "Chicken scratch Tennessee",x)
gsub("arabia","Chicken scratch Tennessee", x,ignore.case=T)
x <- "HMS Victory is a 104-gun ship of the line of the Royal Navy, launched in 1765."
y <- gsub("\\d+","***",x)
y


# regexpr -----------------------------------------------------------------


x <- c("version 1.2", "vvv24", "engine is a v8", "bigfoot", "have you been there?")
regexpr("ve", x)
x <- "Alexander the Great was born in 356 BCE"
y <- regexpr("\\d+",x)
y

# gregexpr ----------------------------------------------------------------


# gregexpr: The gregexpr function is used to identify where a pattern is within a character vector, where each element is searched separately.  The returned object is a list. In contrast, the regexpr function returns a vector. Example:
v1  <- " Do rats prefer pie 3.141 What does the golden rat prefer 1.6180 "
v2  <- gregexpr("\\d+",v1)
v2

# regexec -----------------------------------------------------------------

x <- "http://stat.umn.edu:80/xyz"
m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
m
regmatches(x, m)
str_extract_all("The Howdy Doody Show 123", "[:alpha:]")
str_extract_all("The Howdy Doody Show 123", "[:digit:]")

# unicode -----------------------------------------------------------------

x <- "Tthis string starts with a trademark sign"
my.regex <- "\u{2122}"
grepl(my.regex, x) 
x <- "this string has no trademark sign"
grepl(my.regex, x)
x <- "This string has a colon in it :  "
my.regex <- "\u{003A}"
grepl(my.regex, x)
x <- "\u00b5"
x

#-----------------------------------------------------


# recipes for common R tasks ----------------------------------------------



library(tidyverse)
library(readr)
library(datasets)
if (!require("sjPlot")) install.packages("sjPlot") 
if (!require("ggcharts")) install.packages("ggcharts")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("summarytools")) install.packages("summarytools")
if (!require("skimr")) install.packages("skimr")


# console input -----------------------------------------------------------


# my.city.name <- readline(prompt="What is your city name ")
# my.city.nameX <-  readline("Enter a number")
# Enter a number x 


# read and write csv files ------------------------------------------------


setwd("c:\\temp") #set working directory. Your setting will vary
write_csv(mpg, "my.mpg.csv")
y <- read_csv("my.mpg.csv")
head(y,2)


# windows - copy a file ---------------------------------------------------

#need to have a jpg file in starting folder subdirectory. Use your own drives and subdirectories
starting.folder <- "d:/t"
ending.folder <- "d:/t2"
files.to.be.backed.up <- list.files(starting.folder, ".jpg$")
file.copy(files.to.be.backed.up, ending.folder) 


# built-in datasets -------------------------------------------------------


library(MASS)
data() #shows base datasets available.  
data(Nile)
head(Nile) 
class(Nile)
x <- as.data.frame(Nile)
class(x)
detach("package:MASS", unload=TRUE)  #MASS seems to interfere with tidyverse in many ways; no need to keep it in memory

# visualization - histogram -----------------------------------------------

ggplot(data=mtcars, aes(mpg))+geom_histogram(aes(y =..density..),
    fill="red4")+geom_density()


# horizontal bar chart ----------------------------------------------------
#faceted

library(ggcharts)
df <- txhousing %>% 
  group_by(city,year) %>% 
  summarise(sales_tot = sum(sales))
df %>% filter(year %in% c(2012,2013,2014,2015)) %>% 
  bar_chart(x = city, y = sales_tot, facet = year, top_n = 10)


# lollipop ----------------------------------------------------------------


df <- LifeCycleSavings  
df <- cbind(Country=rownames(df), df) 
  #row names converted to a column
df$disposable.income <- df$dpi
df <- df[1:10,]
   # reduce no of rows for illustration purposes
ggplot(df, aes(x=Country, y=disposable.income)) +
 geom_point() + geom_segment( aes(x=Country, xend=Country, y=0, 
 yend=disposable.income)) +
 labs(title = "Disposable Income for Selected Countries", 
 subtitle = "Country vrs disp inc",
 caption = "Source:LifeCycleSavings built in dataset") +
 theme(axis.text.x = element_text(angle = 65, vjust=0.6))


# step chart --------------------------------------------------------------


postage = read.csv("http://datasets.flowingdata.com/us-postage.csv", sep = ",", header=T)
head(postage,3); plot(postage$Year, postage$Price, type = "s")


# diverging bars ----------------------------------------------------------


data("swiss") 
theme_set(theme_bw()) 
data("swiss")  # load data (R built-in dataset)
swiss$"Provence" <- rownames(swiss)
swiss$Education_z <- round((swiss$Education -
  mean(swiss$Education))/sd(swiss$Education), 2) 
swiss$Education_type <- ifelse(swiss$Education_z < 0,
 "below", "above")
swiss <- swiss[order(swiss$Education_z), ]
swiss$`Provence` <- factor(swiss$`Provence`,
  levels = swiss$`Provence`) 
ggplot(swiss, aes(x=`Provence`, y=Education_z, label="Relative Score in Education")) +
  geom_bar(stat='identity', aes(fill=Education_type), width=.5)  + scale_fill_manual(name="Education Level in Years",
  labels = c("Above Average", "Below Average"),
  values = c("above"="#00ba38", "below"="#f8766d")) +
  theme(text = element_text(size=08)) +
  labs(subtitle="Education Levels by Provence",
       title= "Diverging Bars") +
  coord_flip()


# colorful display - categorical frequencies ------------------------------


if (!require("inspectdf")) install.packages("inspectdf")
library(inspectdf)
mpg %>% 
  inspectdf::inspect_cat() %>% 
  inspectdf::show_plot(text_labels = TRUE)


# donut chart -------------------------------------------------------------


if (!require("ggpubr")) install.packages("ggpubr")
df <- data.frame(supplies = c("Pencils", "Paper", "Staples", "Elmer's Glue"),
    value = c(50, 200, 122, 5))
ggdonutchart(df, "value", label = "supplies")

ggdonutchart(df, "value", label = "supplies",
             fill = "supplies", color = "white",
             palette = c("#00AFBB", "#E7B800", "#FC4E07","13FC07") )


# bubble plot -------------------------------------------------------------


data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
ggplot(df, aes(x = wt, y = mpg)) + 
  geom_point(aes(color = cyl, size = qsec), alpha = 0.5) +
  scale_color_manual(values = c("#00bb06", "#E7B800", 
  "#FC4E07")) + scale_size(range = c(0.4, 12))#change point size 


# scatterplot -------------------------------------------------------------


unemployment = read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv", 
  sep = ",")
plot(1:length(unemployment$Value), unemployment$Value)


# scatterplot with fited loess curve --------------------------------------


scatter.smooth(x=1:length(unemployment$Value), 
  y = unemployment$Value)
scatter.smooth(x= 1:length(unemployment$Value), 
  y = unemployment$Value, ylim=c(0,11), degree=2, 
  col = "#CCCCCC", span = 0.5)


# yearly plot using alternative theme -------------------------------------


if (!require("devtools")) install.packages("devtools")   
library(devtools) 
devtools::install_github('cttobin/ggthemr')
library(ggthemr)
library(lubridate)
dollars <- rep(1:1,20)  #create a vector of 20 elements
dollars[1] <- 100  # initial deposit of $100
for (i in 2:20) dollars[i] <- dollars[i -1] * 1.05 
dollars <- as.data.frame(dollars)
dollars$date <- seq(as.Date("2020/1/1"), 
  as.Date("2039/1/1"), "years")
names(dollars) <- c("Value", "Interest.Date")
ggthemr("earth", type="outer", layout="scientific", spacing=2)
ggplot(dollars, aes(Interest.Date, Value)) +
  geom_line() +
  geom_smooth(method='lm') +
  ggtitle("Investment value of $100 at 5% annual compounding")


# plot all variables in a DF against all other variables (numeric) --------


if (!require("GGally")) install.packages("GGally") 
ggpairs(iris)


# line plot using two sources of data -------------------------------------


n <- 20
x1 <- 1:n
y1 <- rnorm(n,0,.5)
df1 <- data.frame(x1,y1)
x2 <- (.5 * n):((1.5*n) -1)
y2 <- rnorm(n,1,.5)
df2 <- data.frame(x2,y2)
theme_set(theme_grey())
ggplot() +
   geom_line(data = df1, aes(x1,y1), color = "darkblue") +
   geom_line(data = df2, aes(x2, y2), linetype = "dashed", 
   color = "red") +
   ggtitle("Two data sources plotted")


# correllogram ------------------------------------------------------------


library(ggplot2)
if (!require("ggcorrplot")) install.packages("ggcorrplot") 
data(mtcars)
corr <- round(cor(mtcars), 1)
ggcorrplot(corr, hc.order = TRUE,
 type = "lower",
 lab = TRUE,
 lab_size = 3,
 method="circle",
 colors = c("tomato2", "white", "springgreen3"),
 title="Correlogram of mtcars",
 ggtheme=theme_bw)


# word cloud --------------------------------------------------------------


require(tm)
# install.packages("wordcloud2")
require(wordcloud2)
x1 <-  "Football From Wikipedia, the free encyclopedia the free encyclopediaFootball is a family of team sports that involve, to varying degrees, "
x2 <-  "kicking a ball to score a goal. Unqualified, the word football normally means the form of football that is the most popular where" 
x3 <- "the word is used. Sports commonly called football include association football (known as soccer in some countries); "
x4 <- "gridiron football (specifically American football or Canadian football); Australian rules football; rugby football (either rugby"
x5 <-  "union or rugby league); and Gaelic football.[1][2] These various forms of football share to varying extent common origins and are known"
x6 <- "as football codes."

x <- paste0(x1, x2, x3, x4, x5, x6, sep = "")

words <- toString(x) 
words <- str_split(words, pattern = " ", simplify = TRUE)
set.seed(1) 
wordcloud(words, colors = viridis::viridis_pal(end = 0.8)(10),
          min.freq = 1, random.color = TRUE)


# ggplot2 - time series - airline crashes historical data -----------------
#you will need to download this dataset
#from https://data.world/data-society/airplane-crashes/workspace/file?filename=Airplane_Crashes_and_Fatalities_Since_1908.csv
setwd("d:/a/misc5")
airline.crashes <- 
  read_csv("Airplane_Crashes_and_Fatalities_Since_1908.csv")
date.and.fatalities.only <- select(airline.crashes, Date,
                                   Fatalities)

date.and.fatalities.only <- mutate(date.and.fatalities.only, 
  crash.date = mdy(Date))
p <- ggplot(data = date.and.fatalities.only, aes(x = crash.date, y = Fatalities)) +
  geom_point() +
  labs(x = "Date",
    y = "Airline Fatalities",
    title = "Flying is safer than driving but you could still win the devil's lottery",
    subtitle = "source = https://data.world/data-  
   society/airplane-crashes")
p + theme_linedraw()

# text plot ---------------------------------------------------------------



if (!require("textplot")) install.packages("textplot")
if (!require("udpipe")) install.packages("udpipe")
data(brussels_listings, package = 'udpipe')
x <- table(brussels_listings$neighbourhood)
x <- sort(x)
textplot_bar(x,
panel = "Locations", col.panel = "darkgrey", xlab = "Listings",
cextext = 0.75, addpct = TRUE, cexpct = 0.5)


# dot plot ----------------------------------------------------------------


library(tidyverse)
theme_set(theme_classic())
ggplot(mpg, aes(x=manufacturer, y=cty)) + geom_point(col="tomato2", size=3) + 
  geom_segment(aes(x=manufacturer, xend=manufacturer,
  y=min(cty), yend=max(cty)), linetype = "dashed", size=0.1) +
  labs(title="Dot Plot",
  subtitle="Manufacturer(Make) Vs Mileage in the City", caption="source: mpg") + coord_flip()


# survival analysis -------------------------------------------------------


if (!require("survival")) install.packages("survival")
if (!require("survminer")) install.packages("survminer")
data("lung")
head(lung)
fit <- survfit(Surv(time, status) ~ sex, data = lung)
ggsurvplot(fit,
          pval = TRUE, conf.int = TRUE,
          risk.table = TRUE, # Add risk table
          risk.table.col = "strata", # Change risk table color by groups
          linetype = "strata", # Change line type by groups
          surv.median.line = "hv", # Specify median survival
          ggtheme = theme_bw(), # Change ggplot2 theme
          palette = c("#E7B800", "#2E9FDF"))


# prophet -----------------------------------------------------------------


library(prophet)
library(lubridate)
start1 <- mdy("01/01/2001")
end1 <- mdy("01/01/2003")
the.days <- seq(start1, end1,"days")  #daily
co2.levels <- as.numeric(co2)
max.row <- length(co2.levels) 
the.days <- the.days[1:max.row]
df <- data.frame(the.days = the.days, co2.levels = co2.levels)
names(df) <- c("ds", "y")
tail(df)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 50)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)


# holt-winters ------------------------------------------------------------


if (!require("forecast")) install.packages("forecast")
if (!require("graphics")) install.packages("graphics")
if (!require("zoo")) install.packages("zoo") 
values1 <- c(2821.93,
             2839.96,
             2818.37,
             2840.69,
             2850.13,
             2857.05,
             2862.96,
             2861.82,
             2856.98,
             2874.69,
             2896.74,
             2897.52,
             2914.04,
             2901.13,
             2901.52,
             2896.72,
             2888.60,
             2878.05,
             2871.68,
             2877.13,
             2887.89,
             2888.92,
             2904.18,
             2904.98,
             2888.80,
             2904.31,
             2907.95,
             2930.75,
             2929.67,
             2919.37,
             2915.56,
             2905.97,
             2914.00,
             2913.98,
             2924.59,
             2923.43,
             2925.51,
             2901.61,
             2885.57,
             2884.43,
             2880.34,
             2785.68,
             2728.37,
             2767.13,
             2750.79,
             2809.92,
             2809.21,
             2768.78,
             2767.78,
             2755.88,
             2740.69,
             2656.10,
             2705.57,
             2658.69,
             2641.25,
             2682.63,
             2711.74,
             2999.91,
             3013.77,
             3014.30,
             3004.04,
             2984.42,
             2995.11) 
values.as.time.series <- ts(values1, start = c(2009,12), 
  frequency = 12)
model.hw = HoltWinters(values.as.time.series)
p = predict(model.hw, 15) #look at 15 periods into the future
p
m = merge(a = as.zoo(values.as.time.series),
   b = as.zoo(p))
m = as.ts(m)
ts.plot(m, ylab = "Values")
data(mtcars)


# multivariate regression -------------------------------------------------


fit <- lm(mpg ~ am + vs + cyl + disp + hp + wt + drat, data = mtcars)
car.test <- mtcars[c(1,5,15,21),]  #test with four records
my.prediction <- fit %>% predict(car.test)  
car.test[,1]
my.prediction


# basic numeric vector tests ----------------------------------------------


library(datasets)
library(Hmisc)
library(MASS)
data("Animals")
A <- Animals
median(A$brain) # "in the middle"
mean(A$brain)  #average
range(A$brain)  #min and max
mode(A$brain)
sd(A$brain) #standard deviation
plot(A$brain)
qqnorm(A$brain)  

# coefficient of variation ------------------------------------------------

sd(A$brain)/median(A$brain)     #coefficient of variation
summary(A)


# str ---------------------------------------------------------------------


str(A)


# five number tukey statistics --------------------------------------------


fivenum(A$brain)  
describe(A$brain)


# attractive tabular output -----------------------------------------------
install.packages("sjPlot")
library(sjPlot)
tab_df(mpg[1:10,]) #uses built-in mpg dataframe


# formattable -------------------------------------------------------------

if (!require("formattable")) install.packages("formattable")
library(formattable)
formattable (mpg[1:10,]) 

# distributions -----------------------------------------------------------


# normal ------------------------------------------------------------------



x <- rnorm(n=5)
x  # five normally distributed random numbers (numeric, vector)
x <- rnorm(n=20, mean=1000, sd=100)
x 
x <- qnorm(0.95, mean=100, sd=15)
x 
x <- qnorm(0.98, mean = 100, sd = 15)
x 
x <- 1 - pnorm(19, mean=17.46, sd=18.1)
x

# binomial ----------------------------------------------------------------


x <- dbinom(x = 6, size = 10, prob = .3)
x

# poisson -----------------------------------------------------------------


ppois(16, lambda=12)   # lower tail 
ppois(16, lambda=12, lower=FALSE)   # upper tail 



# convenience summary (dfSummary) -----------------------------------------

install.packages("summarytools")
library(summarytools)
dfSummary(airquality) 

# skimr -------------------------------------------------------------------

# install.packages("skimr")
library(skimr)
skim(airquality)


# Hmisc -------------------------------------------------------------------


Hmisc::describe(airquality) 


# standard summary command ------------------------------------------------


summary(airquality)


# table command with numeric vectors --------------------------------------


x <- mtcars$mpg
table(x)
length(x) - length(unique(x)) 


# heat map of correlations ------------------------------------------------
detach("package:MASS", unload=TRUE)   #this will give an error message if MASS is not loaded

library(reshape2)
data <- airquality[,1:4]
qplot(x=Var1, y=Var2, data=melt(cor(data, use="p")), fill=value, geom="tile", 
      main="correlations") +  scale_fill_gradient2(limits=c(-1, 1))


# easy models -------------------------------------------------------------


my.model <- glm(formula= vs ~ wt + disp, data=mtcars, 
  family=binomial)
summary(my.model)
newdata = data.frame(wt = 2.1, disp = 180)
predict(my.model, newdata, type="response")


# prediction using linear regression --------------------------------------


Input_variable_speed <- data.frame(speed = 
      c(10,12,15,18,10,14,20,25,14,12))
linear_model = lm(dist~speed, data = cars)
my_predictions <- predict(linear_model, 
   newdata = Input_variable_speed)
my_predictions


# tm package to remove punctuation ----------------------------------------


# Text mining, also referred to as text data mining, similar to text analytics, is the process of deriving high-quality information from text. It involves "the discovery by computer of new, previously unknown information, by automatically extracting information from different written resources."[1] Written resources may include websites, books, emails, reviews, and articles. High-quality information is typically obtained by devising patterns and trends by means such as statistical pattern learning. According to Hotho et al. (2005) we can differ three different perspectives of text mining: information extraction, data mining, and a KDD (Knowledge Discovery in Databases) process.[2] Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interest. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities).
require(tm)
x <- c("Please....don't throw me in #1 briar #### $$$$  **** patch")
y <- removePunctuation(x)
y
x <- c("this has both numbers like 3,5 and letters and punctuation.....")
z <- removeNumbers(x)
z

# sampling ----------------------------------------------------------------


total.quantity.of.samples <- 200
number.to.be.drawn <- 50
x <- sample(total.quantity.of.samples, number.to.be.drawn, replace = TRUE)
x
my.sample <- 20
y <- split(sample(my.sample), rep(c("control","treatment"),10))
y
z <- as.data.frame(y)
head(z,5)  


# financial functions -----------------------------------------------------


require(quantmod)
price = getSymbols('T', src = 'yahoo',
   from = '2002-01-01', auto.assign = F)
getOption("getSymbols.env")
price = Cl(to.monthly(price, indexAt='endof'))
plot(price, main = "AT&T EOM Stock Price")
plot(diff(price), main = "Month to Month change:AT&T")


# validation --------------------------------------------------------------

# install.packages("validate")
library(validate)
big.engines <- check_that(mtcars, hp >250, cyl >5,  disp < 300)
big.engines   
summary(big.engines)
plot(big.engines)
df <- data.frame(id = 11001:11003, year = c("218","2019","2020"), value = 1:3)
rule <- validator(field_length(year, 4), field_length(id, 5))
out <- confront(df, rule) 
out <- as.data.frame(out)
out

# create a dataframe on the fly -------------------------------------------


test.df <- data.frame( tool = c("Hammer", "Screwdriver","Saw"), 
                       weight = c(1.5,0.5,1.2), cost = c(10,5, 12),  
                       purchased.at = c("Lowes","ACE", "Home Depot"))  


# remove read only from a windows file ------------------------------------

setwd("d:\\t")  #your working directory setting will vary
system("attrib -r test.file.docx")

# rename a windows file ---------------------------------------------------


from.file <- c("my.word.docx")
another.name <- sample(seq(1:99999),size = 1)
another.name <- paste("new.word.doc.name",as.character(another.name), ".docx")
to.file <- another.name; file.rename(from.file, to.file);to.file 





