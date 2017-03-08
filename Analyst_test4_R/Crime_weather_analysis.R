'
Author: Rory Pulvino
Date: Feb. 23, 2017
Title: Crime - Weather Analysis
Summary: This analysis looks at the relationship between weather
in San Francisco and car burglaries in the city. The crime data
is taken from the City open data initiative while the weather 
data was scraped from NOAA website of historical weather data 
for the city.
'
################################################################
############             Packages                 ##############
################################################################

pkgs <- c("purrr", "data.table", "dplyr", "tidyr", "readr", "lubridate", "stringr",
          "ggplot2", "igraph", "tidytext", "FactoMineR", "openNLP", "fpc", "wordcloud")
allLoaded <- sapply(pkgs, require, character.only = TRUE)

################################################################
############             Data                     ##############
################################################################

Weather <- read.csv("~/Dropbox/Python/Example_test_projects/Analyst_test4_R/weather_SF_2016.csv", header = TRUE)
Crime <- read.csv("~/Dropbox/Python/Example_test_projects/Analyst_test4_R/SFPD_crime.csv", header = TRUE)

################################################################
############             Tidying data             ##############
################################################################

# Creating new columns for date as date objects
Weather$Date_r <- as.Date(as.character(Weather$Date), "%m/%d/%y")
Crime$Date_r <- as.Date(Crime$Date, "%m/%d/%Y")

'For ease, cutting down the crime data down to just crimes in 
2016. Also going to cut down the crimes to those that contain
BURGLARY in the description of the crime for analysis to focus
more closely on the question originall proposed.'
Crime_new <- Crime %>% filter(Date_r > as.Date('2015-12-31') & Date_r < as.Date('2017-01-01'))
Crime_f <- Crime_new %>% filter(grepl("BURGLARY",Descript))

'Further cutting down the data set to burglaries related to 
vehicles.'
Crime_f <- Crime_f %>% filter(grepl("VEH",Descript))

'From looking at the data, it appears there are two types of 
burglaries related to cars, attempted and other (presumably
completed).'

# Merging datasets of crime and weather
df <- inner_join(Crime_f, Weather, by = "Date_r")

################################################################
############      Exploring & Analyzing data      ##############
################################################################

# Visualizing burglaries by day of the week
ggplot(df)+
  geom_bar(aes(x = DayOfWeek))
'There is clear pattern to when cars are burglarized. Friday and
Saturday have distinctly low values that could be tested with 
more data to see if those low values are statistically significant.
Wednesday also has a much higher value than the other days. The
low values likely signify increase use of cars that decreases the
opportunity for theft, while Wednesday falling in the middle of 
the week probably has a higher incidence because more people are
at the office with cars unattended that day of the week.'

# Visualizing burglaries by month 
ggplot(df)+
  geom_bar(aes(x = month(Date_r)))
'This data reveals a clear spike in car burglaries during July
and August. There is a hug decrease in September that continues
till December when levels go back to what seems to be the norm.
The spike could be related to a number of factors, increased 
tourism in the summer, kids being out of school, or better 
weather meaning people to use their cars less and walk more. 
The steady decrease after August could be a sign of the end of 
summer or due to a city/police initiative. More data is needed
to understand this data and make recommendations.'

# Visualizing burglaries as correlated with weather
'First looking at the spread of temperature data, I am using the 
average temperature for the day rather than a max or min.'
summarise(df, mean=mean(Average.Temperature, na.rm = TRUE), median=median(Average.Temperature, na.rm = TRUE), min=min(Average.Temperature, na.rm = TRUE), max=max(Average.Temperature, na.rm = TRUE))

summarise(Weather, mean=mean(Average.Temperature, na.rm = TRUE), median=median(Average.Temperature, na.rm = TRUE), min=min(Average.Temperature, na.rm = TRUE), max=max(Average.Temperature, na.rm = TRUE))

'Temperatures are spread between 45 to 78, a range of 33. I will
bin these based on every two degrees of temperature then. I am 
overlaying a histogram from the weather data to get a sense of 
whether burglaries happen more frequently at certain temperatures
or if its merely a function of more days being at those temperatures.'
ggplot()+
  geom_histogram(aes(x = Average.Temperature), data = df, fill = "red", alpha = 0.2, binwidth = 2)+
  geom_histogram(aes(x = Average.Temperature), data = Weather, fill = "blue", alpha = 0.2, binwidth = 2)
'Nothing from this data jumps out to show that there is a correlation
between temperature and burglaries. This is despite the earlier 
finding that more burglaries happened in the summer months. SF also
has fairly consistent temperatures, so the variation in temp may be
too little to drive behavior in the city.'

# Visualizing burglaries as correlated with precipitation
'First looking at the spread of precipitation data.'
summarise(df, mean=mean(Precipitation, na.rm = TRUE), median=median(Precipitation, na.rm = TRUE), min=min(Precipitation, na.rm = TRUE), max=max(Precipitation, na.rm = TRUE))

summarise(Weather, mean=mean(Precipitation, na.rm = TRUE), median=median(Precipitation, na.rm = TRUE), min=min(Precipitation, na.rm = TRUE), max=max(Precipitation, na.rm = TRUE))

'Rain has a range of 0 to 1.31 inches for 2016. I will bin this 
based on every tenth of an inch of rain. In a more thorough look
I would try different binning.'
ggplot()+
  geom_histogram(aes(x = Precipitation), data = df, fill = "red", alpha = 0.2, binwidth = 0.1)+
  geom_histogram(aes(x = Precipitation), data = Weather, fill = "blue", alpha = 0.2, binwidth = 0.1)
'As with temperature, there does not appear to be much correlation
between rain and burglaries. The small sample size may be driving
this result as it rarely rained in 2016 so including more data 
would be helpful.'

################################################################
############            Further Analysis          ##############
################################################################
'Ideally, I would build a substantially larger data set with more
variables that would allow for more testing of hypothesis. This 
analysis though is a quick look at how to begin the problem-solving
process. With more data, I could also test hypothesis that car
burglaries increase in the summer months and are correlated with 
the days of the week.'
