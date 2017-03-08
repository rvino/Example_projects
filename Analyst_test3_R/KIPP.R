# Data Test for KIPP
# Rory Pulvino
# Due: Dec 20, 2016

# Standard data imports
library(dplyr)
library(ggplot2)
library(tidyr)


########################### Importing Data #################################
############################################################################

# Bringing in the relevant excel sheets
library(xlsx)
xref <- read.xlsx("~/Dropbox/Python/Example_test_projects/Analyst_test3_R/Excel_Exercise_Dec2016.xlsx", sheetName = "XRef", header = TRUE)
View(xref)
teach <- read.xlsx("~/Dropbox/Python/Example_test_projects/Analyst_test3_R/Excel_Exercise_Dec2016.xlsx", sheetName = "Teachers", header = TRUE)
View(teach)

########################### Dealing with Dates #############################
############################################################################

# Converting dates in xref to proper format
xref$ed_start_date <- as.Date(as.numeric(as.character(xref$Teaching_Start_Date)), origin = '1899-12-30')
xref$ed_end_date <- as.Date(as.numeric(as.character(xref$Teaching_End_Date)), origin = '1899-12-30')

####### Missing dates ########
"There are many observations with missing end dates. I assume this means the teacher
is still teaching. Since it is not clear when these teachers will quit and given that 
imputing an end date will introduce bias to an analysis of those teachers that have an
end date. I will put in 2016-12-20 as their end date and a dummy to control for the fact that
they are still teaching. I will drop the observations with missing start dates."

xref1 <- xref %>% drop_na(ed_start_date) # Dropping those with missing start dates

# Changing missing end dates to today's date
xref1$ed_end_date[is.na(xref1$ed_end_date)] <- as.Date.numeric(42724, origin = '1899-12-30')

# Creating a dummy variable to denote that a teacher is still teaching
xref1$still_teach <- (factor(with(xref1, ifelse((ed_end_date == '2016-12-20'), 1, 0))))

# Dropping observations with a Went_Id but a missing end date as this is a data entry where I can't tell if the person is still teaching or not
xref1$Went_Id[xref1$Went_Id == 'NULL'] <- NA
ind <- which(with( xref1, (ed_end_date=='2016-12-20' & (is.na(Went_Id) == FALSE | Went_Id == 1)) )) # Pulls out the indices of the observations I want to drop
xref1 <- xref1[-ind, ] # dropping those unwanted observations

####### Cleaning up dates ########
" Some dates from the excel file were listed as 1/0/00, those came into R as 0.
When converted to proper date format, since the origin was 1899-12-30, they were 
converted to that date. Will likely eventually change those to missing. Will look at them 
 to see if there is a pattern to them. Also, #VALUE! cells in excel came into R as NA which is proper."

# Creating a variable for how long a teacher stayed in the job
xref1$time_teach <- as.numeric(difftime(xref1$ed_end_date, xref1$ed_start_date, units = 'days'))
# Some of these have decimals which doesn't make sense in a days calculation, I'm dropping the decimals and rounding down to the nearest day
xref1$time_teach <- trunc(xref1$time_teach)

# Checking out time_teach to see if there are irregularities
summarise(xref1, mean=mean(time_teach, na.rm = TRUE), median=median(time_teach, na.rm = TRUE), min=min(time_teach, na.rm = TRUE), max=max(time_teach, na.rm = TRUE))

# Obviously the min and max values are concerning, so looking at the dataframe and sorting on time_teach
"There are many negative values for the number of days taught which needs to be addressed. 
Two of the huge negative outliers list 1899-12-31 as their end dates which means that the original date that 
came into R was 1/1/00. The other four huge negative outliers are data entry errors with the start years listed 
as 2103, 2104, 2105 instead of 2013, 2014, 2015. There is only one large positive outlier and it is a clear 
data entry error of the end year being listed as 3015. Given that this teacher started in 2012, this year was 
likely meant to be 2015."

# Fixing data entry errors
xref1$ed_end_date <- gsub('3015','2015', xref1$ed_end_date)
xref1$ed_start_date <- gsub('2104','2014', xref1$ed_start_date)
xref1$ed_start_date <- gsub('2103','2013', xref1$ed_start_date)
xref1$ed_start_date <- gsub('2105','2015', xref1$ed_start_date)

# Looking at the 1899-12-30  and 1899-12-31 date entries
df <- xref1 %>% filter(ed_start_date == "1899-12-30" | ed_start_date == "1899-12-31" | ed_end_date == "1899-12-30"| ed_end_date == "1899-12-31")

"Pulling out these observations there are not many. Interesting that the 1899-12-30 entries only show
up in the ed_start_date variable and 1899-12-31 only shows up in ed_end_date. Looking over the observations
There is not a clear pattern to the entries, they are not all from the same school and even where schools are 
the same there is not consistency in the entry. I will change these entries to missing given this information."

# Converting 1899-12-30 and 1899-12-31 dates to missing
xref1[xref1 == '1899-12-30'] <- NA
xref1[xref1 == '1899-12-31'] <- NA

# recalculating time_teach
xref1$time_teach <- as.numeric(difftime(xref1$ed_end_date, xref1$ed_start_date, units = 'days'))
# Some of these have decimals which doesn't make sense in a days calculation, I'm dropping the decimals and rounding down to the nearest day
xref1$time_teach <- trunc(xref1$time_teach)

# Looking at observations with negative values as number of days teaching
df <- xref1 %>% filter(time_teach <= 0)
summarise(df, mean=mean(time_teach, na.rm = TRUE), median=median(time_teach, na.rm = TRUE), min=min(time_teach, na.rm = TRUE), max=max(time_teach, na.rm = TRUE), mode=mode(time_teach))
ggplot(df) +
  geom_histogram(aes(x = time_teach)) # Shows the bunching that you can see when sorting the data, very few outliers beyond 400 days

"There are only 6 outliers beyond 400. 4 of these entries list the same end date 2001-01-01 and the other two share
the same end date of 2001-08-01. They are from different schools and thus are likely not data entry errors
from the same person, unless data entry is done at a more aggregate level. Given this and that they do not share
other information, the shared dates likely do no point to any systematic error in these, but are likely entry errors."

ggplot(df) +
  geom_histogram(aes(x = time_teach), binwidth = 2) +
  coord_cartesian(xlim = c(-400, 0)) # zooming in on teachers with 400 days or less

# Quickly shows the bunching at certain dates that's apparent when just scrolling through the data or in the summary stats.
"The negative numbers are mostly grouped at 365, 90/91 and 39/40 days. This makes sense
as these days align with 1 year, 3 months, and a little over 1 month. All of these are sensible cutoffs for
when teachers might likely leave or be let go after a trial period. These do no point to any systematic error."

"From looking at the outliers and the grouped data of the negative days variable there does not appear to be
systematic error in them. Rather it appears that the end and start dates were misentered and should be switched.
I will switch these columns for these entries and add a dummy variable to indicate that these are potential
errorenous entries for consideration in later analysis."

# Adding dummy
xref1$error <- (factor(with(xref1, ifelse((time_teach > 0), 0, 1))))
xref1$error <- as.numeric(as.character(xref1$error))

# Flipping start and end dates for those entries
xref1$start_date_ed <- factor(with(xref1, ifelse((time_teach > 0), xref1$ed_start_date, xref1$ed_end_date)))
xref1$end_date_ed <- factor(with(xref1, ifelse((time_teach > 0), xref1$ed_end_date, xref1$ed_start_date)))

# Recalculating time spent teaching
xref1$time_teach <- as.numeric(difftime(xref1$end_date_ed, xref1$start_date_ed, units = 'days'))
# Some of these have decimals which doesn't make sense in a days calculation, I'm dropping the decimals and rounding down to the nearest day
xref1$time_teach <- trunc(xref1$time_teach)

"With all data aggregated I can check whether the cutoffs appearing in the negative days are
random or systematic."

xref1 %>% 
  filter(time_teach == 39 | time_teach == 40 | time_teach == 90 | time_teach == 91 | time_teach == 365) %>% # pulling out the suspicious days
  group_by(time_teach) %>%
  summarise(mean=mean(error))

"It seems that only teaching for 39, 40, 90, or 91 days pretty much only happened to those that I pulled
out as having errors in the data entry. This is greater reason to do the later analysis with and without 
these entries. My guess is that these days are general cutoffs in KIPP policy as a trial period and that
the date then is not entered at the school level."

########################### Dealing with duplicates ###########################
############################################################################
length(unique(xref1$Teacher_ID)) # With 10678 unique obs, there must be duplicates in xref1

dupl <- xref1[duplicated(xref1[c(2:8)]),] # These are observations that are complete duplicates
xref1 <- xref1 %>% # Dropped complete duplicate rows, 7 observations
  distinct(Teacher_ID, School_ID, Prev_Years_Teaching, Teaching_Start_Date, Teaching_End_Date, Went_Id, .keep_all = TRUE)

dupl1 <- xref1[duplicated(xref1[c(2,4:8)]),] # These are observations that are complete duplicates other than having taught at different schools

"These observations can be dropped as there is no way to differentiate these observations
other than by school_id which given the other information I cannot distinguish the observations.
I also believe this is likely a case where a teacher transferred schools."

xref1 <- xref1 %>% # Dropping 7 observations that are duplicates other than for School_ID
  distinct(Teacher_ID, Prev_Years_Teaching, Teaching_Start_Date, Teaching_End_Date, Went_Id, .keep_all = TRUE)

dupl1 <- xref1[duplicated(xref1[c(2,4:6)]),]
dupl2 <- xref1[duplicated(xref1[c(2,4:6)], fromLast = TRUE),]

"Looking at the above, there are 2 duplicate observations where only the Went_id variable differs.
The two duplicates each list that variable as unknown, while the other observations give a
value, so I will keep those since it is more likely they are the accurate observations and 
two observations likely will not affect the analysis as well."

ind <- which(with( xref1, (Teacher_ID==40752 & Went_Id==7) | (Teacher_ID==37576 & Went_Id==7) )) # Pulls out the indices of the observations I want to drop
xref1 <- xref1[-ind, ] # dropping those unwanted observations
  
###### Dealing with duplicates of Teacher_ID and start_date #########
dupl1 <- xref1[duplicated(xref1[c(2,13)]),]
dupl2 <- xref1[duplicated(xref1[c(2,13)], fromLast = TRUE),]
dupl <- rbind(dupl1, dupl2)

"Looking at these observations, those where Went_Id = 1 denote teachers that transferred
to other KIPP schools, but stayed on as KIPP teachers. These duplications I drop 
as the later date more accurately reflects how long the teacher stayed on."

# Creating a dummy variable of those observations not to be kept
xref1$stayed <- as.numeric(as.character((factor(with(xref1, ifelse(((duplicated(xref1[c(2,13)], fromLast = TRUE) | duplicated(xref1[c(2,13)])) & Went_Id == 1), 1, 0))))))
xref1$stayed[is.na(xref1$stayed)] <- 0

xref2 <- xref1 %>%
  select(Teacher_ID:Prev_Years_Teaching, Went_Id, still_teach:stayed) %>% # Dropping unneeeded columns
  filter(stayed == 0) # Dropping those observations where the teacher actually continued on as a KIPP teacher at a different school

"For the remaining observations in this group, there is not a necessarily logical way to
distinguish observations. The Went_Id is not revealing as to which observation should be 
kept, so I again keep distinct observations. The last thing to check in this area are
observations that repeat teacher_ID, start_date, and school_ID since these may show if a school
is misentering data and can be fixed."

dupl1 <- xref2[duplicated(xref2[c(1,2,9)]),]
dupl2 <- xref2[duplicated(xref2[c(1,2,9)], fromLast = TRUE),]
dupl <- rbind(dupl1, dupl2)

"There are only 5 pairs in the above set, so looking at them there is not a clear pattern to their 
duplication. Though I could keep one of the observations from each pair, it is not clear which 
is most logical to keep from each pair so I will drop them all. Given the small number of observations
this should not be a huge issue. In dropping observations, I will keep those with Went_Id."

dupl1 <- xref2[duplicated(xref2[c(1,8)]),] # This shows I should drop 38 observations.

xref3 <- xref2 %>% # Dropping 38 observations
  arrange(Went_Id) %>%
  distinct(Teacher_ID, start_date_ed, .keep_all = TRUE)

###### Dealing with duplicates of Teacher_ID and end_date #########
dupl1 <- xref3[duplicated(xref3[c(1,9)]),]
dupl2 <- xref3[duplicated(xref3[c(1,9)], fromLast = TRUE),]
dupl <- rbind(dupl1, dupl2)

"This is a strange group. There are only 18 pairs and I will use distinct to take
only one of the observations. I will prioritize keeping the observation with a Went_Id
and after that, based on teach_time to take those with less days so that estimates are
lower in the final analysis and not overestimated."

xref4 <- xref3 %>% # Dropping 38 observations
  arrange(Went_Id, time_teach) %>%
  distinct(Teacher_ID, end_date_ed, .keep_all = TRUE)

####### Remaining Teacher_ID duplicates ########
dupl <- xref4[duplicated(xref4[c(1)]),] # 248 remaining duplicates

"For this, I want to keep those duplicates where it is the same teacher, but with
no overlapping period of time as a teacher. These would be teachers that taught, left
then came back to teaching. For these teachers, I will create a new variable that sums
these separate periods to give a total teaching time. For observations with overlapping
time periods, there is not a logical way to choose between the observations so I will
drop those observations."

dupl1 <- xref4[duplicated(xref4[c(1)]),]
dupl2 <- xref4[duplicated(xref4[c(1)], fromLast = TRUE),]
dupl <- rbind(dupl1, dupl2) # Looking at the duplicate observations 
dupl <- arrange(dupl, Teacher_ID, start_date_ed)

# Dropping those observations where the end date of one overlaps with the start date of another
xref5 <- xref4 %>%
  group_by(Teacher_ID) %>%
  arrange(start_date_ed) %>%
  mutate(diff = lead(as.Date(start_date_ed))-as.Date(end_date_ed)) %>%
  fill(diff)

xref5$diff[is.na(xref5$diff)] <- 0
xref5 <- xref5 %>% filter(diff >= 0) # Dropping the problematic observations where start and end date overlap

dupl <- xref5[duplicated(xref5[c(1)]),] 

"
There are still 179 duplicate observations. These should be those teachers that left 
teaching and came back or transferred schools and have an end date for the first
school and a start date for the second that are after one another. Since the analysis
is for length of stay by teachers, I will create an aggregate of the total time each
teacher spent as a teacher, regardless of whether they came and left teaching and then
drop all remaining duplicates.
"
# New variable for total teaching time
xref5 <- xref5 %>%
  group_by(Teacher_ID) %>%
  mutate(
    tot_time_teach = sum(time_teach, na.rm=TRUE)
  )

# Dropping remaining duplicates
xref_final <- xref5 %>% # Dropping remaining 179 duplicate observations
  arrange(Teacher_ID, Went_Id) %>%
  distinct(Teacher_ID, .keep_all = TRUE)

########################### Merging xref & teach ###########################
############################################################################

length(unique(xref_final$Teacher_ID)) # All observations are unique
length(unique(teach$Teacher_ID)) # All observations are unique in teach

"
In joining xref_final and teach, only unique observations from xref_final matter
since that dataframe is the one that can be used to tease out unique observations
in general.
"

df <- inner_join(xref_final, teach, by = "Teacher_ID")

####################### Analysis of TFA differences ########################
############################################################################

# Filling in NULL values for ATP
df$ATP_ID[df$ATP_ID == 'NULL'] <- NA

# Dropping observations from before 1989, when TFA was founded and the 19 remaining obs that have a missing start date from when I imputed 1899 dates as missing
df <- df %>% filter(as.Date(start_date_ed) > '1988-12-31')

# Changing ATP_ID to word form
df$ATP_ID_full = factor(df$ATP_ID,
                         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), 
                         labels = c("None", "Teach for America", "NYC Teaching Fellow", "Washington DC Teaching Fellow", 
                                    "Troops to Teachers", "Academy for Urban School Leadership", "Baltimore Teaching Residency",
                                    "Bay Area Teacher Training Institute", "Boston Teacher Residency Program", "Chicago Teaching Fellows",
                                    "Indianapolis Teaching Fellows", "Memphis Teaching Fellows", "Miami Teaching Fellows",
                                    "Oakland Teaching Fellows", "Prince Georges County Teaching Fellows", "Philadelphia Teaching Fellows",
                                    "Texas Teaching Fellows", "Other", "Unknown", "Teaching Excellence"))

"
In looking at differences between TFA alums and other KIPP teachers, it's important
to differentiate between teachers that are still teaching and those that have left.
"
# Assessing for teachers no longer teaching
ATP_done_teaching <- df %>%
  filter(still_teach == 0) %>%  # Limiting to those done teaching
  group_by(ATP_ID_full) %>% # Grouping based on ATP
  summarise(obs = length(Teacher_ID), mean = mean(tot_time_teach, na.rm = TRUE), sd = sd(tot_time_teach, na.rm=TRUE),
            stderr = sd(tot_time_teach, na.rm=TRUE)/sqrt(length(Teacher_ID)), median = median(tot_time_teach, na.rm = TRUE)) # Summary statistics

# Graphing the above
ggplot(ATP_done_teaching, aes(y = mean, x = ATP_ID_full))+
  geom_point(color = "dodgerblue")+
  geom_point(aes(y = 753.8176, x = (ATP_ID_full = "Teach for America"), color = "darkorange"), show.legend = FALSE)+
  geom_errorbar(aes(ymin=mean-2*(stderr), ymax=mean+2*(stderr)), color = "dodgerblue")+
  geom_errorbar(aes(x = (ATP_ID_full = "Teach for America"), ymin=753.8176-2*12.58730, ymax=753.8176+2*12.58730), color = "darkorange")+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Average Total Number of Days Teaching for Former KIPP Teachers",
       y = "Avg Number of Days Teaching", x = "ATP Background")

# Boxplot of information. Doesn't appear too informative in this context
ggplot(df, aes(x = ATP_ID_full, y = tot_time_teach))+
  geom_boxplot()+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Median Number of Days Teaching for Former KIPP Teachers",
       y = "Median Number of Days Teaching", x = "ATP Background")

# Getting the mean, sd, and standard error teaching time for non-TFA alums
s <- df %>%
  filter(ATP_ID != 1 & still_teach == 0) %>%
  summarise(mean = mean(tot_time_teach, na.rm = TRUE))
mean(s$mean)
sd(s$mean)
sd(s$mean)/sqrt(length(s$Teacher_ID))

# Graphing TFA alums against all others
df$TFA <- (factor(with(df, ifelse((ATP_ID == 1), "TFA", "Not TFA"))))

s <- df %>%
  filter(still_teach == 0) %>%
  group_by(TFA) %>%
  summarise(mean = mean(tot_time_teach, na.rm = TRUE), sd = sd(tot_time_teach, na.rm = TRUE), stderr = sd(tot_time_teach, na.rm = TRUE)/sqrt(length(Teacher_ID)))

ggplot(s)+
  geom_point(aes(y = 753.8176, x = (TFA='TFA')), color = "dodgerblue")+
  geom_point(aes(y = 712.7368, x = (TFA='Not TFA'), color = "darkorange"), show.legend = FALSE)+
  geom_errorbar(aes(x = (TFA='Not TFA'), ymin=712.7368-2*10.83, ymax=712.7368+2*10.83), color = "darkorange")+
  geom_errorbar(aes(x = (TFA='TFA'), ymin=753.8176-2*12.58, ymax=753.8176+2*12.58), color = "dodgerblue")+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Average Total Number of Days Teaching for Former KIPP Teachers",
       y = "Avg Total Number of Days Teaching", x = "ATP Status")

# Assessing for teachers still teaching
ATP_still_teaching <- df %>%
  filter(still_teach == 1) %>%  # Limiting to those still teaching
  group_by(ATP_ID_full) %>% # Grouping based on ATP
  summarise(obs = length(Teacher_ID), mean = mean(tot_time_teach, na.rm = TRUE), sd = sd(tot_time_teach, na.rm=TRUE),
            stderr = sd(tot_time_teach, na.rm=TRUE)/sqrt(length(Teacher_ID)), median = median(tot_time_teach, na.rm = TRUE)) # Summary statistics

# Graphing the above
ggplot(ATP_still_teaching, aes(y = mean, x = ATP_ID_full))+
  geom_point(color = "dodgerblue")+
  geom_point(aes(y = 1102.8409, x = (ATP_ID_full = "Teach for America"), color = "darkorange"), show.legend = FALSE)+
  geom_errorbar(aes(ymin=mean-(2*stderr), ymax=mean+(2*stderr)), color = "dodgerblue")+
  geom_errorbar(aes(x = (ATP_ID_full = "Teach for America"), ymin=1102.8409-2*20.905296, ymax=1102.8409+2*20.905296), color = "darkorange")+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Average Total Number of Days Teaching for Current KIPP Teachers",
       y = "Avg Total Number of Days Teaching", x = "ATP Background")


###################### Analysis of gender differences ######################
############################################################################

# Making the gender variable binary
df$Gender[df$Gender == "NULL"] <- NA
df$female <- as.numeric(as.character(factor(with(df, ifelse((Gender == 'Female'), 1, 0)))))

# Getting summary statistics for women and men
s <- df %>%
  filter(still_teach == 0) %>%
  group_by(female) %>%
  summarise(obs = length(Teacher_ID), mean = mean(tot_time_teach, na.rm = TRUE), 
            std_dev = sd(tot_time_teach, na.rm = TRUE), stderr = sd(tot_time_teach, na.rm=TRUE)/sqrt(length(Teacher_ID)),
            median = median(tot_time_teach))

# Boxplot of gender and time teaching to show the overlap in general
ggplot(df, aes(x = Gender, y = tot_time_teach))+
  geom_boxplot()+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Median Number of Days Teaching for Former KIPP Teachers",
       y = "Number of Days Teaching", x = "Gender")

# Breaking apart gender differences based on ATP
s <- df %>%
  filter(still_teach == 0) %>%
  group_by(ATP_ID_full, female) %>%
  summarise(obs = length(Teacher_ID), mean = mean(tot_time_teach, na.rm = TRUE), 
            std_dev = sd(tot_time_teach, na.rm = TRUE), stderr = sd(tot_time_teach, na.rm=TRUE)/sqrt(length(Teacher_ID)),
            median = median(tot_time_teach))

# Graphing this breakdown
ggplot(s, aes(y = mean, x = ATP_ID_full, color = factor(female)))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-(2*stderr), ymax=mean+(2*stderr), color = factor(female)))+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Average Number of Days Teaching at KIPP by Program and Gender",
      y = "Avg Total Number of Days Teaching", x = "ATP Background")

# Pulling out averages for teaching between men and women and TFA and non-TFA
s <- df %>%
  filter(still_teach == 0) %>%
  group_by(TFA, female) %>%
  summarise(obs = length(Teacher_ID), mean = mean(tot_time_teach, na.rm = TRUE), 
            sd = sd(tot_time_teach, na.rm = TRUE), stderr = sd(tot_time_teach, na.rm=TRUE)/sqrt(length(Teacher_ID)),
            median(tot_time_teach))

# Graphing this breakdown
ggplot(s, aes(y = mean, x = TFA, color = factor(female)))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-(2*stderr), ymax=mean+(2*stderr), color = factor(female)))+
  coord_flip()+
  theme(panel.background = element_blank())+
  labs(title = "Average Number of Days Teaching at KIPP by TFA Status and Gender",
       y = "Avg Total Number of Days Teaching", x = "ATP Background")


###################### Analysis of two year trend ##########################
############################################################################

# Graphing average length of tenure to show most teachers leave at whole years
ggplot(df) +
  geom_histogram(aes(x = tot_time_teach, label=..count..), binwidth = 50)+ # Shows the bunching that you can see when sorting the data, very few outliers beyond 400 days
  coord_cartesian(xlim = c(0, 1600))+
  labs(title = "Number of Days Teaching at KIPP for all teachers",
       y = "Number of Teachers", x = "Number of Days Teaching at KIPP (bins are 50 days)")

# Creating a variable for if a teacher stayed at KIPP for more than 800 days
df$two_yrs <- (factor(with(df, ifelse((tot_time_teach >= 800), 1, 0))))
df$two_yrs <- as.numeric(as.character(df$two_yrs))

'Sample is all former teachers and current teachers over 800 days.'
s <- df %>%
  filter(still_teach == 0 | (tot_time_teach >= 800))

'Probability that a teacher stays over 800 days by (current and former teachers over
800 days) / (current teachers over 800 days + all former teachers)'
mean(s$two_yrs) # Overall mean from the subset sample

mean(subset(s, still_teach == 0)$two_yrs) # Mean for former teachers
mean(subset(df, still_teach == 1)$two_yrs) # Mean for current teachers

# Segmented by TFA status and gender
mean(subset(s, TFA == 'TFA')$two_yrs) # Mean for TFA teachers
mean(subset(s, TFA == 'Not TFA')$two_yrs) # Mean for non-TFA teachers

mean(subset(s, TFA == 'TFA' & female == '1')$two_yrs) # Mean for female TFA teachers
mean(subset(s, TFA == 'TFA' & female == '0')$two_yrs) # Mean for male TFA teachers

mean(subset(s, TFA == 'Not TFA' & female == '1')$two_yrs) # Mean for female non-TFA teachers
mean(subset(s, TFA == 'Not TFA' & female == '0')$two_yrs) # Mean for male non-TFA teachers

'Regress two_yrs = TFA to test whether TFA made it more likely a teacher teaches
at KIPP for more than 800 days.'
s$TFA_dum <- (factor(with(s, ifelse((TFA == 'TFA'), 1, 0))))
s$TFA_dum <- as.numeric(as.character(s$TFA_dum))

# Model of TFA status on likelihood of lasting more than 800 days 
model1 <- lm(two_yrs ~ TFA_dum, s) 
summary(model1)

model2 <- lm(two_yrs ~ TFA_dum + female, s)
summary(model2)

model3 <- lm(two_yrs ~ TFA_dum + female + Prev_Years_Teaching, s)
summary(model3)






