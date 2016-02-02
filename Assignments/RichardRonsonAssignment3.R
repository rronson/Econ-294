#Question 0
firstname<-"Richard"
lastname<-"Ronson"
studentID<-1505029
email<-"rronson@ucsc.edu"
assignment<-"Econ 294A Assignment 3"
print(paste(firstname,lastname))
print(studentID)
print(email)
print(assignment)
#####################################################################################
#Question 1
library(foreign)
df.ex<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
class(df.ex)
#Verify it is a data frame.
#####################################################################################
#Question 2
install.packages("dplyr")
library(dplyr)
require(dplyr)
df.ex.2<-df.ex %>%
  dplyr::filter(
    year==2013 & month==12
  )
print(nrow(df.ex.2))
#13,261 observations remain.
df.ex.2<-df.ex%>%
  dplyr::filter(
    year==2013 & (month==7|month==8|month==9)
  )
print(nrow(df.ex.2))
#39,657 observations remain.
#####################################################################################
#Question 3
df.ex.3a<-df.ex%>%
  dplyr::arrange(year,month)
View(df.ex.3a)
#Observations arranged in asscending order from January 1983 to December 2013.
#####################################################################################
#Question 4
df.ex.4a<-df.ex%>%
  dplyr::select(year,month,minsamp,hrlonglk,age)
View(df.ex.4a)
#There are 5 columns.
df.ex.4b<-df.ex%>%
  dplyr::select(year,month,starts_with("i"))
View(df.ex.4b)
#There are Four columns.
#Part C
df.ex.4c<-df.ex%>%
  dplyr::distinct(state)
print(df.ex.4c)
#I believe there is 51 distinct sets of values.
#####################################################################################
#Question 5
stndz<-function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}
#This function returns standard or Z-score.
nrmlz<-function(x){(x - min(x, na.rm = T))  /  (max(x, na.rm = T)-min(x, na.rm = T))}

df.ex.5a<-df.ex%>% 
  dplyr::mutate(
    rw.stndz = stndz(rw),rw.nrlmz = nrmlz(rw)
  )
View(df.ex.5a)
df.ex.5b<-df.ex%>% 
  dplyr::group_by(year,month) %>%
  mutate(
    rw.stndz = stndz(rw),
    rw.nrlmz = nrmlz(rw),
    count    = n()
  )
View(df.ex.5b)
# View January 2013 to confirm.
#####################################################################################
#Question 6
df.ex.6<-df.ex%>%
  dplyr::group_by(year,month,state)%>%
  summarise(
min.rw        = min(rw, na.rm=T), 
firstquant.rw = quantile(rw,0.25, na.rm=T),
median.rw     = median(rw, na.rm=T),
mean.rw       = mean(rw, na.rm=T),
thirdquant.rw = quantile(rw,0.75, na.rm=T),
max.rw        = max(rw, na.rm=T),
count         = n()
  )
#There are 4284 observations.

paste(print(max(df.ex.6$mean.rw)))
#The max mean real wage is 40.6258196447577.

df.ex.6b<-df.ex.6%>%
  dplyr::select(year,month,state,mean.rw)%>%
  filter(mean.rw>=40.62581)

print(df.ex.6b)
# 2013    12     DC 40.62582
#The highest year, month, state observation for mean real wage is 40.63 in December of 2013 in Washington DC . 
#####################################################################################
#Question 7 (Extra Credit)
df.ex$state <- factor(
  df.ex$state,
  levels(df.ex$state)[order(levels(df.ex$state))])
  
df.ex.7a<-df.ex%>%
  dplyr::arrange(year,month,desc(state))
View(df.ex.7a)