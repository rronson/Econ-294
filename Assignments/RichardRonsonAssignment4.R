#Question 0
firstname<-"Richard"
lastname<-"Ronson"
studentID<-1505029
assignment<-"Econ 294A Assignment 4"
print(paste(firstname,lastname))
print(studentID)
print(assignment)
#############################################################################################
#Question 1
flights.df<-read.csv(file="https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv",stringsAsFactors=F)
planes.df<-read.csv(file="https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",stringsAsFactors=F)
weather.df<-read.csv(file="https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv",stringsAsFactors=F)
airports.df<-read.csv(file="https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv",stringsAsFactors=F)
#############################################################################################
#Question 2
as.Date(flights.df$date)
as.Date(weather.df$date)

#There are no date columns for airports.df and planes.df.
#############################################################################################
#Question 3

#Part A
flights.3a<-subset(flights.df,dest=="SFO"|dest=="OAK")

print(nrow(flights.2a))
#There are 3,508 observations.

#Part B
flights.3b<-subset(flights.df,dep_delay+arr_delay>=60)

print(nrow(flights.2b))
#There are 22,755 observations.

#Part C
flights.3c1<-subset(flights.df,arr_delay==2*dep_delay)
#all delays including zero(all types positive,negative, and zero).
flights.3c2<-subset(flights.df,arr_delay==2*dep_delay & arr_delay>0)
#all delays greater that 0 (actual delays).
flights.3c3<-subset(flights.df,arr_delay==2*dep_delay & arr_delay!=0)
#all delays that are not equl to zero(both positive and negative).

#I was not sure which you were looking for so I just did all cases.
print(nrow(flights.3c1))
#There are 7,058 observations. For all types of delays(positive,negative, and zero).
print(nrow(flights.3c2))
#There are 1,742 observations. For all actual delays (positive delays).
print(nrow(flights.3c3))
#There are 6,299 observations. For all delays excluding zero.
#############################################################################################
#Question 4
library(dplyr)
require(dplyr)

way.1<-flights.df%>%
  dplyr::select(starts_with("dep_"),starts_with("arr_"))

way.2<-flights.df%>%
  dplyr::select(ends_with("delay"))

way.3<-flights.df%>%
  dplyr::select(contains("delay"))

#############################################################################################
#Question 5
#Part A
part.5a<-flights.df%>%
  dplyr::arrange(desc(dep_delay))%>%
  select(dep_delay,flight) %>%
  head(n=5)

print(part.5a)
#Top 5 delayed flights.

#Part B
part.5b<-flights.df%>%
  dplyr::mutate(
    caughtup = dep_delay-arr_delay)%>%
    arrange(desc(caughtup))%>%
  select(caughtup,flight) %>%
  head(n=5)

print(part.5b)
#Top 5 flights that made up the most ground.

#############################################################################################
#Question 6

#Part A 

flights.df<-flights.df%>%
  dplyr::mutate(
    delta = dep_delay - arr_delay, speed = dist / (time / 60))

View(flights.df)  
#created delta and speed column.

part.6a<-flights.df%>%
  dplyr::arrange(desc(speed))%>%
  select(speed,flight) %>%
  head(n=5)

print(part.6a)
#Top 5 flight's speed.

#Part B

part.6b<-flights.df%>%
  dplyr::arrange(desc(delta))%>%
  select(delta,flight) %>%
  head(n=5)

print(part.6b)
#Top 5 flights that made up the most ground.

#Part C
part.6c<-flights.df%>%
  dplyr::arrange(delta)%>%
  select(delta,flight) %>%
  head(n=5)

print(part.6c)
#Top 5 flights that lost the most time.
#############################################################################################
#Question 7

#Part A

flights7.a<-flights.df%>%
  dplyr::group_by(carrier)%>%
summarise(
          min.d        = min(delta, na.rm=T), 
          firstquant.d = quantile(delta,0.25, na.rm=T),
          median.d     = median(delta, na.rm=T),
          mean.d       = mean(delta, na.rm=T),
          thirdquant.d = quantile(delta,0.75, na.rm=T),
          quant90th.d = quantile(delta,0.90, na.rm=T),
          max.d        = max(delta, na.rm=T),
          count         = n(),
         cancelled=sum(cancelled==1),
         percan=(((cancelled)/(count))*100)
          )

sumflights7.a<-flights7.a%>%
  dplyr::arrange(desc(percan))%>%
  select(carrier,percan)%>%
  head(n=5)

print(sumflights7.a)
#Top 5 carriers with highest relative cancellations of flights (in percentages).

#Part B
day_delay<-dplyr::filter(summarize(group_by(dplyr::filter(flights.df,!is.na(dep_delay)),
                                            date),
                                   delay=mean(dep_delay),
                                   n=n()
                                   ),
                         n>10
                         )

#Determines average departure delay and number of total delays for each date.

day_delay.7b<-flights.df%>%
  dplyr::filter(!is.na(dep_delay))%>%
      group_by(date)%>%
      summarise(
      delay=mean(dep_delay,na.rm=T),
      n=n())%>%
filter(n>10)

#recreated part 7.b using magrettr's %>% operator
      
cat("flights.df%>%
  dplyr::filter(!is.na(dep_delay))%>% #filtering out missing values
    group_by(date)%>%                 #group data by date
    summarise(                        #summarizing average dep_delay and total number of delays per date.
    delay=mean(dep_delay,na.rm=T),
    n=n())%>%
    filter(n>10)                   #filtering days that have greater than 10 delays"
    )
#prints explaination of code

#############################################################################################
#Question 8

#Part A

day_delay.8a<-day_delay%>%
  dplyr::mutate(diffdelay=delay-lag(delay)
    )

#Part B

day_delay.8b<-day_delay.8a%>%
  dplyr::arrange(desc(diffdelay))%>% 
  select(date,diffdelay)%>%
  head(n=5)

print(day_delay.8b)
#Top 5 days that has the largest increase in average depature delay from one day to the next.

#############################################################################################
#Question 9

#Part A

dest_delay.9<-flights.df%>%
  dplyr::filter(!is.na(arr_delay))%>%
  group_by(dest)%>%
  summarise(
    dest_delay=mean(arr_delay,na.rm=T),
    n=n())

airports.df.9<-airports.df%>%
  dplyr::select(dest=iata,name=airport,city,state,lat,long)


df.9a<-dest_delay.9%>% 
  dplyr::left_join(airports.df.9, by = "dest") 

df.9a2<-df.9a%>%
  dplyr::arrange(desc(dest_delay))%>% 
  select(dest_delay,city,state)%>%
  head(n=5)

print(df.9a2) 
#The Top 5 cities/states with the highest average arrival delay.

#Part B

df.9b<-dest_delay.9%>% 
  dplyr::inner_join(airports.df.9, by = "dest") 

nrow(df.9a)
nrow(df.9b)
#Their are two less observations when using the inner_join command than when using the left_join command.

#Part C

df.9c<-dest_delay.9%>% 
  dplyr::right_join(airports.df.9, by = "dest") 

nrow(df.9c)
#There are 3376 observations.
#Yes there are many NA's in this dest_delay because right join includes all observations in y 
# which is why there is NA's here and not from left join as it includes all observations in x.
#There are not matches for those locations.

#Part D

df.9d<-dest_delay.9%>% 
  dplyr::full_join(airports.df.9, by = "dest") 

nrow(df.9d)
#There are 3378 observations.
#Yes there are NA's because full_join includes all observations from both tables x and y.
#There are not matches for those locations which is why there is NA's in the table.
#############################################################################################
#Question 10

#Part A
install.packages("tidyr")
require(tidyr)

flights.df10<-flights.df%>%
  tidyr::separate(date,c("date","discard"),sep=" ")

#This will allow us to merge with the weather.df as date will now be formatted the same.

hourly_delay.10<-flights.df10%>%
  dplyr::filter(!is.na(dep_delay))%>%
  group_by(date,hour)%>%
  summarise(
    hourly_delay=mean(dep_delay,na.rm=T),
    n=n())
#Created new column and table.

df.10<-hourly_delay.10%>% 
  dplyr::left_join(weather.df, by = c("date", "hour") ) 
#Merged new table with weather.df

df.10.2<-df.10%>%
  dplyr::arrange(desc(hourly_delay))%>% 
  select(hourly_delay,5:16)%>%  
  head(n=5)

print(df.10.2)
#for all weather conditions
#Top 5 hourly delays and weather conditions they are associated with.

#Part B

df.10.2b<-df.10%>%
  dplyr::arrange(desc(hourly_delay))%>% 
  select(hourly_delay,conditions)%>%  
  head(n=5)

print(df.10.2b)
#just the variable "conditions".
#Top 5 hourly delays and weather conditions they are associated with.

#############################################################################################
#Question 11

#Part A

df<-data.frame(treament=c("a","b"),subject1=c(3,4),subject2=c(5,6))
df

df.11a<-df%>%
  tidyr::gather(df, treatment, subject1, subject2, na.rm = T) %>%
  separate(df,c("discard","subject"),sep="t")%>%
  dplyr::select(subject,treatment=treament,value=treatment) %>%
  arrange(subject)

print(df.11a)

#Part B  

df.b<-data.frame(subject=c(1,1,2,2),treatment=c("a","b","a","b"),value=c(3,4,5,6)) 
df.b

df.11b<-df.b%>%
  tidyr::spread(key=subject,value=value) %>%
  dplyr::rename(subject1=`1`,subject2=`2`)

print(df.11b)

#Part C 

df.c<-data.frame(subject=c(1,2,3,4),
                 demo=c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
                 value=c(3,4,5,6))
df.c

df.11c<-df.c%>%
  tidyr::separate(demo,c("sex","age","state"),sep="_")

print(df.11c)

#Part D
  
df.d<-data.frame(subject=c(1,2,3,4),
                  sex=c("f","f","m",NA),
                  age=c(11,55,65,NA),
                  city=c("DC","NY","WA",NA),
                  value=c(3,4,5,6))  
df.d

df.11d<-df.d%>%
  tidyr::unite(demo,...=sex, age, city,sep=".")
  
print(df.11d)  
#############################################################################################