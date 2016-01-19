#'---
#'title: Assignment 2
#'author: "Richard Ronson"
#'date: "Winter 2016"

# Question 0:

RichardRonsonAssignment2<- list(
  firstname="Richard",
  lastname="Ronson",
  email="rronson@ucsc.edu",
  studentID=1505029
)

#Question 1:
diamonds<-get (load(file=url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")))

RichardRonsonAssignment2$s1a <- nrow(diamonds)
print(RichardRonsonAssignment2$s1a)
# There are 7 observations.
RichardRonsonAssignment2$s1b <- ncol(diamonds)
print(RichardRonsonAssignment2$s1b)
# There are 4 columns. 
RichardRonsonAssignment2$s1c <- names(diamonds)
print(RichardRonsonAssignment2$s1c)
# The header names are: "carat", "cut", "clarity" and "price".
RichardRonsonAssignment2$s1d <- summary(diamonds$price)
print(RichardRonsonAssignment2$s1d)
#' The summary of the prices are:Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#                                420     450     600     650     825     980       1 

save(RichardRonsonAssignment2,file="Assignments/RichardRonsonAssignment2.RData")

#Question 2: (see solutions from assignment 2)
df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
  sep = "\t",
  header = T)
RichardRonsonAssignment2$s2a <- nrow(df.td)
print(RichardRonsonAssignment2$s2a)
# There are 4,785 observations.
RichardRonsonAssignment2$s2b<-ncol(df.td)
print(RichardRonsonAssignment2$s2b)
# There are 9 columns.
RichardRonsonAssignment2$s2c<-names(df.td)
print(RichardRonsonAssignment2$s2c)
# The header names are: "HHX","FMX", "FPX", "SEX", "BMI", "SLEEP", "educ", "height", "weight".
RichardRonsonAssignment2$s2d<-mean(df.td$weight)
print(RichardRonsonAssignment2$s2d)
# The mean weight is 266.2357.
RichardRonsonAssignment2$s2e<-median(df.td$weight)
print(RichardRonsonAssignment2$s2e)
# The median weight is 175.
df.hist<-hist(df.td$weight)
df.table<-table(df.td$weight)
View(df.table)
df.td$adj<-ifelse(df.td$weight>=996,NA,df.td$weight)
# This created new column adjusting for weights greater than or equal to 996 = NA.
RichardRonsonAssignment2$s2f<-mean(df.td$adj,na.rm=TRUE)
print(RichardRonsonAssignment2$s2f)
# The mean of the adjusted weight is 174.0741.
RichardRonsonAssignment2$s2g<-median(df.td$adj,na.rm=TRUE)
print(RichardRonsonAssignment2$s2g)
# The median of the adjusted weight is 170.
women<-subset(df.td,SEX==2)
View(women)
RichardRonsonAssignment2$s2h<-summary(women$adj,na.rm=TRUE)
print(RichardRonsonAssignment2$s2h)
# The summary of women weight is: Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#                                 100.0   130.0   150.0   158.2   178.0   274.0     329 
men<-subset(df.td,SEX==1)
RichardRonsonAssignment2$s2i<-summary(men$adj,na.rm=TRUE)
print(RichardRonsonAssignment2$s2i)
# The summary of men weight is: Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#                               128.0   169.0   187.0   192.8   212.0   298.0     207 

#Question 3: 
vec<- c(letters, LETTERS)
RichardRonsonAssignment2$s3a<-paste(vec[seq(2,52,2)],collapse="")
print(RichardRonsonAssignment2$s3a)
#"bdfhjlnprtvxzBDFHJLNPRTVXZ"

RichardRonsonAssignment2$s3b<-paste(vec[c(44,9,3)],collapse="")
print(RichardRonsonAssignment2$s3b)
#Ric for Richard

arr<-array(c(letters,LETTERS), dim=c(3,3,3))
print(arr)

RichardRonsonAssignment2$s3c<-paste(arr[1:3,1,2])
print(RichardRonsonAssignment2$s3c)
# "j" "k" "l"
RichardRonsonAssignment2$s3d<-paste(arr[2,2,1:3])
print(RichardRonsonAssignment2$s3d)
# "e" "n" "w"
RichardRonsonAssignment2$s3e<-paste(arr[3,3,2],arr[3,3,1],arr[3,1,1],sep="")
print(RichardRonsonAssignment2$s3e)
#ric for Richard 

#Question 4:
library(foreign)
org_example<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

sort(unique(org_example$year))
sort(unique(org_example$month))
sort(unique(org_example$educ))

RichardRonsonAssignment2$s4<-aggregate(org_example$rw,list(Year=org_example$year,Month=org_example$month,Education=org_example$educ),mean,na.rm=TRUE)
print(RichardRonsonAssignment2$s4)
dim(RichardRonsonAssignment2$s4)
#dimensions are 420 by 4. x= average rw.