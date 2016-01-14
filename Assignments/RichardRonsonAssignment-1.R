# Question 0:
firstname<- "Richard"
lastname<- "Ronson"
print(paste(firstname,lastname))
studentID<-'1505029'
print(studentID)
source('/Users/Richard/Documents/UCSC Winter 2015/Econ 294-Assignment1.R')
#to check if done correctly
#*****************************************************************************
# Question 1:
library(foreign)
library(help=foreign)
df.dta<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")

df.csv<-read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")

df.td<-read.table(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
#The name assigned to this RData file is NHIS_2007_RData.
#*****************************************************************************
# Question 2:
write.table(df.csv,file="df.csv")
write.table(df.dta,file="df.dta")
write.table(df.td,file="df.td")
write.table(NHIS_2007_RData,file="NHIS_2007_RData")
#'saves files to hard drive
#'The data files sizes are as follows:
#'df.csv- 174 KB
#'df.dta- 174 KB
#'df.td- 261 KB
#'NHIS_2007_RData- 174 KB

#'df.csv,df.dta, and NHIS_2007_RDataare tied for the smallest size at 174 KB.
#'df.td has the largest size at 261 KB.
#'I believe what accounts for the variablilty in size is that the 
#'df.td file has 4786 observations while the others have 4785 and with it being a
#'text file it may not be as compressed at the other data files which may be why 
#'there is variablity in the sizes. Also their file comes with names already assigned to the data.
#*****************************************************************************
# Question 3:
typeof(NHIS_2007_RData)
class(NHIS_2007_RData)
#The object df.rdata consist of a list type of and a data.frame class of this data structure.
length(NHIS_2007_RData)
print(length(NHIS_2007_RData))
#9
dim(NHIS_2007_RData)
print(dim(NHIS_2007_RData))
#4785 by 9
nrow(NHIS_2007_RData)
print(nrow(NHIS_2007_RData))
#4785 rows
ncol(NHIS_2007_RData)
print(ncol(NHIS_2007_RData))
#9 colomns
summary(NHIS_2007_RData)
print(summary(NHIS_2007_RData))
#*****************************************************************************
# Question 4:
df<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(df)
#There are 1,119,754 observations with 30 variables.
summary(df$rw)
#' The variable rw has the follwing characteristics: min of 1.8, mean of 19.8, median of 15.9, 
#' max of 354.8, first quartile value of 10.7, third quartile of 24.4, and there are 521,279 total NA's.
#*****************************************************************************
# Question 5:
v<- c(1,2,3,4,5,6,7,4,NULL,NA)
length(v)
#'9
#'The number of values in the vector do not match the number of reported
#'length because they are not accounting for NULL as its viewed as nothing there exists.
mean(v,na.rm=TRUE) #while ignoring NA value
#The mean is 4.
#*****************************************************************************
# Question 6:
x<-matrix(1:9,3,3,byrow=TRUE)
print(x) #to verify it is correct.

# may show its transpose by using the following command t(x)
xt<-t(x)
print(xt) # to verify 

eigen(x)
# the eigen values of x are 1.611684e+01, -1.116844e+00, and -1.303678e-15.

y<- c(1,2,3,3,2,1,2,3,0)
y<-matrix(y,3,3,byrow=TRUE)
print(y) # to verify it is correct.

# may show its transpose by using the following command t(y)
yt<-t(y)
print(yt)

#Multiplying a matrix by the inverse matrix will equal identity matrix.
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
#install MASS package
# can also use solve()function to find inverse.
yi<-ginv(y) # This is how you may solve the inverse.
print(yi) #to verify

imatrix<-yi%*%y
print(imatrix) #to verify
#'the off diagonals or the numbers that are not =1 
#'are so close to zero I I believe the math was done correctly
#'as the result should be the identity matrix.
#*****************************************************************************
# Question 7:
diamonds <- read.table(header = TRUE, text = '
                       carat cut clarity price
                       5 "fair" "SI1" 850
                       2 "good" "I1" 450
                       0.5 "very good" "VI1" 450
                       1.5 "good" "VS1" NULL
                       5 "fair" "IF" 750
                       NA "Ideal" "VVS2" 980
                       3 "fair" NA 420
                       ')

diamonds
class(diamonds)
class(diamonds$carat)
class(diamonds$cut)
class(diamonds$clarity)
class(diamonds$price)

mean(as.numeric(diamonds$price)) #coerce factor into numeric, so you can calc mean
#calculates 3.28714 which I believe is incorrect.

diamonds.2 <- subset(diamonds,cut == "fair")
mean(as.numeric(diamonds.2$price))
#calculates 2.67, which I also believe is incorrect. 

diamonds.3 <- subset(diamonds, (cut == "good" |cut == "Ideal" | cut == "very good"))
mean(as.numeric(diamonds.3$price))
##calculates 3.75, which I also believe is incorrect. 

diamonds.4 <- subset(diamonds, carat > 2 & (cut == "Ideal" | cut == "very good"))
diamonds.4 # there are no observations that meet that criteria. 

#'Due to the values part 1-3 being incorrect I attempted to run it another way 
#'to calculate the true means.

carat<-c(5,2,0.5,5,NA,3)
cut<-c("fair","good","very good","fair","ideal","fair")
clarity<-c("SI1","I1","VI1","IF","VVS2","NA")
price<-c(850,450,450,750,980,420)
diamonds.o<-data.frame(carat,cut,clarity,price)
print(diamonds.o) #to verify it is correct.

#' I ommiited the NULL row so it will allow us to find the correct means as the price would 
#' not affect the variable of interest either way which was price.

mean(diamonds.o$price,na.rm=TRUE)
#The mean price is 650.00.

with(diamonds.o,mean(price[cut=="fair"]))
#The mean price of fair cut diamonds is 673.33.

with(diamonds.o,mean(price[cut=="good"| cut=="very good" |cut=="ideal"]))
#The mean price of fair,good, or ideal cut diamonds is 626.67.