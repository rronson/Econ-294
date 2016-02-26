#Question 0
firstname<-"Richard"
lastname<-"Ronson"
studentID<-1505029
assignment<-"Econ 294 A  Assignment 5"
print(paste(firstname,lastname))
print(studentID)
print(assignment)
#############################################################################################
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(scales)
#Question 1

#Part A

plot1a<-ggplot(diamonds,aes(x=x*y*z,y=price))
part1a<-plot1a +scale_x_log10()+ scale_y_log10()+geom_point(aes(colour = clarity),alpha=0.2)+ aes(size=carat)
part1a

#Part B

plot1b <- ggplot(diamonds, aes(x=carat,y = ..density..))
part1b<-plot1b + geom_histogram(aes(fill = clarity),binwidth = 0.2)+facet_grid(cut~.)
part1b

#Part C

plot1c<-ggplot(diamonds,aes(x=cut,y=price))
part1c<-plot1c+ geom_violin()+geom_jitter(alpha=0.03)
part1c

#############################################################################################
#Question 3

library(foreign)
org_example <- read.dta("/Users/Richard/Documents/UCSC Winter 2016/Econ 217/Data/org_example.dta")

org_example <- org_example %>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  filter(!is.na(rw)) %>%
  tbl_df()

#Part A

plot3a<-ggplot(
  data = (
    org_example %>%group_by(year,month)%>%
      mutate(
        Median.RW = (median(rw)),
        qr1st=quantile(rw, probs = 0.25, na.rm = T, names = T, type = 7),
        qr3rd=quantile(rw, probs = 0.75 , na.rm = T, names = T, type = 7), 
        dec1st=quantile(rw, probs = 0.1, na.rm = T, names = T, type = 5) ,
        dec9th=quantile(rw, probs = 0.9, na.rm = T, names = T, type = 5),
        total = n()
      )  %>% select(date, Median.RW, qr1st, qr3rd, dec1st, dec9th, educ)),
  aes(
    x = date, 
    y = Median.RW,
  )) 

part3a<-plot3a+ geom_line()+ geom_ribbon(aes(ymin = qr1st, ymax = qr3rd), alpha=0.4)+geom_ribbon(aes(ymin = dec1st, ymax = dec9th), alpha=0.2) + ylim(0, 50)
part3a

#Part B

plot3b<-ggplot(
  data = (
    org_example %>%group_by(year,month,educ)%>%
      mutate(
        Median.RW = (median(rw))
      )
  ),
  aes(
    x = date, 
    y = Median.RW,
    colour = educ
  )
) 

part3b<-plot3b+geom_line()
part3b

#############################################################################################