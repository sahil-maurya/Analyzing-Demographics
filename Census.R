library(readr)
library(ggplot2)
library(dplyr)
library(readr)
library(psych)
library(readr)
library(gtsummary)
library(qwraps2)
library(MASS)
library(broom)
library(purrr)
library(readr)

library(readr)

dataframe <- read_csv("Downloads/Module4_dataset.csv", 
                      col_types = cols(ID = col_number(), YEAR = col_number(), 
                                       YEAR_OF_BIRTH = col_number(), C1DOB_Y = col_number(), 
                                       FAMSIZE_ = col_number(), TNFI_ = col_number(), 
                                       JOBSNUM_ = col_number(), INCOME_ = col_number(), 
                                       INCOME_MAX = col_number()))
View(dataframe)

# ------------Data cleaning and finding outliers------------
boxplot(dataframe$INCOME_, plot=FALSE)$out
 outliers <- boxplot(dataframe$INCOME_, plot=FALSE)$out
 x<-dataframe
 dataframe<- x[-which(x$INCOME_ %in% outliers),]
boxplot(dataframe$INCOME_, plot=FALSE)$out
boxplot(dataframe$INCOME_MAX, plot=FALSE)$out
outliers <- boxplot(dataframe$INCOME_MAX, plot=FALSE)$out
x<-dataframe
dataframe<- x[-which(x$INCOME_MAX %in% outliers),]
boxplot(dataframe$INCOME_MAX, plot=FALSE)$out
boxplot(dataframe$FAMSIZE_, plot=FALSE)$out
# NO OUTLIER FOUND IN THE FOLLOWING
boxplot(dataframe$TNFI_, plot=FALSE)$out
 boxplot(dataframe$NET_WORTH_, plot=FALSE)$out
 boxplot(dataframe$FAMSIZE_, plot=FALSE)$out
 boxplot(dataframe$C1DOB_Y, plot=FALSE)$out


#-----Milestone 1
dataframe <- dataframe[!(is.na(dataframe$WHEN_IN_POVERTY) | dataframe$WHEN_IN_POVERTY==""),]
p <- ggplot(data.frame(dataframe$WHEN_IN_POVERTY), aes(x=dataframe$WHEN_IN_POVERTY)) + geom_bar(fill="#d4a9ce")
p + coord_cartesian(ylim = c(0,6500))

# mytable1 <- function(subset_data2){
#         Table1 <- psych::describe(subset_data2)
#         Table1 <- Table1 %>% select(n, mean, max, min, sd)
#         Table1
#         write.csv(Table3, "Table3.csv")
# }
View(dataframe)
#Subsetting
subset_data <- dataframe[ which(dataframe$COUNTRY_OF_BIRTH=="IN THE US" & dataframe$SAMPLE_SEX=="FEMALE"), ]
subset_data
dataframe$COUNTRY_OF_BIRTH
Table2 <- psych::describe(subset_data)
Table2 <- Table2 %>% select(n, mean, max, min, sd)
Table2
write.csv(Table2, "Table2.csv")
getwd()
dataframe$MARSTAT_KEY_
subset_data2 <- dataframe[ which(dataframe$MARSTAT_KEY_=="1: 1  MARRIED" & dataframe$HAVING_HEALTHPLAN=="No"), ]
subset_data2
Table3 <- psych::describe(subset_data2)
Table3 <- Table3 %>% select(n, mean, max, min, sd)
Table3
write.csv(Table3, "Table3.csv")

dataframe$HAVING_HEALTHPLAN

mytable1 <- function(subset_data2){
        Table1 <- psych::describe(subset_data2)
        Table1 <- Table1 %>% select(n, mean, max, min, sd)
        Table1
        write.csv(Table3, "Table3.csv")
}
mytable1(subset_data2)
write.csv(Table3, "Table3.csv")

#psych::describe(subset_data)


boxplot(dataframe$NET_WORTH_,main = "Boxplot for NET_WORTH_",
        xlab= "Net_WORTH",
        col="BLUE",
        border = "brown",
        breaks=10,
        ylim = c(-2000,150000),
        horizontal = TRUE,ylab=NA)

# Cut the screen in 4 parts
par(mfrow=c(2,2))

boxplot(dataframe$INCOME_~dataframe$YEAR_OF_BIRTH,
        data=airquality,
        main="Different boxplots for INCOME VS Year Of Birth",
        xlab="YEAR_OF_BIRTH",
        ylab="INCOME",
        col=rainbow(8),
        border="brown"
)

# Find analytics of employment distribution per family size and the total net Family income
status <- dataframe$EMP_STATUS_
ggplot(dataframe, aes(x=dataframe$FAMSIZE_, y=dataframe$TNFI_, main="Scatter plot for Employment Distribution" )) + geom_point(aes(colour = factor(status)))


#---- Milestone 2---



#------question 1 -----
#Do people who have health plan or not have the same family size?
dataframe <- dataframe[!dataframe$HAVING_HEALTHPLAN == "-2", ]
dataframe <- dataframe[!dataframe$HAVING_HEALTHPLAN == "-3", ]
family <- dataframe$FAMSIZE_
healthplan <- dataframe$HAVING_HEALTHPLAN

#boxplot Health Plan taken vs Familiy size
boxplot(family~healthplan,
        data=dataframe,
        main="Different boxplots per healthplan choice",
        xlab="Health Plan taken",
        ylab="Familiy size",
        col=c("darkred", "lightblue")
)

#2 sample t test
HA_YES <- subset(dataframe, subset=(dataframe$HAVING_HEALTHPLAN=="Yes"))
HA_NO <- subset(dataframe, subset=(dataframe$HAVING_HEALTHPLAN=="No"))
test1 <- t.test(x=HA_YES$FAMSIZE_, y =HA_NO$FAMSIZE_,
                mu = 1 , paired = FALSE, var.equal = FALSE,
                conf.level = 0.95)
test1
table_test1 <- map_df(list(test1), tidy)
table_test1

summary(HA_YES$FAMSIZE_)
summary(HA_NO$FAMSIZE_)



#-----Question 2-----
t.test(dataframe$INCOME_MAX)

#boxplot MAX INCOME
boxplot(dataframe$INCOME_MAX,
        data=dataframe,
        main="MAX INCOME",
        col=c("lightblue"),
        border="black"
)




#-----Question 3-----
#Do people who have or have not been ever divored have different maximum income?
dataframe <- dataframe[!dataframe$EVER_DIVORCED_ == "-5", ]
divorced <- dataframe$EVER_DIVORCED_
max_income <- dataframe$INCOME_MAX


#boxplot Health Plan taken vs Familiy size
boxplot(max_income~divorced,
        data=dataframe,
        main="Different boxplots per history of divorce",
        xlab="Divorced ?",
        ylab="maximum income",
        col=c("darkred", "lightblue"),
        border="black"
)

#2 sample t test
Ev_divoredYes <-subset(dataframe, subset=(divorced=="Yes"))
Ev_divoredNo <-subset(dataframe, subset=(divorced=="No"))
test2 <- t.test(x=Ev_divoredYes$INCOME_MAX, y =Ev_divoredNo$INCOME_MAX,
                mu = 1 , paired = FALSE, var.equal = FALSE,
                conf.level = 0.95)

test2
table_test2 <- map_df(list(test2), tidy)
table_test2

summary(Ev_divoredYes$INCOME_MAX)
summary(Ev_divoredNo$INCOME_MAX)

#-----Question 4----
#Does the number of jobs differ for people belonging to Black and Hispanic race ?
dataframe <- dataframe[!dataframe$SAMPLE_RACE == "NON-BLACK, NON-HISPANIC", ]
sample_race <- dataframe$SAMPLE_RACE
num_jobs <- dataframe$JOBSNUM_

#boxplot Health Plan taken vs Familiy size
boxplot(num_jobs~sample_race,
        data=dataframe,
        main="Different boxplots as per sample race",
        xlab="Races",
        ylab="number of jobs",
        col=c("darkred", "lightblue"),
        border="black"
)

#2 sample t test
B_RACE <-subset(dataframe, subset=(sample_race=="BLACK"))
H_RACE <-subset(dataframe, subset=(sample_race=="HISPANIC"))
test3 <- t.test(x=B_RACE$JOBSNUM_, y =H_RACE$JOBSNUM_,
                mu = 1, paired = FALSE, var.equal = FALSE,
                conf.level = 0.95)

test3
table_test3 <- map_df(list(test3), tidy)
table_test3

summary(B_RACE$JOBSNUM_)
summary(H_RACE$JOBSNUM_)


#-------Final prroject ---
#Questtion 1: What is max net worth of 30% population is more than 35000?

t.test(dataframe$NET_WORTH_)
T1 <-t.test(dataframe$NET_WORTH_)
table_T1 <- map_df(list(testX), tidy)
table_T1
boxplot(dataframe$NET_WORTH_,main = "Boxplot for NET_WORTH_",
        xlab= "Net_WORTH",
        col="lightblue",
        border = "black",
        breaks=10,
        ylim = c(-2000,150000),
        horizontal = TRUE,ylab=NA)

# Question 2 : How year of birth is related to income?

testX <- t.test(x=dataframe$INCOME_, y =dataframe$YEAR_OF_BIRTH,
                mu = 1 , paired = FALSE, var.equal = FALSE,
                conf.level = 0.95)
testX
table_testX <- map_df(list(testX), tidy)
table_testX
summary(lm(data=dataframe, INCOME_~YEAR_OF_BIRTH))
Reg2 <-summary(lm(data=dataframe, INCOME_~YEAR_OF_BIRTH))
table_Reg2<- map_df(list(Reg2), tidy)
table_Reg2
write.csv(table_Reg2, "table_Reg2.csv")

ggplot(dataframe, aes(x=INCOME_, y=YEAR_OF_BIRTH)) + 
        geom_point()+
        geom_smooth(method=lm)

boxplot(dataframe$INCOME_~dataframe$YEAR_OF_BIRTH,
        data=airquality,
        main="Different boxplots for INCOME VS Year Of Birth",
        xlab="YEAR_OF_BIRTH",
        ylab="INCOME",
        col=terrain.colors(4),
        border="brown"
)
# Question 3: How is family size proportional to employment
#Find analytics of employment distribution per family size and the total net Family income

testY <- t.test(x=dataframe$FAMSIZE_, y=dataframe$TNFI_,
                mu = 1 , paired = FALSE, var.equal = FALSE,
                conf.level = 0.95)
testY
table_testY <- map_df(list(testY), tidy)
table_testY
summary(lm(data=dataframe, FAMSIZE_~TNFI_))
Reg1 <-summary(lm(data=dataframe, FAMSIZE_~TNFI_))
table_Reg1<- map_df(list(Reg1), tidy)
table_Reg1
write.csv(table_Reg1, "table_Reg1.csv")

ggplot(dataframe, aes(x=FAMSIZE_, y=TNFI_)) + 
        geom_point()+
        geom_smooth(method=lm)

status <- dataframe$EMP_STATUS_
ggplot(dataframe, aes(x=dataframe$FAMSIZE_, y=dataframe$TNFI_, main="Scatter plot for Employment Distribution" )) + geom_point(aes(colour = factor(status)))

