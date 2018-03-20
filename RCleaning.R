df <- read.csv('C:/r_tutorial/NHS-99M001X-E-2011-pumf-individuals_F1.csv', header = TRUE)

#Accessing columns in a data frame
mean(df$TOTINC)

#Access colums directly, attach a data frame
attach(df)
mean(TOTINC)

#Subsetting the data
myvars <- c("AGEGRP", "TOTINC", "WEIGHT")
new_data_frame <- df[myvars]

#Select 1st row and columns from index 5 to 10 
new_data_frame<-df[1,5:10]

#Select first 5 observations(rows)
new_data_frame<-df[1:5,]

#Select first 5 observations & first 3 columns
new_data_frame<-df[1:5,1:3]

#Select all females that earn more than 35,000
new_data_frame<-df[which(df$SEX=='1' & df$TOTINC > 35000),]


#Joining rows
nhs_subset2<-df[11:21,1:3]
nhs_subset1<-df[1:10,1:3]

nhs_subset3<-rbind(nhs_subset1,nhs_subset2)

#Joining columns
nhs_subset4 <- df[1:20, 1:3]
nhs_subset5 <- df[1:20, 4:6]
nhs_subset6 <- cbind(nhs_subset4, nhs_subset5)

#Transposing
nhs_subset1.transpose <- as.data.frame(t(nhs_subset1))

#tapply
tapply(TOTINC, SEX, mean) 

#Aggregate
aggregate(TOTINC~SEX,df, mean)

aggregate(TOTINC~SEX+PR, df, mean)