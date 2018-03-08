#Installing the packages
#install.packages("ggplot2",dependecies =TRUE)
#install.packages("devtools",dependecies = TRUE)
#install.packages("jtools",dependecies = TRUE)
#devtools::install_github("dgrtwo/gganimate")
  #Install ImageMagick from http://www.imagemagick.org/script/download.php
  #While installing change application path to C (IMPORTANT)
  #Also, select - install legacy tools
#install.packages("plotly",dependencies = TRUE)

#Set this turn off the scientifc notations
options(scipen = 999)

#Load all the packages
library(ggplot2)
library(scales)
library(plotly)
library(gganimate)

#NHS DATA SET
#Select Total Income & Gross Rent Province wise
NHS.99M001X.E.2011.pumf.individuals_F1 <- read.csv("C:/Users/avaidya/NHS-99M001X-E-2011-pumf-individuals_F1.csv", header=FALSE, row.names=1)
NHS_subset <- NHS.99M001X.E.2011.pumf.individuals_F1[,c("PR","TOTINC","GROSRT")]

#Remove Missing Values & Aggregate TOTINC on GROSRT
NHS_subset.agg<- aggregate(cbind(TOTINC,GROSRT)~PR,NHS_subset[!(NHS_subset$TOTINC==9999999 | NHS_subset$GROSRT==9999 | NHS_subset$GROSRT==8888),],mean)
NHS_subset.agg$ProvinceName <- c("Newfoundland & Labrador","Prince Edward Island","Nova Scotia","New Brunswick","Quebec","Ontario","Manitoba","Saskatchewn","Alberta","British Columbia","Northern Canada")

#Plot a bar chart of PR vs TOTINC stacked with GROSS rent
#Also add value lables to the bars
ggplot() + geom_bar(aes(y = TOTINC, x = factor(ProvinceName), fill = GROSRT),data = NHS_subset.agg,stat = "identity",position = "stack") + geom_text(data = NHS_subset.agg,aes(x = factor(ProvinceName),y = TOTINC,label=paste0(format(round(GROSRT, 2), nsmall = 2))),size = 3.5) + scale_x_discrete(labels = abbreviate)
#Flipping Coordinates
ggplot() + geom_bar(aes(y = TOTINC, x = factor(ProvinceName), fill = GROSRT),data = NHS_subset.agg,stat = "identity",position = "stack") + geom_text(data = NHS_subset.agg,aes(x = factor(ProvinceName),y = TOTINC,label=paste0(format(round(GROSRT, 2), nsmall = 2))),size = 3.5) + scale_x_discrete(labels = abbreviate) + coord_flip()

#Select PR & HDGREE
NHS_subset.degreePR<-NHS.99M001X.E.2011.pumf.individuals_F1[!(NHS.99M001X.E.2011.pumf.individuals_F1$HDGREE==88 | NHS.99M001X.E.2011.pumf.individuals_F1$HDGREE==99),c("PR","HDGREE")]
#Find number of Observations for each HDGREE province wise
NHS_subset.HdegreePRwise<-table(NHS_subset.degreePR)

#Select PR & HDGREE & TOTINC
temp<-(NHS.99M001X.E.2011.pumf.individuals_F1[!(NHS.99M001X.E.2011.pumf.individuals_F1$HDGREE==88 | NHS.99M001X.E.2011.pumf.individuals_F1$HDGREE==99),c("PR","HDGREE","TOTINC")])
#Aggregate TOTINC on PR and HDGREE to find out total income for each hdgree in each province
temp.try <- aggregate(TOTINC ~ PR + HDGREE, temp, mean)

#Merge
NHS_subset.HdegreePRTOTINC <- merge(NHS_subset.HdegreePRwise, temp.try, all.y=TRUE)
g<-ggplot(NHS_subset.HdegreePRTOTINC, aes(x=HDGREE, y=TOTINC, size=Freq, frame=PR, color = HDGREE)) + geom_point() + scale_y_log10() + labs(x="Higher Education Degree", y="Total Income", title ="Total Income by Higher Education Degree (Province wise)")
gganimate(g, interval = 1)

#Jitter Plot
#Size of the circles is determined by the Frequency
ggplot(NHS_subset.HdegreePRTOTINC, aes(x=HDGREE, y=TOTINC)) + geom_jitter(aes(col=PR, size=Freq))  + labs(x="Higher Education Degree", y="Total Income", title ="Province Wise Total Income by Higher Education Degree (Frequency wise)")

#GDP Dataset

#Read the ForecastGDP dataset
df_forecastgdp <- read.csv(file = "C:\\Data\\Ajinkya\\RealGDP.csv",header = TRUE,strip.white = TRUE,stringsAsFactors = FALSE)

#Subset the annual data for Canada, Mexico & USA
df_forecastgdp.subset<- subset(df_forecastgdp, subset = ((LOCATION == "MEX" & FREQUENCY =="A") | (LOCATION == "CAN" & FREQUENCY =="A") | (LOCATION == "USA" & FREQUENCY =="A")), select = c(LOCATION, TIME, Value))
#Replace the year with it's abbvreviated form
df_forecastgdp.subset$TIME<-gsub('^19',"'",df_forecastgdp.subset$TIME)
df_forecastgdp.subset$TIME<-gsub('^20',"'",df_forecastgdp.subset$TIME)
df_forecastgdp.subset$TIME <- factor(df_forecastgdp.subset$TIME, levels = df_forecastgdp.subset$TIME)
#Plot Forecasted GDP country wise
ggplot(data = df_forecastgdp.subset,aes(x = TIME, y=Value, group = LOCATION)) + geom_line(aes(color = LOCATION)) + geom_point(aes(color = LOCATION)) + theme(legend.position = "bottom")

#Read the Real GDP Dataset
df_gdp <- read.csv(file = "C:\\Data\\Ajinkya\\GDP.csv",header = TRUE,strip.white = TRUE,stringsAsFactors = FALSE)
#Subset the data for Canada, Mexico & USA and select only the Million US Dollars  
df_gdp.subset_MLNUSD <- subset(df_gdp, subset = ((ï..LOCATION == "MEX" & MEASURE =="MLN_USD") | (ï..LOCATION == "CAN" & MEASURE =="MLN_USD") | (ï..LOCATION == "USA" & MEASURE =="MLN_USD")), select = c(ï..LOCATION, TIME, Value))
colnames(df_gdp.subset_MLNUSD) <- c("LOCATION","TIME","Value")
#Subset the data for Canada, Mexico & USA and select only the US Dollars/Capita
df_gdp.subset_USDCAP <- subset(df_gdp, subset = ((ï..LOCATION == "MEX" & MEASURE =="USD_CAP") | (ï..LOCATION == "CAN" & MEASURE =="USD_CAP") | (ï..LOCATION == "USA" & MEASURE =="USD_CAP")), select = c(ï..LOCATION, TIME, Value))
colnames(df_gdp.subset_USDCAP) <- c("LOCATION","TIME","Value")


#Plot countryiwse GDP in month MLNUSD and USDCAP.
ggplot(data = df_gdp.subset_MLNUSD,aes(x = TIME, y=Value, group = LOCATION)) + geom_line(aes(color = LOCATION)) + geom_point(aes(color = LOCATION)) + theme(legend.position = "bottom")
ggplot(data = df_gdp.subset_USDCAP,aes(x = TIME, y=Value, group = LOCATION)) + geom_line(aes(color = LOCATION)) + geom_point(aes(color = LOCATION)) + theme(legend.position = "bottom")

#Use Facets to show 3 different plots for Canada, Mexico & USA respectively
#Also, transform the numbers in their abbreviated form (eg. 30000 as 30K)
ggplot(data = df_gdp.subset_MLNUSD,aes(x = TIME, y=Value, group = LOCATION)) + geom_line(aes(color = LOCATION)) + geom_point(aes(color = LOCATION)) + theme(legend.position = "bottom") + facet_wrap( ~ LOCATION, ncol =3) + scale_y_continuous(name = "Million US Dollars",labels =unit_format(unit = 'M',scale = 1e-6)) + xlab("Years")
#Using APA theme to stucture the chart
ggplot(data = df_gdp.subset_USDCAP,aes(x = TIME, y=Value, group = LOCATION)) + geom_line(aes(color = LOCATION)) + geom_point(aes(color = LOCATION)) + theme(legend.position = "bottom") + facet_wrap( ~ LOCATION, ncol =3) + scale_y_continuous(name = "US Dollars/Capita",labels =unit_format(unit = 'K',scale = 1e-3)) + xlab("Years") +jtools::theme_apa(legend.pos = "bottom")

#Change the shape of the points to triangle & make the line graph thicker
newplot<-ggplot(data = df_gdp.subset_USDCAP,aes(x = TIME, y=Value, group = LOCATION)) + geom_line(aes(color = LOCATION),size =1.2) + geom_point(aes(color = LOCATION),shape =2) + theme(legend.position = "bottom") + facet_wrap( ~ LOCATION, ncol =3) + scale_y_continuous(name = "US Dollars/Capita",labels =unit_format(unit = 'K',scale = 1e-3)) + xlab("Years") +jtools::theme_apa(legend.pos = "bottom")
#Prints the plot in the plot Window
print(newplot)
#Saves the plot in the desired format
ggsave("C:\\myggplot.pdf", plot=newplot)

#For an interactive ggplot we use the ability of the package plotly.
#We can zoom, see actual values on mouse hover and similar abilites using plotly
ggplotly(newplot)

