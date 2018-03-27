#install.packages("XML",dependecies = TRUE) #Install the XML packages

doc<-xmlParse("C:\\TEI XML\\XML reviewed\\PN970_A1_C45_1800z.xml")
xmldf <- xmlToDataFrame(nodes = getNodeSet(doc, "//ic:iconClass"))
View(xmldf)

wordfreq<-table(unlist(strsplit(tolower(xmldf$desc), " ")))
View(wordfreq)

#get all the filenames with their full paths form the directory where the XML files are stored
myFiles <- list.files(path="C:\\TEI XML\\XML reviewed",pattern = "*.xml",full.names = TRUE)

#Iterate over all the XML files and parse them
myDB <- lapply(myFiles, xmlParse)

XMLDFs <- data.frame()

#Iterate over the parsed XML files, to only select the iconclass tags
#Convert the name and desc tag into a data frame
for(i in 1:length(myDB)){
    tempXML<-xmlToDataFrame(nodes = getNodeSet(myDB[[i]], "//ic:iconClass"))
    if(nrow(tempXML))
    {
      #Keep binding the rows to create 1 long data frame instead of separate short ones
      XMLDFs <- rbind(XMLDFs,tempXML[,c(1,2)]) 
    }
}

#COnvert factor to character
XMLDFs[] <- lapply(XMLDFs, as.character)
#install.packages("tm")
library(tm)

#Use tm library to remove punctuations, strip whitespaces, remove numbers and remove stopwords
XMLDFs$desc<-(removePunctuation(XMLDFs$desc))
XMLDFs$desc<-(stripWhitespace(XMLDFs$desc))
XMLDFs$desc<-(removeNumbers(XMLDFs$desc))
XMLDFs$desc<-(removeWords(XMLDFs$desc,stopwords("en")))

#Remove blank entries
XMLDFs<-XMLDFs[XMLDFs$desc!="",]

#Create a word count
wordfreq_desc<-table(unlist(strsplit(tolower(XMLDFs$desc), " ")))

