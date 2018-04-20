#install.packages("XML",dependecies = TRUE) #Install the XML packages

library(XML)
# doc<-xmlParse("C:\\Data\\Ajinkya\\mcgill-chapbooks-xml\\XML reviewed\\PN970_A1_C45_1800z.xml")
# xmldf <- xmlToDataFrame(nodes = getNodeSet(doc, "//ic:iconClass"))
# View(xmldf)

# wordfreq<-table(unlist(strsplit(tolower(xmldf$desc), " ")))
# View(wordfreq)

#get all the filenames with their full paths form the directory where the XML files are stored
myFiles <- list.files(path="C:\\Data\\Ajinkya\\mcgill-chapbooks-xml\\XML reviewed",pattern = "*.xml",full.names = TRUE)
myFileNames <- list.files(path="C:\\Data\\Ajinkya\\mcgill-chapbooks-xml\\XML reviewed",pattern = "*.xml",full.names = FALSE)

#Iterate over all the XML files and parse them
myDB <- lapply(myFiles, xmlParse)

allRoots <- lapply(myDB, xmlRoot) #Finding the root element of the XML Tree
allChild <- lapply(allRoots, xmlChildren) #Finding all the child nodes of the root node


XMLDFs <- data.frame()
typeXML <- data.frame()
j <- 1
noIconClass<-list()
#Iterate over the parsed XML files, to only select the iconclass tags
#Convert the name and desc tag into a data frame
for(i in 1:length(myDB)){
    tempXML<-xmlToDataFrame(nodes = getNodeSet(myDB[[i]], "//ic:iconClass"))
    #Get the type of each document
    typeXML <- rbind(typeXML,data.frame(filename = myFileNames[i],type = xmlGetAttr(allChild[[i]]$text,name = "type")))  
    if(nrow(tempXML))
    {
      #Keep binding the rows to create 1 long data frame instead of separate short ones
      tempXML <- tempXML[,c("name","desc")]
      tempXML$fileName <- myFileNames[i]
      XMLDFs <- rbind(XMLDFs,tempXML[,c(1,2,3)]) 
    }
    else
    {
      noIconClass[j] <- myFileNames[i]
      j <- j +1
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

#Use tm library to remove punctuations, strip whitespaces &remove stopwords
XMLDFs$name<-(removePunctuation(XMLDFs$name))
XMLDFs$name<-(stripWhitespace(XMLDFs$name))
XMLDFs$name<-(removeWords(XMLDFs$name,stopwords("en")))

#Remove blank entries
XMLDFs<-XMLDFs[XMLDFs$desc!="",]
XMLDFs<-XMLDFs[XMLDFs$name!="",]
#Create a word count
wordfreq_desc<-table(unlist(strsplit(tolower(XMLDFs$desc), " ")))
wordfreq_title<-table(unlist(strsplit(tolower(XMLDFs$name), " ")))

wordfreq_desc<-table(unlist((tolower(XMLDFs$desc))))

#write.csv("C:\\ParsedXMLContent.csv",x=XMLDFs)
#write.csv("C:\\WordCount.csv",x=wordfreq_desc)
typeFreqCount <- table(unlist(typeXML$type))

df.typeFreqCount <- as.data.frame(typeFreqCount,stringsAsFactors = FALSE)
df.idFreqCount <- as.data.frame(wordfreq_title,stringsAsFactors = FALSE)
df.descFreqCount <- as.data.frame(wordfreq_desc,stringsAsFactors = FALSE)
df.descFreqCount<-df.descFreqCount[!(df.descFreqCount$Var1 == ""),]

write.csv("C:\\Data\\Ajinkya\\iconClassID_Wordcount.csv",x=wordfreq_title)
write.csv("C:\\Data\\Ajinkya\\iconClassDesc_WordCount(Stopwords not removed).csv",x=wordfreq_desc)

write.csv("C:\\Data\\Ajinkya\\textType_FreqCount.csv",x=typeFreqCount)
write.csv("C:\\Data\\Ajinkya\\textType_perDocument.csv",x=typeXML)

library(wordcloud)

wordcloud(words = df.typeFreqCount$Var1,freq = df.typeFreqCount$Freq,min.freq = 7,colors = brewer.pal(8, "Dark2"),rot.per=0.35)
wordcloud(words = df.idFreqCount$Var1,freq = df.idFreqCount$Freq,min.freq = 7,colors = brewer.pal(8, "Dark2"),rot.per=0.35)
wordcloud(words = df.descFreqCount$Var1,freq = df.descFreqCount$Freq,min.freq = 10,colors = brewer.pal(8, "Dark2"),rot.per=0.35)