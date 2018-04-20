library(XML)
doc<-xmlParse("C:\\Data\\Ajinkya\\mcgill-chapbooks-xml\\XML reviewed\\PN970_A1_C45_1800z.xml")
root <- xmlRoot(doc) #Finding the root element of the XML Tree
child <- xmlChildren(root) #Finding all the child nodes of the root node
#child[1] ==> First child node - teiHeader
#child[2] ==> Second child node - text

mainNode <- child[[2]] #Only subsetting the <text> node as <teiHeader> is irrelevent
xmlChildren(mainNode)
xmlChildren(mainNode)[[1]][[2]]

front<-xmlChildren(mainNode)[[1]] #subsetting all the child nodes of the <front> tag
titlePage <- xmlChildren(front[[2]]) #subsetting the <titlePage> tag from the <front>

docTitle<-xmlValue(titlePage$docTitle) #Getting the value of the <docTitle> tag
epigraph<-xmlValue(titlePage$epigraph) #Getting the value of the <epigraph> tag


front_preface<-xmlChildren(front[[4]]) #Finding all children nodes of the preface

front_preface_title<-xmlValue(front_preface$head) # Getting the value of preface title
front_preface_text<-xmlValue(front_preface$p) #Getting all the text in the preface

text_body <- xmlChildren(mainNode)[[3]] #Subsetting the textbody from the main node
text_body_title <- xmlValue(text_body[[1]]) #Getting the title
text_body_text <- xmlValue(text_body[[3]]) #Getting the text