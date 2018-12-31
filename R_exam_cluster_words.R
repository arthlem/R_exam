#---------------------------------------------GITHUB-------------------------------------------------
#Lors de changements
#1. git add .
#2. git commit -m "Le message a envoyer"
#3. git push

#Pour récupérer les changements
#git pull

#-------------------------------------------ONLINE RETAIL CSV-------------------------------------------------
#Libraries to install
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ade4")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("cluster")

#Libraries to load
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(ade4)
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
#General constants
turnoverByMonthScale <- 1/1000

#----A. IMPORT THE DATA----
Onlineretail <- read.csv2(file.choose(), header=TRUE, sep=";", dec=",", row.names = NULL, fileEncoding = "UTF-8-BOM") #Load CSV File

#Check if the data has been imported correctly
#View(Onlineretail)

#Match canceled orders and  corresponding orders
ProductsPerInvoice <- aggregate(Onlineretail$Quantity, by=list(Category=Onlineretail$InvoiceNo), FUN=sum)

#----B. DATA CLEANING----
#Removing the missing variables (CustomerID that are empty)
OnlineretailClean <- subset(Onlineretail, CustomerID != "")

#Finish cleaning dataset in one line 
# DataToRemove <- c('POST', 'D', 'C2', 'M', 'BANK CHARGES', 'PADS', 'DOT')
# 
# OnlineretailClean <- subset(OnlineretailClean, !(StockCode %in% DataToRemove))

#Remove POSTAGE
OnlineretailClean <- subset(OnlineretailClean, StockCode != "POST")

#Remove Discount
OnlineretailClean <- subset(OnlineretailClean, StockCode != "D")

#Remove CARRIAGE
OnlineretailClean <- subset(OnlineretailClean, StockCode != "C2")

#Remove Manual
OnlineretailClean <- subset(OnlineretailClean, StockCode != "M")

#Remove Bank Charges
OnlineretailClean <- subset(OnlineretailClean, StockCode != "BANK CHARGES")

#Remove PADS TO MATCH ALL CUSHIONS 
OnlineretailClean <- subset(OnlineretailClean, StockCode != "PADS")

#Remove DOTCOM POSTAGE
OnlineretailClean <- subset(OnlineretailClean, StockCode != "DOT")

#Remove Unit Price <= 0
OnlineretailClean <- subset(OnlineretailClean, UnitPrice > 0)

#Remove Quantity < 0
OnlineretailClean <- subset(OnlineretailClean, Quantity > 0)

#Remove Duplicates
OnlineretailUnique <- unique(OnlineretailClean)
dim(OnlineretailClean)-dim(OnlineretailUnique)

#Compute total revenue per row
#setDT(OnlineretailClean)[, TotalPrice := as.numeric(as.character(UnitPrice))*Quantity]

OnlineretailClean <- OnlineretailClean %>% 
  mutate(TotalPrice = Quantity*UnitPrice)

#Cluster on words usage
#uniqueDescriptionList <- unique(OnlineretailClean["Description"]) 

#uniqueDescriptionList <- subset(OnlineretailClean, unique(OnlineretailClean["Description"]))
uniqueDescriptionList <- OnlineretailClean[!duplicated(OnlineretailClean[,c('Description')]),]
uniqueDescriptionList <- uniqueDescriptionList[c("StockCode", "Description")]
#Putting a name for col description
names(uniqueDescriptionList) <- c("StockCode","Description")
#Put descriptions in a vector
descVector <- uniqueDescriptionList[["Description"]]
#Converting to corpus

dd<-data.frame(doc_id=uniqueDescriptionList[["StockCode"]],text=uniqueDescriptionList[["Description"]])
head(dd)

docs <- VCorpus(DataframeSource(dd))
docs <- VCorpus(VectorSource(descVector))
#docs <- VCorpus(VectorSource(uniqueDescriptionList))
summary(docs)
#docnames<-uniqueDescriptionList[["StockCode"]]

# Remove numbers
docs<-tm_map(docs, removeNumbers)
# Convert to lowercase
docs <- tm_map(docs,content_transformer(tolower))

#docs<-tm_map(docs, PlainTextDocument)

summary(docs)
#Let's remove some useless words like colors..
docs<-tm_map(docs, removeWords, c('pink', 'blue', 'tag', 'green', 'orange','red','set'))
#Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#Remove whitespaces
docs <- tm_map(docs, stripWhitespace)

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#No need for this command because language is english
#docs <- tm_map(docs,stemDocument)

#Create a term document matrix from the corpus
minTermFreq<- 25
maxTermFreq<-Inf
dtm <- DocumentTermMatrix(docs,control=list(wordLengths=c(3,Inf), bounds = list(global = c(minTermFreq, maxTermFreq))))
View(dtm)
#Clean dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ]

#Tests

dtm_tfxidf <- weightTfIdf(dtm)

dtmss <- removeSparseTerms(dtm, 0.99) # This makes a matrix that is only 25% empty space, maximum.   
inspect(dtmss) 

d <- dist(t(dtmss), method="euclidian")   
# Clustering:
fit <- hclust(d=d, method="complete")   
# Dendrogram:
plot(fit)
plot(fit, hang=-1)# Pour avoir tous les mots ? la m?me hauteur
# Groupons les mots en 10 groupes:
groups <- cutree(fit, k=2)   # "k=" defines the number of clusters you are using
# On ajoute les groupes au dendrogramme:
rect.hclust(fit, k=2, border="red") # draw dendogram with red borders around 
# the 10 clusters   

# Sizes of the clusters
table(groups)
# see the clusters
colnames(dtmss[,groups==1])
colnames(dtmss[,groups==5])
colnames(dtmss[,groups==3])


##Clustering
tdm.tfidf <- tm::weightTfIdf(dtm)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.99) 

m <- as.matrix(dtm_tfxidf)

norm_eucl <- function(m)
    m/apply(m, 1, function(x) sum(x^2)^.5)

m_norm <- norm_eucl(m)
results <- kmeans(m_norm, 2)

clusters <- 1:3

for(i in clusters){
  cat("Cluster ", i, ":", findFreqTerms(dtm_tfxidf[results$cluster== i,], lowfreq=25), "\n\n")
}

res.km <- eclust(uniqueDescriptionList["Description"], "kmeans", nstart = 25)
res.hc <- eclust(uniqueDescriptionList, "hclust")

fviz_nbclust(m_norm, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Frequencies of words
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
freq[ord]
#inspect least frequently occurring terms
freq[tail(ord)] 

#That done, let’s take get a list of terms that occur at least a  40 times in the entire corpus. This is easily done using the findFreqTerms() function as follows:
findFreqTerms(dtm,lowfreq=40)

#findAssocs(dtmr,"hair",0.1)

wf=data.frame(term=names(freq),occurrences=freq)
ggplot(subset(wf, freq>40), aes(term, occurrences)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#On construit un data.frame avec les fr?quences de mots:
mfw<-data.frame(word=names(freq), freq=freq)
# On s'assure que les mots sont dans l'ordre de fr?quences
mfw<-mfw[order(mfw[,2], decreasing=TRUE),]

# On fait un barplot des fr?quences
barplot(mfw[,2], names.arg = mfw[,1],las=2, horiz = TRUE)
# Heu il y a trop de mots, on n'y voit rien :D
# On cr?e un nouvel index pour ne retenir que les mots apparaissant au 
# moins 15 fois:
mfw2<-subset(mfw, freq[ord]>15)
# On refait le barplot en ajoutant une palette de couleur (avec la 
# fonction heatcolors())  contenant autant de couleur que le nombre 
# de lignes(de mots donc) retenues dan smfw2.
barplot(mfw2[,2], names.arg = mfw2[,1],las=2,
        horiz = TRUE, main="Most Frequent Words",
        col=heat.colors(dim(mfw2)[1]))

#setting the same seed each time ensures consistent look across clouds
set.seed(52)

#limit words by specifying min frequency and add color
wordcloud(names(freq),freq,min.freq=10,colors=brewer.pal(6,"Dark2"), random.order=FALSE)
