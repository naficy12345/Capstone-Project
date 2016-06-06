library(tm)
library(stringi)
library(filehash) 
library(tau)
options(mc.cores=1)
setwd('C:/Shahrzad_Docs/PERSONAL_DOCUMENTS/COURSERA/CAPSTONE Project/Coursera-SwiftKey/final/en_US')

# Input to R from 3 data sources
input_twitter <- readLines('en_US.twitter.txt', encoding = 'UTF-8',skipNul = TRUE)
input_news <- readLines('en_US.news.txt', encoding = 'UTF-8',skipNul = TRUE)
input_blogs <- readLines('en_US.blogs.txt', encoding = 'UTF-8',skipNul = TRUE)

# Randomly sample 70 percent of each data source
set.seed(12345)
training_data <- c(input_twitter[sample(1:length(input_twitter),round(0.7*length(input_twitter)))],input_news[sample(1:length(input_news),round(0.7*length(input_news)))],input_blogs[sample(1:length(input_blogs),round(0.7*length(input_blogs)))])

training_data <- sapply(training_data, function(x) iconv(enc2utf8(x), sub = "byte"))

write(training_data,"./DB_DIR/training_data.txt")
 
# Data Cleansing
training_data_db <- PCorpus(DirSource("DB_DIR", encoding="UTF-8",mode="text"),
                    dbControl=list(dbName="training_data.db", dbType="DB1"))
training_data_db <- tm_map(training_data_db, content_transformer(stri_trans_tolower));dbInit("training_data.db")
training_data_db <- tm_map(training_data_db, removeWords, stopwords("en"));dbInit("training_data.db")
training_data_db <- tm_map(training_data_db, removePunctuation);dbInit("training_data.db") 
training_data_db <- tm_map(training_data_db, removeNumbers);dbInit("training_data.db")
training_data_db <- tm_map(training_data_db, stripWhitespace);dbInit("training_data.db")  
training_data_db <- tm_map(training_data_db, PlainTextDocument);dbInit("training_data.db")  

write(training_data_db[[1]][[1]],"./DB_DIR/training_data.txt")
training_data_db <- PCorpus(DirSource("DB_DIR", encoding="UTF-8",mode="text"),
                    dbControl=list(dbName="training_data_2.db", dbType="DB1"))
for (i in seq(training_data_db)) {
     training_data_db[[i]][[1]] <-gsub(" \'s","", training_data_db[[i]][[1]])
     training_data_db[[i]][[1]] <-gsub("<.*>","", training_data_db[[i]][[1]])
}
write(training_data_db[[1]][[1]],"./DB_DIR/cleaned_training_data.txt")

# Divide the clean data into 4 chunks to create the Tri gram data frame
trigram_db <- PCorpus(DirSource("DB_DIR", encoding="UTF-8",mode="text"),
                    dbControl=list(dbName="trigram.db", dbType="DB1"))


trigram_txt<-c(trigram_db[[1]][[1]]) 

trigram_txt_1<-trigram_txt[1:800000]
TriGram <- textcnt(trigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
TrigramDF<-data.frame(Tri = names(TriGram), counts = unclass(TriGram))
names(TrigramDF)<-c("Tri","counts")
write.csv (TrigramDF,"TriGramDF1.csv")

trigram_txt_1<-trigram_txt[800001:1600000]
TriGram <- textcnt(trigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
TrigramDF<-data.frame(Tri = names(TriGram), counts = unclass(TriGram))
names(TrigramDF)<-c("Tri","counts")
write.csv (TrigramDF,"TriGramDF2.csv")

trigram_txt_1<-trigram_txt[1600001:2000000]
TriGram <- textcnt(trigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
TrigramDF<-data.frame(Tri = names(TriGram), counts = unclass(TriGram))
names(TrigramDF)<-c("Tri","counts")
write.csv (TrigramDF,"TriGramDF3.csv")

trigram_txt_1<-trigram_txt[2000001:2335687]
TriGram <- textcnt(trigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
TrigramDF<-data.frame(Tri = names(TriGram), counts = unclass(TriGram))
names(TrigramDF)<-c("Tri","counts")
write.csv (TrigramDF,"TriGramDF4.csv")

# combining the 4 chunks to a data frame and find the frequencies and write the final csv file to disk 
# to be used by the next word application
df1 <- read.csv("TriGramDF1.csv", header=TRUE)
df2 <- read.csv("TriGramDF2.csv", header=TRUE)
df3 <- read.csv("TriGramDF3.csv", header=TRUE)
df4 <- read.csv("TriGramDF4.csv", header=TRUE)
trigramDF<-rbind(df1,df2,df3,df4)
trigramDF<-aggregate(trigramDF$counts,list(Tri=trigramDF$Tri),sum)
write.csv (trigramDF,"FINAL_triGramDF.csv")

# repeat the above steps for bi-grams
bigram_txt_1<-bigram_txt[1:1000000]
BiGram <- textcnt(bigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
BigramDF<-data.frame(Bi = names(BiGram), counts = unclass(BiGram))
names(BigramDF)<-c("Bi","counts")
write.csv (BigramDF,"BiGramDF2.csv")

bigram_txt_1<-bigram_txt[1000001:1800000]
BiGram <- textcnt(bigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
BigramDF<-data.frame(Bi = names(BiGram), counts = unclass(BiGram))
names(BigramDF)<-c("Bi","counts")
write.csv (BigramDF,"BiGramDF2.csv")

bigram_txt_1<-bigram_txt[1800001:2335687]
BiGram <- textcnt(bigram_txt_1, method = "string",n=as.integer(2),split = "[[:space:][:digit:]]+",decreasing=T)
BigramDF<-data.frame(Bi = names(BiGram), counts = unclass(BiGram))
names(BigramDF)<-c("Bi","counts")
write.csv (BigramDF,"BiGramDF3.csv")

df1 <- read.csv("BiGramDF1.csv", header=TRUE)
df2 <- read.csv("BiGramDF2.csv", header=TRUE)
df3 <- read.csv("BiGramDF3.csv", header=TRUE)

twogramDF<-rbind(df1,df2,df3)
twogramDF<-aggregate(twogramDF$counts,list(Bi=twogramDF$Bi),sum)
write.csv (twogramDF,"FINAL_BiGramDF.csv")






