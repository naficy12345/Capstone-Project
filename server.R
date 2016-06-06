library(stylo)
library(tm)
library(stringr)
tri_df <- read.csv("./FINAL_triGramDF.csv", header=TRUE)
bi_df <- read.csv("./FINAL_biGramDF.csv", header=TRUE)

nextword <- function(phrase1) {
cleanphrase1 <- tolower(phrase1)
cleanphrase1 <- removePunctuation(cleanphrase1)
cleanphrase1 <- removeNumbers(cleanphrase1)
cleanphrase1 <- str_replace_all(cleanphrase1, "[^[:alnum:]]", " ")
cleanphrase1 <- stripWhitespace(cleanphrase1)
cleanphrase1 <- str_trim(cleanphrase1)
phraseSize <-length(strsplit(cleanphrase1, " ")[[1]])
input_phrase <- txt.to.words.ext(cleanphrase1, 
                                      language="English.all", 
                                      preserve.case = TRUE)

if (phraseSize >= 2) {
input_phrase1 <- input_phrase[(phraseSize-1):phraseSize]
input_phrase1 <- paste(input_phrase1,collapse=" ")
input_phrase2 <-paste("^",input_phrase1,sep="")
index<-grep(input_phrase2 ,tri_df$Tri)
if (length(index) != 0) 
{output_phrase<-tri_df[min(index),1]
 output_phrase1 <- txt.to.words.ext(output_phrase, 
                                      language="English.all", 
                                      preserve.case = TRUE)
return(output_phrase1[3])
} else { 
input_phrase1 <- input_phrase[phraseSize]
input_phrase2 <-paste("^",input_phrase1,sep="")
index1<-grep(input_phrase2 ,bi_df$Bi)
   if (length(index1) != 0) 
   {output_phrase<-bi_df[min(index1),1]
    output_phrase1 <- txt.to.words.ext(output_phrase, 
                                      language="English.all", 
                                      preserve.case = TRUE)
    return(output_phrase1[2])
       } else return("Sorry! Not able to predict the next word")

}
return ("test 1111")
} 
 
if (phraseSize == 1) {
input_phrase1 <- input_phrase[phraseSize]
input_phrase2 <-paste("^",input_phrase1)
index1<-grep(input_phrase2 ,bi_df$Bi)
if (length(index1) != 0) 
{output_phrase<-bigramDF[min(index1),1]
 output_phrase1 <- txt.to.words.ext(output_phrase, 
                                      language="English.all", 
                                      preserve.case = TRUE)
return(output_phrase1[2])
} else return("Sorry! Not able to predict the next word for one word")
}
if (phraseSize == 0) return("Please make sure you enter at least one word")

}
shinyServer(
function(input,output) {
output$howtouse <-renderPrint(" step 1") 
output$ophrase <-renderPrint({input$phrase})
output$onextword <-renderPrint({nextword(input$phrase)})
}
)



