#install.packages(c("twitteR","RColorBrewer","plyr","ggplot2","devtools","httr"))

#install.packages("devtools")
library("devtools")
#install.packages("rjson")
#install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
#install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")
#install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
#install_github("twitteR", username="geoffjentry")

library(slam)
library(sentiment)
library(twitteR)
library(RColorBrewer)
library(ggplot2)

api_key <- "F70Tuo2dL0e08mgLU7QTSP9R9"
api_secret <- "I8jKWmBnq2qOlGVnRKintRztIL79q53YDmMUZV5phWpKW9SnBP"
access_token <- "92553187-DZCuxLZDywqblyYukt9jP0NJC1uKEacvwcC5kqlpD"
access_token_secret <- "U6eo9CBfQFSm5aU71Rhg3iQHHSn8h7c7gKT71r9h5C40l"

setup_twitter_oauth(api_key,
                    api_secret,
                    access_token,
                    access_token_secret)

tweets <- searchTwitter("duque", n = 1500, lang = "es")

text <- sapply(tweets,function(x) x$getText())

clean.data <- function(text){
  #eliminar re-tweets y @ del texto original
  text = gsub("(RT|VIA)((?:\\b\\W*@\\w+)+)", "",text)
  text = gsub("@\\w+","",text)
  #eliminar signos de puntuacion y numeros del 0 al 9
  text = gsub("[[:punct:]]", "", text)
  text = gsub("[[:digit:]]", "", text)
  #eliminar links html, tabulaciones y espacios
  text = gsub("http\\w+", "", text)
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
}


text<-clean.data(text)

handle.error <- function(x){
  y = NA
  try_error <- tryCatch(tolower(x),error=function(e)e)
  if(!inherits(try_error,"error"))
    y = tolower(x)
  return(y)
}

text = sapply(text, handle.error)

text <- text[!is.na(text)]

names(text) <- NULL

#analisis de sentimiento
#Enfado|Disgusto|Miedo|Alegría|Tristeza|Sorpresa|Mejor Sentimiento
sentimientos<- classify_emotion(text,algorithm="bayes",prior=1)

emocion <- sentimientos[,7]
emocion[is.na(emocion)]<-"desconocido"

head(emocion)

#analisis de polaridad

polaridad <- classify_polarity(text,algorithm="bayes")

polaridad <- polaridad[,4]


emocion_df <- data.frame(text=text,
                         emocion=emocion,
                         polaridad=polaridad,
                         stringsAsFactors = FALSE)


emocion_df <- within(emocion_df,
                     emocion<-factor(emocion,levels = names(sort(table(emocion),decreasing = TRUE))))

ggplot(emocion_df,aes(x=emocion))+
  geom_bar(aes(y=..count..,fill=emocion))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Categoria emocion",y="Numero de Tweets")+
  labs(title="Anális de Sentimiento")

ggplot(emocion_df,aes(x=polaridad))+
  geom_bar(aes(y=..count..,fill=polaridad))+
  scale_fill_brewer(palette = "Set3")+
  labs(x="Categoria Polaridad",y="Numero de Tweets")+
  labs(title="Anális de Polaridad")




