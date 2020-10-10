library(tidyverse)
library(tidytext)
library(igraph)
library(visNetwork)
setwd("~/Documents/kaggle_data/songs/")
songs<-read_csv("./songdata.csv")
glimpse(songs)
song_grp<-songs %>% group_by(artist) %>% 
  summarise(song_cnt=unique(length(song))) %>% 
  arrange(desc(song_cnt))
wordcloud2::wordcloud2(song_grp[1:600,],size = 0.5)

tidy_lyrics <- songs %>% unnest_tokens(word,text)
head(tidy_lyrics)
song_wrd_count<-tidy_lyrics %>% count(song)
song_wrd_count %>% arrange(desc(n)) %>% 
  top_n(n=10) %>% ggplot(aes(x=factor(song,levels=song),y=n))+
  geom_col(col="yellow",fill="darkorange",size=1)+
  labs(x="song",y="word count",title="Words per song-Top 10")
song_wrd_count %>% arrange(desc(n)) %>% 
  tail(n=10) %>% ggplot(aes(x=factor(song,levels=song),y=n))+
  geom_col(col="yellow",fill="darkorange",size=1)+
  labs(x="song",y="word count",title="Which song has very less words")+
  theme(axis.text.x = element_text(angle=45))

lyric_counts <- tidy_lyrics %>%
  left_join(song_wrd_count, by = "song") %>% 
  rename(total_words=n)

lyric_sentiment<-lyric_counts %>% inner_join(get_sentiments("nrc"),by="word")
lyric_sentiment %>% filter(!sentiment %in% c("positive","negative")) %>% 
  count(word,sentiment,sort=TRUE) %>% group_by(sentiment) %>% top_n(n=10) %>% ungroup() %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=sentiment))+
  geom_col(show.legend = FALSE)+facet_wrap(~sentiment,scales="free")+
  coord_flip()

lyric_sentiment %>% count(song,sentiment,sort=TRUE) %>% group_by(sentiment) %>%
  top_n(n=5) %>% ggplot(aes(x=reorder(song,n),y=n,fill=sentiment))+
  geom_bar(stat="identity",show.legend = FALSE)+
  facet_wrap(~sentiment,scales="free")+coord_flip()

lyric_sentiment %>% count(artist,sentiment,sort=TRUE) %>% 
  group_by(sentiment) %>% 
  filter(sentiment %in% c("joy","sadness","anger")) %>% top_n(n=5) %>% 
  ggplot(aes(x=reorder(artist,n),y=n,fill=sentiment))+
  geom_bar(stat="identity",show.legend = FALSE)+
  facet_wrap(~sentiment,scales="free")+coord_flip()


song_lex<-tidy_lyrics %>% inner_join(get_sentiments("bing"),by="word")
song_sent<-song_lex %>% count(song,sentiment)
song_sent %>% filter(sentiment=="positive") %>% arrange(desc(n)) %>% 
  head(10) %>% ggplot(aes(x=reorder(song,n),y=n))+
  geom_col(fill="green4")+
  labs(title="Top Songs - positive words",x="song",y="+ve Word Count")+
  coord_flip()
song_sent %>% filter(sentiment=="negative") %>% arrange(desc(n)) %>% 
  head(10) %>% ggplot(aes(x=reorder(song,n),y=n))+geom_col(fill="red3")+
  labs(title="Top Songs - Negative words",x="song",y="+ve Word Count")+
  coord_flip()

uncommon_wrd<-tidy_lyrics %>% count(song,word) %>% 
  bind_tf_idf(word, song, n) %>% arrange(desc(tf_idf))
uncommon_wrd %>% arrange(desc(tf_idf)) %>% head(20) %>%
  ggplot(aes(x=word,y=tf_idf,fill=song))+geom_col()+
  labs(x="words",title="top 20- Associated words to songs in Lyrics")+
  theme(axis.text.x=element_text(angle=90))

song_lex %>% group_by(word,sentiment) %>% count(sort = T) %>% 
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 300)

#n-gram
lyrics_bigram <- unnest_tokens(songs, input = text, 
                               output = bigram, token = "ngrams", n=2)
bigram_filtered<-lyrics_bigram %>% separate(bigram,c("word1","word2",sep=" "))%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_united <- bigram_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_united %>% count(bigram, sort = TRUE) %>% 
  arrange(desc(n)) %>% head(20) %>% 
  ggplot(aes(x=factor(bigram,levels=bigram),y=n))+
  geom_bar(stat="identity",fill="#FF3E45")+labs(title="Top 20 bigram words in Songs",y="")+
  coord_flip()

section_abba<-songs %>% filter(artist=='ABBA')%>% 
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

lyric_pair<-section_abba %>% widyr::pairwise_count(word,section,sort=TRUE)
word_corr<- section_abba %>% group_by(word) %>%
  filter(n() >= 20) %>%
  widyr::pairwise_cor(word, section, sort = TRUE)

word_corr %>%
  filter(item1 %in% c("love","happy")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity",fill="blue") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

word_corr %>%
  filter(correlation > .1) %>%
  graph_from_data_frame() %>% 
  toVisNetworkData()->vis.data
visNetwork(vis.data$nodes, vis.data$edges) %>% visOptions(highlightNearest = TRUE)
  