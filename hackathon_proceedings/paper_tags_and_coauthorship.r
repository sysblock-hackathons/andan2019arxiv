library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)

#Загружаем архив
arxiv_full_df <- read.csv("arxiv_data_full.csv")
arxiv_sample_df <- arxiv_full_df[1:5000,]

#Парсим авторов
n_parsed <- str_split(arxiv_full_df$names,"\t")

library(plyr)
#Сделали широкую таблицу
authors = ldply(n_parsed,function(s){t(data.frame(unlist(s)))})
for(i in 1:nrow(authors)){
  authors$count[i]=75-sum(is.na(authors[i,]))
}


arxiv_full_df$n_auth <- authors$count

auth_max <- auth_count %>% select(year, n_auth) %>% group_by(year,n_auth) %>% count()

#Присвоили id архиву и нашей таблице 
arxiv_full_df$id <- 1:nrow(arxiv_full_df)
authors$id <- 1:nrow(arxiv_full_df)

#Добавили название статьи нашей таблице
authors$title <- arxiv_full_df$title

#Сделали длинную таблицу
authors_long <- authors %>% gather(Number, Author, 1:75)

#Очистили от NA и отсортировали по названию статьи
authors_long <- na.omit(authors_long) %>% arrange(title)

write.csv(authors_long, file = "Длинный_список_авторов.csv")

#Создаем переменную дата
arxiv_full_df$date <- paste(arxiv_full_df$year, arxiv_full_df$mon, arxiv_full_df$day, sep="-") %>% ymd() %>% as.Date()


arxiv_full_df %>% select(year) %>% count(year) %>% 
  ggplot(aes(x=year,y=n))+geom_point()


for(i in 1:nrow(arxiv_sample_df)){
  tagmerged=as.character()
  var_tag=str_split(tagmerged)
}

tags <- list()
for(i in 1:nrow(arxiv_full_df)){
  tag_split <- arxiv_full_df$tag[i] %>% as.character %>% 
    str_split(.,'\t')
  tag_split <- unlist(tag_split)
  tags[[i]]=tag_split
}


#--------
#неудачные попытки кодинга
#------
#Парсим тэги на 1ю и 2ю части
tags <- list()
tags1 <- vector(length=nrow(arxiv_sample_df))
tags2 <- vector(length=nrow(arxiv_sample_df))
t1=vector(length=1)
t2=vector(length=1)
for(i in 1:nrow(arxiv_sample_df)){
tag_split <- arxiv_sample_df$tag[i] %>% as.character %>% 
    str_split(.,'\t')
tag_split <- unlist(tag_split)
#   for(j in 1:length(tag_split)){
#     t1[j] <- tag_split %>% str_extract("[:alnum:]+[,\\.]" ) %>% 
#       str_remove("[\\.,]") %>% list()
#     t2[j] <- tag_split %>% str_extract("[,\\.][:alnum:]+") %>% 
#       str_remove("[\\.,]")%>% list()
#   }
# tags1=c(t1)
# tags2=c(t2)
tags[[i]]=tag_split
}

for(i in ncol(arxiv_sample_df)){
  print(sum(is.na(arxiv_sample_df[,i])))
}
#------------------

library(plyr)
tags_sort = ldply(tags,function(s){t(data.frame(unlist(s)))})
for(i in 1:ncol(tags_sort)){
  print(sum(!is.na(tags_sort[,i])))
}

tags_sort=tags_sort[,1:4]

firstpart1 <- vector()
secondpart1 <- vector()

for(i in 1:length(tags_sort$`1`)){
  firstpart1[i] <- tags_sort$`1`[i] %>% str_extract("[:alnum:]+[,\\.]" )
  secondpart1[i] <- tags_sort$`1`[i] %>% str_extract("[,\\.][:alnum:]+" )
}

tags_sort$firstpart1 <- as.factor(firstpart1)
tags_sort$secondpart1 <- as.factor(secondpart1)


#tags_sort %>% select(`1`,`2`) %>% group_by(`1`) %>% count(match()) 

arxiv_sample_df$tags=tags
levels(tags_sort$firstpart1)

arxiv_full_df$sciencefield=as.factor(firstpart1)
arxiv_full_df$secondpart1=as.factor(secondpart1)

sum(is.na(firstpart1))

arxiv_full_df %>% select(sciencefield,year) %>% drop_na() %>% group_by(sciencefield) %>% count() %>% 
  ggplot(aes(x=year, y=freq,color=sciencefield))+geom_point()+geom_line()

levels(tags_sort$secondpart1)

secondpart2 <- vector()
for(i in 1:length(tags_sort$`2`)) {
  ifelse (is.na(tags_sort$`2`[i])==T,secondpart2[i] <- NA, if (str_detect(tags_sort$`2`[i],"\\.") ==F){
    secondpart2[i] <- NA
  } else {
  secondpart2[i] <- tags_sort$`2`[i] %>% str_extract("[,\\.][:alnum:]+" )
  })
}
tags_sort$secondpart2=as.factor(secondpart2)
arxiv_full_df$secondpart2=as.factor(secondpart2)

write.csv(tags_sort, file = "Articles_tags.csv")
pairwise_tags <- arxiv_full_df %>% select(secondpart1,secondpart2) %>% 
  drop_na(secondpart1, secondpart2) %>% group_by(secondpart1) %>% count() %>% arrange(desc(freq))
pairwise_tags[1:100,]
write.csv(pairwise_tags, file = "pairwise_tags.csv")
a=arxiv_full_df %>% select(secondpart1) %>% count() %>% arrange(desc(freq))

write.csv(a$secondpart1, file="annot.csv")
annot=read.csv2("annot.csv")
write.csv(annot, file="annotation.csv")

arxiv_full_df %>% select(secondpart1, year) %>% 
  filter(secondpart1 %in% c(".CV",".LG",".AI",".CL",".ML",".NE",".IR",".RO",".OC",".LO")) %>% 
  group_by(year) %>% count() %>% 
  ggplot(aes(x=year,y=freq,color=secondpart1))+geom_point()+geom_line()

total_year <- arxiv_full_df %>% select(year) %>% group_by(year) %>% count() 

top10_publ <- arxiv_full_df %>% select(secondpart1, year) %>% 
  filter(secondpart1 %in% c(".CV",".LG",".AI",".CL",".ML",".NE",".IR",".RO",".OC",".LO")) %>% 
  group_by(year) %>% count()


for(i in 1:nrow(top10_publ)){
  top10_publ$total[i] <- total_year$freq[top10_publ$year[i]==total_year$year]
}

top10_publ$percent=top10_publ$freq/top10_publ$total

top10_publ %>% 
  
  
ggplot(top10_publ,mapping=aes(x=top10_publ$year,y=top10_publ$percent,color=top10_publ$secondpart1))+
  geom_point()+geom_line()
ggplot(top10_publ,mapping=aes(x=top10_publ$year,y=top10_publ$freq,color=top10_publ$secondpart1))+
  geom_point()+geom_line()

#ML AI CV CL LO RO SI HC




















tag1lev <- levels(tags_sort$`1`)
tag2lev <- levels(tags_sort$`2`)

#ML AI CV CL LO RO SI HC

df$filter=



for (i in 1:length(tag1lev)){
  tag1lev[i] %>% match(na.omit(tags_sort$`2`)) %>% count() %>% print(.,"-",tag1lev[i],"\n")
}
tag2lev
match(tags_sort$`2`)

tag1lev[120]




