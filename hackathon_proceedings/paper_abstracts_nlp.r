library(readr)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library("RColorBrewer")
library(stringr)

arxiv_small <- read_csv("arxiv_data_small.csv")  
arxiv_full <- read_csv("arxiv_with_lemm_abstracts.csv")  # todo получить от Дани код
tags <- read_csv("pairwise_tags.csv")  # из paper_tags_and_coauthorship.r
annotations <- read_csv("annotation.csv")  # из paper_tags_and_coauthorship.r
head(tags)
colnames(tags)


#Посмотрите на колонку abstract. Кажется из-за разных кодировок знаки апостров и цитирования отображаются текстом. Этот мусор нужно убрать.
#Пакет stringr работает с текстовыми строками.

#?str_replace_all

# Приводим все слова к нижнему регистру
arxiv_full$lemm_abstract = tolower(arxiv_full$lemm_abstract)
arxiv_full$lemm_abstract = str_replace_all(arxiv_full$lemm_abstract, "w/", "with")

# Убирем то, что осталось от кавычек и апострофа
arxiv_full$lemm_abstract = str_replace_all(arxiv_full$lemm_abstract, "\\&quot\\;", " ")
arxiv_full$lemm_abstract = str_replace_all(arxiv_full$lemm_abstract, "\\&apos\\;", " ")

# Некоторые сообщения начинаются со слова "Summary:". Уберем его
arxiv_full$lemm_abstract = str_replace_all(arxiv_full$lemm_abstract, "summary", "")

# Уберем всю пунктуацию
arxiv_full$lemm_abstract = str_replace_all(arxiv_full$lemm_abstract, "[[:punct:]]", "")

# И числа
arxiv_full$lemm_abstract = str_replace_all(arxiv_full$lemm_abstract, "[0-9]+", "")

#Теперь переделаем датасет в формат tidy text. Для этого нам необходимо разбить имеющиеся тексты на индивидуальные "токены" (сделать токенизацию). С этим поможет функция unnest_tokens(). Токен является значимой единицей текста (чаще всего словом), в которой мы заинтересованы для анализа. В формате tidy text - документ дублируется столько раз, сколько в нем токенов, в каждой строчке один токен (слово). Такой формат данных необходим для агрегации или построения графиков, например.
#Также уберем из данных "стоп-слова", то есть те слова, которые не представляют ценности для анализа (междометия, союзы и т.д.).

arxiv_full_tidy <- arxiv_full %>%
  unnest_tokens(word, lemm_abstract) %>% # разбиваем на слова
  anti_join(stop_words) %>% #убираем стоп-слова
  select(-emails)

head(arxiv_full_tidy)

#Посмотрим, какие слова наиболее часто употребляют.
#Не удивительно, что в топе object и light. Наблюдаем проблему: light и lights считается за два разных слова. Как это можно исправить? С помощью лемматизации.

arxiv_full_tidy_2012 <- arxiv_full_tidy %>% filter (year < 2012)
arxiv_full_tidy_2013 <- arxiv_full_tidy %>% filter (year >= 2012)


arxiv_full_tidy_2012 %>%
  count(word, sort = TRUE) %>%
  filter(row_number() < 30) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(x = "word") + 
  coord_flip() +
  theme_minimal() + 
  ggtitle("Частотность слов до 2012 года")

arxiv_full_tidy_2013 %>%
  count(word, sort = TRUE) %>%
  filter(row_number() < 30) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(x = "word") + 
  coord_flip() +
  theme_minimal()+ 
  ggtitle("Частотность слов после 2012 года")



arxiv_full_tidy_2012 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

arxiv_full_tidy_2013 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


getCount <- function(data,keyword)
{
  wcount <- str_count(arxiv_full_tidy$lemm_abstract, keyword)
  return(data.frame(data,wcount))
}

model <- getCount(arxiv_full_tidy,'model')

pairwise_count(tags, firstpart1, secondpart1, sort = TRUE)

pairwise_tags <- tags %>% select(secondpart1,secondpart2) %>% 
  drop_na(secondpart1, secondpart2) %>% group_by(secondpart1) %>% count() %>% arrange(desc(n))

col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)

heatmap(tags, scale = "none", col =  col, 
        RowSideColors = rep(c("blue", "pink"), each = 16),
        ColSideColors = c(rep("purple", 5), rep("orange", 6)))

heatmap(tags, scale = "row")
class(tags$freq)
colnames(tags)
tags <- tags %>% select(-X1)
