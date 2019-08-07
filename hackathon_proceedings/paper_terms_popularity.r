library('data.table')
library('dplyr')
library('tidyr')
library('lubridate')
library('stringr')
library('ggplot2')
install.packages('widyr')
library('RColorBrewer')

arxiv_full_df <- read.csv("arxiv_data_full.csv", stringsAsFactors = F)
arxiv_sample_df <- read.csv("arxiv_data_small.csv", stringsAsFactors = F)

arxiv_full_df <- as.data.table(arxiv_full_df)
arxiv_full_df <- arxiv_full_df[!is.na(title)]

head(arxiv_full_df$emails)

#train separate emails into name and domain
train_email <- as.data.table(read.csv('us-500.csv', stringsAsFactors = F))  # todo: что за файл us-500.csv
separate(data=train_email, col=email, into = c('email_name', 'email_domain'), sep = '@')

#создаем колонку со списком тэгов
tags=vector(length=nrow(arxiv_sample_df))
for(i in 1:nrow(arxiv_sample_df)){
  tag_split <- arxiv_sample_df$tag[i] %>% as.character %>% 
    str_split(.,'\t')
  tags[i]=c(tag_split)
}
arxiv_sample_df$tags=tags

#разбиваем тэги на колонки и ищем в каждой строке каждый уникальный тэг (есть = T, нет = F)
unique_tags<-unique(unlist(arxiv_sample_df$tags))
arxiv_sample_df <- as.data.table(arxiv_sample_df)
arxiv_sample_df[,c(unique_tags):=lapply(unique_tags,function(x) sapply(tags, function(y)  x %in% y))]

arxiv_sample_df <- arxiv_sample_df[, c(1:10, 387:390)]
arxiv_sample_df$id=1:nrow(arxiv_sample_df)

#создаем колонку со списком emails
emails_list=vector(length=nrow(arxiv_sample_df))
for(i in 1:nrow(arxiv_sample_df)){
  emails_split <- arxiv_sample_df$emails[i] %>% as.character %>% 
    str_split(.,'\t')
  emails_list[i]=c(emails_split)
}
arxiv_sample_df$emails_list=emails_list


# arxiv_sample_df[,no_emails:=sapply(emails_list, function(x) length(x))]
# unique(arxiv_sample_df$no_emails)
max(arxiv_sample_df$no_emails)

#для каждого email из списка создаем свою колонку
arxiv_sample_df <- arxiv_sample_df %>% unnest(emails_list) %>% group_by(id) %>% mutate(index=1:n()) %>% spread(key = index, value = emails_list)

str_replace_all(arxiv_sample_df$emails[[4988]], '\\t', '.')

arxiv_sample_df$emails[[86]]

#меняем имена
#for i in 1:max(arxiv_sample_df$no_emails){
  setnames(arxiv_sample_df, '1', 'email01')
  setnames(arxiv_sample_df, '2', 'email02')
  setnames(arxiv_sample_df, '3', 'email03')
  setnames(arxiv_sample_df, '4', 'email04')
  setnames(arxiv_sample_df, '5', 'email05')
  setnames(arxiv_sample_df, '6', 'email06')
  setnames(arxiv_sample_df, '7', 'email07')
  setnames(arxiv_sample_df, '8', 'email08')
  setnames(arxiv_sample_df, '9', 'email09')
  setnames(arxiv_sample_df, '10', 'email10')
  setnames(arxiv_sample_df, '11', 'email11')
  setnames(arxiv_sample_df, '12', 'email12')
  setnames(arxiv_sample_df, '13', 'email14')
  setnames(arxiv_sample_df, '14', 'email14')
  setnames(arxiv_sample_df, '15', 'email15')
  setnames(arxiv_sample_df, '16', 'email16')
#}


  View(unique(arxiv_sample_df$email01))


class(arxiv_sample_df)


  
vars_email <- grep('email', names(arxiv_sample_df), fixed=T, value=T)
View(vars_email)
vars_email <- vars_email[3:18]
arxiv_sample_df <- as.data.table(arxiv_sample_df)
cols <- c('id', 'names', vars_email)
data_id_names_emails <- arxiv_sample_df[, .SD, .SDcol=cols]


View(data_id_names_emails)

if (grepl('@', data_id_names_emails$email01)==TRUE) {
  separate(data=data_id_names_emails, col=email01, into = c('email_name01', 'email_domain01'), sep = '@')
}

#separate(data=data_id_names_emails, col=email01, into = c('email_name01', 'email_domain01'), sep = '@')


write.csv(data_id_names_emails, file = "Emails_separated.csv")





#look for 'model' by row
model_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  model_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'model')
  model_detected[i]=c(model_search)
  
  #arxiv_full_df[, .SD, .SDcol='abstract'] [i] %>% str_detect(., 'model')
  }

arxiv_full_df$model_detected=model_detected

#look for 'rule-based' by row
rule_based_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  rule_based_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'rule-based')
  rule_based_detected[i]=c(rule_based_search)
}
arxiv_full_df$rule_based_detected=rule_based_detected

#look for 'neural' by row
neural_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  neural_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'neural')
  neural_detected[i]=c(neural_search)
}
arxiv_full_df$neural_detected=neural_detected

#look for 'neural_network' by row
neural_network_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  neural_network_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'neural network')
  neural_network_detected[i]=c(neural_network_search)
}
arxiv_full_df$neural_network_detected=neural_network_detected


#look for 'deep learning' by row
deep_learning_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  deep_learning_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'deep learning')
  deep_learning_detected[i]=c(deep_learning_search)
}
arxiv_full_df$deep_learning_detected=deep_learning_detected

#look for 'computer vision' by row
CV_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  CV_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'computer vision')
  CV_detected[i]=c(CV_search)
}
arxiv_full_df$CV_detected=CV_detected

#look for 'natural language processing' by row
NLP_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  NLP_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'natural language processing')
  NLP_detected[i]=c(NLP_search)
}
arxiv_full_df$NLP_detected=NLP_detected

#look for 'reinforcement learning' by row
reinf_learning_detected=vector(length=nrow(arxiv_full_df))
for (i in 1:nrow(arxiv_full_df)) {
  reinf_learning_search <- tolower(arxiv_full_df$abstract[i]) %>% str_detect(., 'reinforcement learning')
  reinf_learning_detected[i]=c(reinf_learning_search)
}
arxiv_full_df$reinf_learning_detected=reinf_learning_detected

#create new tables
#
detected_year <- arxiv_full_df[, .SD, .SDcol=c('year', 'model_detected', 'rule_based_detected', 'neural_detected', 'neural_network_detected', 'deep_learning_detected', 'CV_detected', 'NLP_detected', 'reinf_learning_detected')]
# model_year <- arxiv_full_df[, .SD, .SDcol=c('year', 'model_detected')]
# model_year_graph <- model_year[,.N, by=year]
# setnames(model_year_graph, old='N', new='Model_FreQ')
#model_year_graph$year <- sort(model_year_graph$year, decreasing = FALSE)
#View(model_year_graph)

detected_year <- melt(data=detected_year, id.vars='year', measure.vars=c('model_detected', 'rule_based_detected', 'neural_detected', 'neural_network_detected', 'deep_learning_detected', 'CV_detected', 'NLP_detected', 'reinf_learning_detected'))
detected_year <- detected_year[,sum(value), by=.(year, variable)]
detected_year2 <- detected_year[,varsum:=sum(V1), by=year]
detected_year2[,perc:=V1/varsum]
detected_year2$perc[which(is.na(detected_year2$perc)==TRUE)] <- 0
detected_year2[,perc_label:=paste0(round(perc*100,1),"%")]

detected_year <- detected_year2
# detected_year2<-arxiv_full_df[, .SD, .SDcol=c('year', 'model_detected', 'rule_based_detected', 'neural_detected', 'neural_network_detected', 'deep_learning_detected', 'CV_detected', 'NLP_detected', 'reinf_learning_detected')]
# DETECTED <- grep('detected',names(detected_year2), fixed=T, value=T )
# detected_by_year <- detected_year2[,.N,by=c('year', DETECTED)]

setnames(detected_year, old='variable', new ='Topics')


sorted_years <- sort(unique(detected_year$year), decreasing = FALSE)
detected_year[,year:=factor(year, levels = sorted_years)]

mypalette<-brewer.pal(8,"Purples")

ggplot(data=detected_year, aes(x=year, y=perc, color=Topics, fill=Topics)) +geom_bar(stat='identity') +theme_classic() +ggtitle('Frequency of common phrases and topics occurrence') +labs(x='Years') + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), axis.line = element_blank()) + scale_fill_manual(values = mypalette)


#+geom_text(aes(label = perc_label, y = perc))