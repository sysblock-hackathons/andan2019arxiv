library(readr)
library(igraph)
library(tidytext)
library(widyr)
library(tidyverse)
library(ggplot2)
library(ggraph)
library(tnet)
library(dplyr)

edges <- read_csv("edges.csv")  # todo стрясти с олега код, пока из edges.csv
# authors <- read_csv("Authors.csv")  # todo откуда
edges_weighted <- read_csv("edges_weighted_positive.csv")  # из edges_ctr.py ненулевые
tags <- read_csv("pairwise_tags.csv")  # из paper_tags_and_coauthorship.r
head(edges_weighted)
edges_weighted <- na.omit(edges_weighted)

#сеть университетов - сотрудничество (попробовать взвешенные)
tags <- tags %>% filter(freq >= 50)
tags <- tags %>% slice(1:20)


total = data.frame(
  secondpart1 = c(tags$secondpart1),
  secondpart2 = c(tags$secondpart2)
)

g = graph_from_data_frame(total, directed = FALSE)
g <- igraph::simplify(g)
#g_degree <- degree(g, mode="all")
E(g)$weight = tags$freq
E(g)$width <- E(g)$weight/6
E(g)$width <- 1+E(g)$weight/12



netm <- get.adjacency(g, attr="weight", sparse=F)
V(g)


colnames(netm) <- V(g)$tags
rownames(netm) <- V(g)$tags
g

palf <- colorRampPalette(c("gold", "dark orange")) 

heatmap(netm[,100:1], Rowv = NA, Colv = NA, col = palf(100), 
        
        scale="none", margins=c(10,10) )

l <- layout_on_sphere(g)
plot(g, layout=l, edge.width=E(g)$width)



edges_weighted <- edges_weighted %>% rename(gmail = hhi.fraunhofer.de)
edges_weighted <- edges_weighted %>% rename(unifor = hhi.fraunhofer.de_1)
E(g)$weight = edges_weighted$`38`
freq_weight <- as.data.frame(table(edges_weighted$`38`))
quantile(freq_weight$Freq)
hist(freq_weight$Freq)

filter <- freq_weight %>% filter(Freq >= 10)
edges_weighted_f <- edges_weighted %>% filter(`38` > 10)

# Get the components of an undirected graph
cl = clusters(g)

# How many components?
cl$no           

# How big are these (the first row is size, the second is the number of components of that size)?
table(cl$csize) 

# Get the giant component
nodes = which(cl$membership == which.max(cl$csize))
plot(nodes)

# Color in red the nodes in the giant component and in sky blue the rest
V(g)$color  = "SkyBlue2"
V(g)[nodes]$color = "red"
plot(g, vertex.size = 3, vertex.label=NA)

giant.component <- function(graph) { 
  cl <- clusters(g) 
  induced.subgraph(g, which(cl$membership == which.max(cl$csize)))} 
G <- giant.component(as.igraph(C)) 

#g2 <- giant_component_extract(g) requires package CINNA

cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g), vertex.label = NA)

clp <- cluster_label_prop(g)
plot(clp, g)

library(visNetwork)
g1 <- toVisNetworkData(g)
g_degree <- degree(g, mode="all")
g1$nodes$size = g_degree/3
g1$edges$size = E(g)$width/50
g1$nodes$color = RColorBrewer::brewer.pal(12, "Paired")

visNetwork(nodes = g1$nodes, edges = g1$edges, height = "800px", width = "800px") %>% 
  visIgraphLayout()


# #сеть соавторства по авторам
# total10 = data.frame(
#   title = c(authors$title),
#   Author = c(authors$Author)
# )

# g10 = graph_from_data_frame(total10, directed = FALSE)
# g10 = simplify(g10)
# V(g10)$type = bipartite_mapping(g10)$type

# plot(g10)


# g10.bp <- bipartite.projection(g10) 

# par(mfrow=c(1,2))
# plot(g10.bp$proj1, vertex.label.color="black", vertex.label=NA)
#      #vertex.label=nodes2$media[!is.na(nodes2$media.type)])
# plot(g10.bp$proj2, vertex.label.color="black", vertex.label=NA)
#      #vertex.label=nodes2$media[ is.na(nodes2$media.type)]) 
