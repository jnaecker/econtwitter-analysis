library(rtweet)
library(igraph)
library(ggraph)
library(tidyverse)

# Following from here: https://rud.is/books/21-recipes/visualizing-a-graph-of-retweet-relationships.html

# download tweets
tweets <- search_tweets("#econtwitter", n = 2000)

# create graph of retweets
graph_data <- 
  tweets %>%
  filter(retweet_count > 0) %>%
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>%
  filter(!is.na(mentions_screen_name)) %>%
  graph_from_data_frame()

# label only the nodes that have lots of retweets
V(graph_data)$node_label <- unname(ifelse(degree(graph_data)[V(graph_data)] > 30, names(V(graph_data)), "")) 

# make plot
graph_data %>%
  ggraph(layout = "igraph", algorith = "kk") +
    geom_edge_fan(aes(alpha = ..index..), 
                  edge_width = 0.125) +
    geom_node_label(aes(label = node_label),
                    label.size = 0,
                    repel = T,
                    color="red") +
    theme_graph() +
    theme(legend.position = "none")
    