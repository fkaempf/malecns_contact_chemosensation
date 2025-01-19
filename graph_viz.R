library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)
library(igraph)
library(ggraph)

PPN1.ids <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
#PPN1.ids <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627')
PPN1.ids <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
VAB3.ids <- c('14320','922722','16747','524298') #Bella, Billy, Lisa

PPN1.connection.P1 <- cf_partners(cf_ids(malecns = PPN1.ids),partners = 'outputs',threshold=5)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(type=substr(type,1,nchar(type)-1))%>%
  mutate(pre_type='PPN1')

vAB3.connection.P1 <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold=5)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(type=substr(type,1,nchar(type)-1))%>%
  mutate(pre_type='vAB3')


vAb3.output.mAL.connections <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  mutate(pre_type='vAB3')


mAL.ids <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type))%>%
  pull(post_id) %>%
  unique()

mAL.output.P1.connections <- cf_partners(cf_ids(malecns = mAL.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(type=substr(type,1,nchar(type)-1))%>%
  mutate(pre_type='mAL')





graph_input <- rbind(PPN1.connection.P1,
                     vAB3.connection.P1,
                     mAL.output.P1.connections,
                     vAb3.output.mAL.connections)

edges <- graph_input %>%
  select(from = pre_id, to = post_id, weight) 





graph <- graph_from_data_frame(edges, directed = TRUE)
V(graph)$category <- ifelse(
  V(graph)$name %in% vAB3.ids, "vAB3",
  ifelse(V(graph)$name %in% PPN1.ids, "PPN1", 
         ifelse(V(graph)$name %in% mAL.ids, "mAL", "Other"))
)
#graph <- simplify(graph, remove.multiple = TRUE, edge.attr.comb = "sum")
unique_from <- unique(graph$edges$from)
random_colors <- sample(colors(), length(unique_from))

ggraph(graph, layout = "sugiyama") +  # Fruchterman-Reingold layout
  geom_edge_diagonal(aes(color = factor(from), width = weight,alpha=weight), 
             arrow = arrow(length = unit(4, 'mm')), 
             end_cap = circle(7, 'mm')) +  # Add arrows
  geom_node_point(size = 5, aes(color = category)) +  # Style the nodes
  geom_node_text(aes(label = name), repel = F, hjust = 0.5, vjust = 0,size=3) +  # Add node labels
  theme_void() +  # Clean plot background
  labs(title = "Directed Network Graph", 
       edge_width = "Weight")+
  scale_edge_colour_discrete(palette(colorspace::rainbow_hcl(length(unique(graph_input$type.pre)))))