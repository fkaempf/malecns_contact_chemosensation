library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)

#vAB3 and PPN1 ids
vAB3.ids <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.ids <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627')


vAb3.output.P1.connections <- cf_partner_summary(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5,aggregate.query = T) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type.post))%>%
  mutate(type.post=substr(type.post,1,nchar(type.post)-1))

PPN1.output.P1.connections <- cf_partner_summary(cf_ids(malecns = PPN1.ids),partners = 'outputs',threshold = 5,aggregate.query = T) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type.post))%>%
  mutate(type.post=substr(type.post,1,nchar(type.post)-1))


vAb3.output.mAL.connections <- cf_partner_summary(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5,aggregate.query = T) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type.post)) 

mAL.ids <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type))%>%
  pull(post_id) %>%
  unique()

mAL.output.P1.connections <- cf_partner_summary(cf_ids(malecns = mAL.ids),partners = 'outputs',threshold = 5,aggregate.query = T) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type.post))%>%
  mutate(type.post=substr(type.post,1,nchar(type.post)-1))





graph_input <- rbind(vAb3.output.P1.connections, mAL.output.P1.connections, vAb3.output.mAL.connections)

edges <- graph_input %>%
  select(from = type.pre, to = type.post, weight) 

graph <- graph_from_data_frame(edges, directed = TRUE)


unique_from <- unique(graph$edges$from)
random_colors <- sample(colors(), length(unique_from))

ggraph(graph, layout = "sugiyama") +  # Fruchterman-Reingold layout
  geom_edge_(aes(color = factor(from), width = weight), 
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(7, 'mm'),alpha=0.3) +  # Add arrows
  geom_node_point(size = 5, color = "lightblue") +  # Style the nodes
  geom_node_text(aes(label = name), repel = F, hjust = 0.5, vjust = 0,size=3) +  # Add node labels
  theme_void() +  # Clean plot background
  labs(title = "Directed Network Graph", 
       edge_width = "Weight")+
  scale_edge_colour_discrete(palette(colorspace::rainbow_hcl(length(unique(graph_input$type.pre)))))
  

