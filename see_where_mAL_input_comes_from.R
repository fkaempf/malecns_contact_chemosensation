library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)


synapse_cutoff = 5
#vAB3 and PPN1 ideas extracted by cosine plot and visual inspection
vAB3.ids <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.ids <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627')


#find subgroup of P1 outputs of PPN1 and vAB3
PPN1.unique.output.P1.type <- cf_partners(cf_ids(malecns=PPN1.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique()

vAB3.unique.output.P1.type <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique()

#find mAL postsynaptic to vAB3 that input to P1
vAB3.output.mAL.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(post_id) %>% 
  unique()

vAB3.output.mAL.type <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(type) %>% 
  unique()

vAB3.output.mAL.unique.output.P1.type <- cf_partners(cf_ids(malecns=vAB3.output.mAL.id), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique() 


all.targets.ccs <- c(vAB3.unique.output.P1.type, vAB3.output.mAL.unique.output.P1.type,PPN1.unique.output.P1.type) %>% 
  unique() %>%
  sort()


mAL.input.P1.unique.type <-  cf_partners(cf_ids(malecns=vAB3.output.mAL.id ), partners = 'inputs',threshold=synapse_cutoff)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique() 

LC10a.output.P1



a <- mAL.input.P1.unique.type
b <- all.targets.ccs


venn.plot <- venn.diagram(
  x = list(all.p1.that.input.css_mALs= a, all.p1.get.input.from.mAL.PPN1.vAB3 = b),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the P1 neurons the contact chemonsensation is convering onto" # Title
)

grid.draw(venn.plot)






