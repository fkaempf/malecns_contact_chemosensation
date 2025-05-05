library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)


#vAB3 and PPN1 ideas extracted by cosine plot and visual inspection
vAB3.ids <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.ids <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627')

#find subgroup of P1 outputs of PPN1 and vAB3
PPN1.unique.output.P1.id <- cf_partners(cf_ids(malecns=PPN1.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(post_id) %>% 
  as.character() %>%
  unique()

vAB3.unique.output.P1.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(post_id) %>% 
  as.character() %>%
  unique()

#find mAL postsynaptic to vAB3 that input to P1

vAB3.output.mAL.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(post_id) %>% 
  as.character() %>%
  unique()

vAB3.output.mAL.unique.output.P1.id <- cf_partners(cf_ids(malecns=vAB3.output.mAL.id), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(post_id) %>% 
  as.character() %>%
  unique() 



vab3.not.mAL.P1 <- setdiff(vAB3.unique.output.P1.id, vAB3.output.mAL.unique.output.P1.id)
mAL.not.vAB3.P1 <- setdiff(vAB3.output.mAL.unique.output.P1.id, vAB3.unique.output.P1.id)

PPN1.and.mAL.P1 <- intersect(PPN1.unique.output.P1.id, vAB3.output.mAL.unique.output.P1.id)
PPN1.not.mAL.P1 <- setdiff(PPN1.unique.output.P1.id, vAB3.output.mAL.unique.output.P1.id)
mAL.not.PPN1.P1 <- setdiff(vAB3.output.mAL.unique.output.P1.id, PPN1.unique.output.P1.id)

PPN1.and.vAB3.P1 <- intersect(PPN1.unique.output.P1.id, vAB3.unique.output.P1.id)
PPN1.not.vAB3.P1 <- setdiff(PPN1.unique.output.P1.id, vAB3.unique.output.P1.id)
vAB3.not.PPN1.P1 <- setdiff(vAB3.unique.output.P1.id, PPN1.unique.output.P1.id)

all.targets.ccs <- c(vAB3.unique.output.P1.id, vAB3.output.mAL.unique.output.P1.id,PPN1.unique.output.P1.id) %>% 
  unique() %>%
  sort()



a <- PPN1.unique.output.P1.id
b <- vAB3.output.mAL.unique.output.P1.id
c <- vAB3.unique.output.P1.id


venn.plot <- venn.diagram(
  x = list(PPN1.P1.targets = a, mAL.P1.targets = b, vAB3.P1.targets = c),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the P1 neurons the contact chemonsensation is convering onto" # Title
)

grid.draw(venn.plot)

#from here on new code
#where does vAB3 and mAL go 
vAB3.and.mAL.P1.id <- intersect(vAB3.unique.output.P1.id, vAB3.output.mAL.unique.output.P1.id)
vAB3.and.mAL.not.PPN1.P1.id <- setdiff(vAB3.and.mAL.P1.id,PPN1.unique.output.P1.id)
vAB3.and.mAL.not.PPN1.P1.outputs<-cf_partner_summary(cf_ids(malecns=vAB3.and.mAL.not.PPN1.P1.id),partners='output',threshold=5,normalise=T)%>%
  drop_na(type.post) %>%
  filter(weight>=0.05)

#where does vAB3 and mAL and PPN1 go 
vAB3.and.mAL.and.PPN1.P1.id <- intersect(intersect(vAB3.unique.output.P1.id, vAB3.output.mAL.unique.output.P1.id),PPN1.unique.output.P1.id)
vAB3.and.mAL.and.PPN1.P1.outputs<-cf_partner_summary(cf_ids(malecns=vAB3.and.mAL.and.PPN1.P1.id),partners='output',threshold=5,normalise=T)%>%
  drop_na(type.post) %>%
  filter(weight>=0.05)
#where does mAL go
mAL.not.vAB3.P1.id <- setdiff(vAB3.output.mAL.unique.output.P1.id,vAB3.unique.output.P1.id)
mAL.not.vAB3.P1.outputs<-cf_partner_summary(cf_ids(malecns=mAL.not.vAB3.P1.id),partners='output',threshold=5,normalise=T)%>%
  drop_na(type.post) %>%
  filter(weight>=0.05)

#where does vAB3 go
vAB.not.mAL.P1.id <- setdiff(vAB3.unique.output.P1.id,vAB3.output.mAL.unique.output.P1.id)
vAB.not.mAL.P1.outputs<-cf_partner_summary(cf_ids(malecns=vAB.not.mAL.P1.id),partners='output',threshold=5,normalise=T) %>%
  drop_na(type.post) %>%
  filter(weight>=0.05)

dev.off()
a= vAB.not.mAL.P1.outputs$type.post %>% unique()
b =mAL.not.vAB3.P1.outputs$type.post %>% unique()
c = vAB3.and.mAL.and.PPN1.P1.outputs$type.post %>% unique()
d = vAB3.and.mAL.not.PPN1.P1.outputs$type.post %>% unique()



venn.plot <- venn.diagram(
  x = list(vAB_not_mAL.P1.output=a,mAL_not_vAB.P1.output=b,vAB_and_mAL_and_PPN1.P1.output=c,vAB_and_mAL_bot_PPN1.P1.output=d),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'black'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 1,                         # Text size inside the circles
  cat.cex = 0.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the outputs of the different ccs P1 input types" # Title
)

grid.draw(venn.plot)


