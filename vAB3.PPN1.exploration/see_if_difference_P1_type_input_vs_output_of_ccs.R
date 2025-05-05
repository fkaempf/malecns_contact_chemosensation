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

#find subgroup of P1 outputs of PPN1 and vAB3 and mAL
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


vAB3.output.mAL.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(post_id) %>% 
  as.character() %>%
  unique()

 


#look at it from vAB3 


vAB3.unique.input.descending.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'input',threshold=synapse_cutoff) %>% 
  filter(superclass=='descending_neuron') %>%
  pull(pre_id) %>% 
  as.character() %>%
  unique()


vAB3.unique.input.descending.input.P1.id <- cf_partners(cf_ids(malecns=vAB3.unique.input.descending.id), partners = 'input',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(pre_id) %>% 
  as.character() %>%
  unique() #doesnt seem like on this way that there are P1s

#look at it from PPN1

PPN1.unique.input.descending.id <- cf_partners(cf_ids(malecns=PPN1.ids), partners = 'input',threshold=synapse_cutoff) %>% 
  filter(superclass=='descending_neuron') %>%
  pull(pre_id) %>% 
  as.character() %>%
  unique()


PPN1.unique.input.descending.input.P1.id <- cf_partners(cf_ids(malecns=PPN1.unique.input.descending.id), partners = 'input',threshold=synapse_cutoff) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(pre_id) %>% 
  as.character() %>%
  unique()  #doesnt seem like on this way that there are P1s

#ok so rather over in the brain, mAL?



