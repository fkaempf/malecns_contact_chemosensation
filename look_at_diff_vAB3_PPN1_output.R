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

#find subgroup of outputs of PPN1 and vAB3
PPN1.unique.output.id <- cf_partners(cf_ids(malecns=PPN1.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  pull(post_id) %>% 
  as.character() %>%
  unique()

vAB3.unique.output.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=synapse_cutoff) %>% 
  pull(post_id) %>% 
  as.character() %>%
  unique()

PPN1.and.vAB3 <- intersect(PPN1.unique.output.id, vAB3.unique.output.id)
PPN1.not.vAB3 <- setdiff(PPN1.unique.output.id, vAB3.unique.output.id)
vAB3.not.PPN1 <- setdiff(vAB3.unique.output.id, PPN1.unique.output.id)

subset_result <- cf_partners(cf_ids(malecns = vAB3.ids), 
                             partners = 'out', 
                             threshold = synapse_cutoff) %>%
  filter(post_id %in% vAB3.not.PPN1)

subset_result2 <- cf_partners(cf_ids(malecns = PPN1.ids), 
                             partners = 'out', 
                             threshold = synapse_cutoff) %>%
  filter(post_id %in% PPN1.not.vAB3)