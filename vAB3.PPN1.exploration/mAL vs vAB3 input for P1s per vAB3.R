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


#find mAL postsynaptic to vAB3 that input to P1
P1_inputs <- list()

for (vAB3.id in vAB3.ids) {
   single.vAB3.P1s.connections <- cf_partners(cf_ids(malecns=vAB3.id), partners = 'out',threshold=synapse_cutoff) %>% 
     filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))
   single.vAB3.mALs.connections.id <- cf_partners(cf_ids(malecns=vAB3.id), partners = 'out',threshold=synapse_cutoff) %>% 
     filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type))%>%
     pull(post_id) %>%
     unique()
   single.vAB3.mALs.connections.P1.connections <- cf_partners(cf_ids(malecns=single.vAB3.mALs.connections.id), partners = 'out',threshold=synapse_cutoff) %>% 
     filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))
  
}