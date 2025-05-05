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
PPN1.ids <- c('520195', '13399', '17503', '21035','21039', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627','20108')
# AN05B102 is the type determined by bella,billy,lisa

View(cf_partners(cf_ids(malecns='520195')))

for (PPN1.id in PPN1.ids){
  print(PPN1.id)
  temp.id <- cf_partners(cf_ids(malecns=PPN1.id),partners='input') %>% filter(superclass=='sensory') %>% pull(type) %>% unique()
  temp.type <- cf_meta(cf_ids(malecns=PPN1.id))$type
  #print(temp)
  print(sprintf("%s (%s) gets input from M-cells = %s (SNch07)", PPN1.id,temp.type, as.character("SNch07" %in% temp.id)))
  print(sprintf("%s (%s) gets input from F-cells = %s (SNch05)", PPN1.id,temp.type, as.character("SNch05" %in% temp.id)))
}