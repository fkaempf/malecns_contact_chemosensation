library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)
library(igraph)
library(ggraph)
library(malevnc)
synapse_cutoff=5
PPN1.id <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
vAB3.id <- c('14320','922722','16747','524298') #Bella, Billy, Lisa

vAB3.input.by.PPN1 <- cf_partners(cf_ids(malecns = vAB3.id),partners='inputs',threshold = synapse_cutoff)%>% 
  filter(pre_id %in% PPN1.id)

vAB3.input.by.vAB3 <- cf_partners(cf_ids(malecns = vAB3.id),partners='inputs',threshold = synapse_cutoff)%>% 
  filter(pre_id %in% vAB3.id)

PPN1.input.by.PPN1 <- cf_partners(cf_ids(malecns = PPN1.id),partners='inputs',threshold = synapse_cutoff)%>% 
  filter(pre_id %in% PPN1.id)

PPN1.input.by.vAB3 <- cf_partners(cf_ids(malecns = PPN1.id),partners='inputs',threshold = synapse_cutoff)%>% 
  filter(pre_id %in% vAB3.id)


vAB3.output.mAL <- cf_partners(cf_ids(malecns = vAB3.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  mutate(pre_type='vAB3')%>%group_by(pre_id)

vAB3.output.mAL.output.P1 <- cf_partners(cf_ids(malecns = unique(vAB3.output.mAL.id)),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='mAL')


PPN1.output.P1 <- cf_partners(cf_ids(malecns = PPN1.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='PPN1')

vAB3.output.P1 <- cf_partners(cf_ids(malecns = vAB3.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='vAB3')


mAL.not.vAB3.not.PPN1.P1.outputs <- cf_partners(cf_ids(malecns=unique(mAL.not.vAB3.not.PPN1.P1.id)),partners='outputs',threshold = synapse_cutoff)
vAB3.not.mAL.not.PPN1.P1.outputs <- cf_partners(cf_ids(malecns=unique(vAB3.not.mAL.not.PPN1.P1.id)),partners='outputs',threshold = synapse_cutoff)
vAB3.and.mAL.not.PPN1.P1.outputs <- cf_partners(cf_ids(malecns=unique(vAB3.and.mAL.not.PPN1.P1.id)),partners='outputs',threshold = synapse_cutoff)
mAL.and.PPN1.not.vAB3.P1.outputs <- cf_partners(cf_ids(malecns=unique(mAL.and.PPN1.not.vAB3.P1.id)),partners='outputs',threshold = synapse_cutoff)
mAL.and.PPN1.and.vAB3.P1.outputs <- cf_partners(cf_ids(malecns=unique(mAL.and.PPN1.and.vAB3.P1.id)),partners='outputs',threshold = synapse_cutoff)

mAL.not.vAB3.not.PPN1.P1.inputs <- cf_partners(cf_ids(malecns=unique(mAL.not.vAB3.not.PPN1.P1.id)),partners='inputs',threshold = synapse_cutoff)
vAB3.not.mAL.not.PPN1.P1.inputs <- cf_partners(cf_ids(malecns=unique(vAB3.not.mAL.not.PPN1.P1.id)),partners='inputs',threshold = synapse_cutoff)
vAB3.and.mAL.not.PPN1.P1.inputs <- cf_partners(cf_ids(malecns=unique(vAB3.and.mAL.not.PPN1.P1.id)),partners='inputs',threshold = synapse_cutoff)
mAL.and.PPN1.not.vAB3.P1.inputs <- cf_partners(cf_ids(malecns=unique(mAL.and.PPN1.not.vAB3.P1.id)),partners='inputs',threshold = synapse_cutoff)
mAL.and.PPN1.and.vAB3.P1.inputs <- cf_partners(cf_ids(malecns=unique(mAL.and.PPN1.and.vAB3.P1.id)),partners='inputs',threshold = synapse_cutoff)

#mAL to subtypes
p1_subpop.ids <- vAB3.and.mAL.not.PPN1.P1.id #change this to the P1-mAL subset

print(dim(vAB3.output.mAL.output.P1 %>% 
            filter(post_id %in% p1_subpop.ids))) #get shape to find synapse number

in_from_parent <- vAB3.output.mAL.output.P1 %>% 
  filter(post_id %in% p1_subpop.ids) %>%
  pull(weight) %>%
  sum() #get number of synapses going onto P1 sub type

total_out_parent <- cf_partners(cf_ids(malecns = vAB3.output.mAL.output.P1%>% 
                                            filter(post_id %in% p1_subpop.ids)%>%
                                            pull(pre_id)%>%
                                            unique()),partners='outputs',threshold=5)%>%
                    pull(weight)%>%
        sum() #total inputs of P1 sub population mAL that input to P1 subtype
total_in_child <- cf_partners(cf_ids(malecns = unique(p1_subpop.ids)),partners='input',threshold=5)%>%
        pull(weight)%>%
        sum() #total inputs of P1 sub population


print(in_from_parent/total_out_parent)
print(in_from_parent/total_in_child)

#vAB3 to mAL connection
print(dim(vAB3.output.mAL))
in_from_parent<-vAB3.output.mAL%>%pull(weight)%>%sum()
total_out_parent<-cf_partners(cf_ids(malecns = unique(vAB3.output.mAL$pre_id)),partners='outputs')%>%
  pull(weight)%>%
  sum()

total_in_child<-cf_partners(cf_ids(malecns = unique(vAB3.output.mAL$post_id)),partners='inputs')%>%
  pull(weight)%>%
  sum()


print(in_from_parent/total_out_parent)
print(in_from_parent/total_in_child)



#vAB3 to subtypes
p1_subpop.ids <- mAL.and.PPN1.and.vAB3.P1.id #change this to the P1-vAB3 subset

print(dim(vAB3.output.P1 %>% 
            filter(post_id %in% p1_subpop.ids))) #get shape to find synapse number

in_from_parent <- vAB3.output.P1 %>% 
  filter(post_id %in% p1_subpop.ids) %>%
  pull(weight) %>%
  sum() #get number of synapses going onto P1 sub type

total_out_parent <- cf_partners(cf_ids(malecns = vAB3.output.P1%>% 
                                         filter(post_id %in% p1_subpop.ids)%>%
                                         pull(pre_id)%>%
                                         unique()),partners='outputs',threshold=5)%>%
  pull(weight)%>%
  sum() #total inputs of P1 sub population mAL that input to P1 subtype
total_in_child <- cf_partners(cf_ids(malecns = unique(p1_subpop.ids)),partners='input',threshold=5)%>%
  pull(weight)%>%
  sum() #total inputs of P1 sub population


print(in_from_parent/total_out_parent)
print(in_from_parent/total_in_child)





#PPN1 to subtypes
p1_subpop.ids <- mAL.and.PPN1.and.vAB3.P1.id #change this to the P1-PPN1 subset

print(dim(PPN1.output.P1 %>% 
            filter(post_id %in% p1_subpop.ids))) #get shape to find synapse number

in_from_parent <- PPN1.output.P1 %>% 
  filter(post_id %in% p1_subpop.ids) %>%
  pull(weight) %>%
  sum() #get number of synapses going onto P1 sub type

total_out_parent <- cf_partners(cf_ids(malecns = PPN1.output.P1%>% 
                                         filter(post_id %in% p1_subpop.ids)%>%
                                         pull(pre_id)%>%
                                         unique()),partners='outputs',threshold=5)%>%
  pull(weight)%>%
  sum() #total inputs of P1 sub population mAL that input to P1 subtype
total_in_child <- cf_partners(cf_ids(malecns = unique(p1_subpop.ids)),partners='input',threshold=5)%>%
  pull(weight)%>%
  sum() #total inputs of P1 sub population


print(in_from_parent/total_out_parent)
print(in_from_parent/total_in_child)

#inputs PPN1 and vAB3
vAB3.inputs <- cf_partners(cf_ids(malecns = vAB3.id),partners='inputs',threshold = synapse_cutoff)
vAB3.inputs.summary <- cf_partners(cf_ids(malecns = vAB3.id),partners='inputs',threshold = synapse_cutoff)%>%
  group_by(post_id,type) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE))%>%
  arrange(desc(total_weight))%>%
  pivot_wider(names_from = type, 
              values_from = total_weight,
              values_fill = 0)%>%
  ungroup()
  
vAB3.inputs.summary.normed <- (colSums(vAB3.inputs.summary[-1])/vAB3.inputs %>%
                                 pull(weight)%>%
                                 sum())%>%sort(decreasing=T)

vAB3.inputs.summary.means <- colMeans(vAB3.inputs.summary[-1])%>%sort(decreasing=T)
vAB3.inputs.summary.sum <- colSums(vAB3.inputs.summary[-1])%>%sort(decreasing=T)


PPN1.inputs <- cf_partners(cf_ids(malecns = PPN1.id),partners='inputs',threshold = synapse_cutoff)
PPN1.inputs.summary <- cf_partners(cf_ids(malecns = PPN1.id),partners='inputs',threshold = synapse_cutoff)%>%
  group_by(post_id,type) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE))%>%
  arrange(desc(total_weight))%>%
  pivot_wider(names_from = type, 
              values_from = total_weight,
              values_fill = 0)

PPN1.inputs.summary.normed <- (colSums(PPN1.inputs.summary[-1])/PPN1.inputs %>%
                                 pull(weight)%>%
                                 sum())%>%sort(decreasing=T)
PPN1.inputs.summary.means <- colMeans(PPN1.inputs.summary[-1])%>%sort(decreasing=T)
PPN1.inputs.summary.sum <- colSums(PPN1.inputs.summary[-1])%>%sort(decreasing=T)

#to find total synapeses of input cells
cf_partners(cf_ids(malecns=vAB3.inputs %>% filter(type=='AN05B023') %>% pull(pre_id) %>% unique()),partners = 'outputs',threshold=synapse_cutoff) %>%pull(weight) %>% sum()
vAB3.inputs %>% filter(type=='IN05B011') %>% pull(weight) %>% sum()



big.vAB3.input.output <- cf_partners(cf_ids(malecns=vAB3.inputs %>% 
                                       filter(type=='AN05B035') %>% 
                                       pull(pre_id) %>% 
                                       unique()),partners = 'outputs',threshold=synapse_cutoff)
big.vAB3.input.output.shared.with.vAB3 <- big.vAB3.input.output%>%
  filter(post_id %in% (cf_partners(cf_ids(malecns=vAB3.id),partners='output',threshold = synapse_cutoff)%>%
           pull(post_id)%>%
           unique()))



#stuff used by greg
malecns::mcns_connection_table('AN05B035', partners = 'out', threshold = 10)
AN05B035.out=malecns::mcns_connection_table('AN05B035', partners = 'out', threshold = 10)
?mcns_connection_table
?neuprintr::neuprint_connection_table()
AN05B035.out.byroi=malecns::mcns_connection_table('AN05B035', partners = 'out', threshold = 10, by.roi=T)
View(AN05B035.out.byroi)
AN05B035.out.byroi %>% tidyr::pivot_wider(id_cols = c(bodyid, partner, weight, name, type), names_from = roi, values_from = ROIweight) %>% View()
AN05B035.out.byroi %>% filter(roi %in% c("CentralBrain")) %>% tidyr::pivot_wider(id_cols = c(bodyid, partner, weight, name, type), names_from = roi, values_from = ROIweight) %>% View()
AN05B035.out.byroi %>% filter(roi %in% c("CentralBrain", "VNC")) %>% tidyr::pivot_wider(id_cols = c(bodyid, partner, weight, name, type), names_from = roi, values_from = ROIweight, values_fill = 0) %>% View()
malevnc::manc_meta("AN05B023")
manc=malevnc::manc_body_annotations('AN05B023')
malevnc::manc_body_annotations('AN05B023')
malevnc::manc_neuprint_meta('AN05B023')
#malevnc::choose_malevnc_dataset('')
manc=malevnc::manc_body_annotations('AN05B023')
manc=malevnc::manc_neuprint()
malevnc::choose_malevnc_dataset('VNC')
malevnc::manc_neuprint_meta('AN05B023')

malevnc::manc_neuprint_meta('AN05B023')
malevnc::choose_malevnc_dataset('VNC')
malevnc::manc_neuprint_meta('AN05B023')
malevnc::choose_malevnc_dataset('VNC')
malevnc::manc_neuprint_meta('AN05B023')

cf_meta(cf_ids(flywire='/type:mAL_m1'))%>%View()

