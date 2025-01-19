library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)
library(igraph)
library(ggraph)

vAB3.ids <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.ids <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627')
#PPN1.ids <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
#VAB3.ids <- c('14320','922722','16747','524298') #Bella, Billy, Lisa

PPN1.connection.P1 <- cf_partners(cf_ids(malecns = PPN1.ids),partners = 'outputs',threshold=5)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='PPN1')

vAB3.connection.P1 <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold=5)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='vAB3')


vAb3.output.mAL.connections <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  mutate(pre_type='vAB3')


mAL.ids <- cf_partners(cf_ids(malecns = vAB3.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type))%>%
  pull(post_id) %>%
  unique()

mAL.connections.P1 <- cf_partners(cf_ids(malecns = mAL.ids),partners = 'outputs',threshold = 5) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='mAL')



P1.downstream.vAB3.and.mAL <- rbind(mAL.connections.P1,vAB3.connection.P1,PPN1.connection.P1)




summarized_df <- P1.downstream.vAB3.and.mAL %>%
  group_by(post_id, pre_type) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE),
            type = first(type))%>%
  pivot_wider(names_from = pre_type, 
              values_from = total_weight)%>%
  replace_na(list('mAL' = 0,'vAB3'=0,'PPN1'=0))

ggplot(summarized_df, aes(x = mAL, y = vAB3)) +
  geom_point( shape = 21,aes(color = PPN1 > 0)) + # Points' size based on total_weight
  geom_text(aes(label = type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 10) + # Increase distance to the right
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend for clarity
  labs(x = "mAL total input", y = "vAB3 total input", title = "Scatter Plot with Total Weight")+
  coord_fixed(ratio = 1)+
  geom_abline(alpha=0.2)+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))

