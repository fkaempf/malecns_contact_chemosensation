library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)
library(igraph)
library(ggraph)
library(ggVennDiagram)
library(ggvenn)

vAB3.id <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.id <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627','21309','20108')
PPN1.id <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
PPN1.ids <-  c('11431', '12286') #new PPN1 decision
vAB3.id <- c('14320','922722','16747','524298') #Bella, Billy, Lisa
synapse_cutoff <- 5

#venn diagram P1 targets of vAB3 PPN1 and mAL cell level
PPN1.output.P1 <- cf_partners(cf_ids(malecns = PPN1.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='PPN1')

vAB3.output.P1 <- cf_partners(cf_ids(malecns = vAB3.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='vAB3')

vAB3.output.mAL <- cf_partners(cf_ids(malecns = vAB3.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  mutate(pre_type='vAB3')

vAB3.output.mAL.output.P1 <- cf_partners(cf_ids(malecns = unique(vAB3.output.mAL.id)),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='mAL')


data_list <- list(PPN1.P1.targets = PPN1.output.P1%>%pull(post_id)%>%as.character(), 
     mAL.P1.targets = vAB3.output.mAL.output.P1%>%pull(post_id)%>%as.character(), 
     vAB3.P1.targets = vAB3.output.P1%>%pull(post_id)%>%as.character())
ggvenn(data_list, 
       fill_color = c("#FFC145", "#5B5F97", "#ADF7B6"),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of PPN1, vAB3, and mAL P1 unique target cells')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))


data_list <- list(PPN1.P1.targets = PPN1.output.P1%>%pull(type)%>%unique()%>%as.character(), 
                  mAL.P1.targets = vAB3.output.mAL.output.P1%>%pull(type)%>%unique()%>%as.character(), 
                  vAB3.P1.targets = vAB3.output.P1%>%pull(type)%>%unique()%>%as.character())
ggvenn(data_list, 
       fill_color = c("#FFC145", "#5B5F97", "#ADF7B6"),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of PPN1, vAB3, and mAL P1 target types')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))


#Venn diagramm overlap with DA1 PNs

PPN1.output.P1 <- cf_partners(cf_ids(malecns = PPN1.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='PPN1')


DA1.output.P1 <- cf_partners(cf_ids(malecns="/type:DA1.+"),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='DA1')





data_list <- list(PPN1.P1.targets = PPN1.output.P1%>%pull(post_id)%>%as.character(), 
                  mAL.P1.targets = vAB3.output.mAL.output.P1%>%pull(post_id)%>%as.character(), 
                  vAB3.P1.targets = vAB3.output.P1%>%pull(post_id)%>%as.character(),
                  DA1.P1.targets = DA1.output.P1%>%pull(post_id)%>%as.character())
ggvenn(data_list, 
       fill_color = c("#0081A7", "#00AFB9", "#FDFCDC",'#F07167'),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of PPN1, vAB3, mAL, and DA1PNs P1 unique target cells')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))



data_list <- list(PPN1.P1.targets = PPN1.output.P1%>%pull(type)%>%unique()%>%as.character(), 
                  mAL.P1.targets = vAB3.output.mAL.output.P1%>%pull(type)%>%unique()%>%as.character(), 
                  vAB3.P1.targets = vAB3.output.P1%>%pull(type)%>%unique()%>%as.character(),
                  DA1.P1.targets = DA1.output.P1%>%pull(type)%>%unique()%>%as.character())
ggvenn(data_list, 
       fill_color = c("#0081A7", "#00AFB9", "#FDFCDC",'#F07167'),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of PPN1, vAB3, mAL, and DA1PNs P1 unique target type')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))

#overlap of mAL targets of DA1.P1s and vAB3.P1s

DA1.output.P1.output.mAL <- cf_partners(cf_ids(malecns=unique(DA1.output.P1$post_id)),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  mutate(pre_type='DA1')


dev.off()

venn.plot <- venn.diagram(
  x = list(vAB3.mAL.targets = vAB3.output.mAL$post_id, 
           DA1.P1.mAL.targets = DA1.output.P1.output.mAL$post_id),
  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping mAL targets of vAB3 and DA1pn target P1 (unique cells)" # Title
)
grid.draw(venn.plot)
dev.off()


data_list <- list(vAB3.mAL.targets = vAB3.output.mAL%>%pull(post_id)%>%as.character(), 
                  DA1.P1.mAL.targets = DA1.output.P1.output.mAL%>%pull(post_id)%>%as.character())
ggvenn(data_list, 
       fill_color = c("#0081A7",'#F07167'),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of vAB3 and DA1PN unique mALs')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))


data_list <- list(vAB3.mAL.targets = vAB3.output.mAL%>%pull(type)%>%unique()%>%as.character(), 
                  DA1.P1.mAL.targets = DA1.output.P1.output.mAL%>%pull(type)%>%unique()%>%as.character())
ggvenn(data_list, 
       fill_color = c("#0081A7",'#F07167'),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of vAB3 and DA1PN unique mAL types')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))


#looking at shared inputs of vAB3 and PPN1

vAB3.inputs <- cf_partners(cf_ids(malecns=unique(vAB3.id)),partners='inputs',threshold = synapse_cutoff)
PPN1.inputs <- cf_partners(cf_ids(malecns=unique(PPN1.id)),partners='inputs',threshold = synapse_cutoff)

vAB3.inputs.sensory <- vAB3.inputs %>% filter(superclass == 'sensory')
PPN1.inputs.sensory <- PPN1.inputs %>% filter(superclass == 'sensory')

data_list <- list(PPN1.sensory.inputs = as.character(PPN1.inputs.sensory%>%pull(pre_id)),
                  vAB3.sensory.inputs = as.character(vAB3.inputs.sensory%>%pull(pre_id)))
ggvenn(data_list, 
       fill_color = c("#00AFB9",'#FED9B7'),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of sensory input to PPN1 and vAB3, unique cells')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))


data_list <- list(PPN1.sensory.inputs = as.character(PPN1.inputs.sensory%>%pull(type)%>%unique()),
                  vAB3.sensory.inputs = as.character(vAB3.inputs.sensory%>%pull(type)%>%unique()))
ggvenn(data_list, 
       fill_color = c("#00AFB9",'#FED9B7'),
       stroke_size = 0.5,
       set_name_size = 3)+
  ggtitle('Overlap of sensory input to PPN1 and vAB3, unique types')+
  theme('minimal',plot.title = element_text(hjust = 0.5, size = 14))


#look how the input is diverging from the different P1 subclasses based on CCS input

vAB3.output.mAL.output.P1
vAB3.output.P1
PPN1.output.P1

mAL.not.vAB3.not.PPN1.P1.id <- setdiff(as.character(vAB3.output.mAL.output.P1$post_id),
                                       as.character(rbind(vAB3.output.P1,PPN1.output.P1)$post_id))
vAB3.not.mAL.not.PPN1.P1.id <- setdiff(as.character(vAB3.output.P1$post_id),
                                       as.character(rbind(vAB3.output.mAL.output.P1,PPN1.output.P1)$post_id))

vAB3.and.mAL.not.PPN1.P1.id <- setdiff(intersect(as.character(vAB3.output.P1$post_id),
                                                 as.character(vAB3.output.mAL.output.P1$post_id)),
                                       as.character(PPN1.output.P1$post_id))
mAL.and.PPN1.not.vAB3.P1.id <- setdiff(intersect(as.character(PPN1.output.P1$post_id),
                                                as.character(vAB3.output.mAL.output.P1$post_id)),
                                      as.character(vAB3.output.P1$post_id))

mAL.and.PPN1.and.vAB3.P1.id <- intersect(intersect(as.character(PPN1.output.P1$post_id),
                                                   as.character(vAB3.output.mAL.output.P1$post_id)),
                                         as.character(vAB3.output.P1$post_id))

mAL.not.vAB3.not.PPN1.P1.outputs <- cf_partners(cf_ids(malecns=unique(mAL.not.vAB3.not.PPN1.P1.id)),partners='outputs',threshold = synapse_cutoff)
vAB3.not.mAL.not.PPN1.P1.outputs <- cf_partners(cf_ids(malecns=unique(vAB3.not.mAL.not.PPN1.P1.id)),partners='outputs',threshold = synapse_cutoff)
vAB3.and.mAL.not.PPN1.P1.outputs <- cf_partners(cf_ids(malecns=unique(vAB3.and.mAL.not.PPN1.P1.id)),partners='outputs',threshold = synapse_cutoff)
mAL.and.PPN1.not.vAB3.P1.outputs <- cf_partners(cf_ids(malecns=unique(mAL.and.PPN1.not.vAB3.P1.id)),partners='outputs',threshold = synapse_cutoff)
mAL.and.PPN1.and.vAB3.P1.outputs <- cf_partners(cf_ids(malecns=unique(mAL.and.PPN1.and.vAB3.P1.id)),partners='outputs',threshold = synapse_cutoff)




data_list <- list(only.mAL = mAL.not.vAB3.not.PPN1.P1.outputs%>%pull(post_id)%>%unique()%>%as.character(),
                       only.vAB3 = vAB3.not.mAL.not.PPN1.P1.outputs%>%pull(post_id)%>%unique()%>%as.character(),
                       vAB3.and.mAL =vAB3.and.mAL.not.PPN1.P1.outputs%>%pull(post_id)%>%unique()%>%as.character(),
                       mAL.and.PPN1 = mAL.and.PPN1.not.vAB3.P1.outputs%>%pull(post_id)%>%unique()%>%as.character(),
                       all=mAL.and.PPN1.and.vAB3.P1.outputs%>%pull(post_id)%>%unique()%>%as.character())
ggVennDiagram(data_list, label = "count") +  # Ensure labels are shown
  scale_fill_gradient(low = "#FED9B7", high = "#0081A7") +  # Define color gradient
  ggtitle("Overlapping Outputs by Different P1 Populations defined by how they receive input from PPN1, vAB3, and mAL") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text = element_text(size = 8)  # Adjusts legend text size
  ) +
  theme(venn_label = element_text(size = 0.1))  # Make numbers smaller









data_list <- list(only.mAL = mAL.not.vAB3.not.PPN1.P1.outputs%>%pull(type)%>%unique()%>%as.character(),
                  only.vAB3 = vAB3.not.mAL.not.PPN1.P1.outputs%>%pull(type)%>%unique()%>%as.character(),
                  vAB3.and.mAL =vAB3.and.mAL.not.PPN1.P1.outputs%>%pull(type)%>%unique()%>%as.character(),
                  mAL.and.PPN1 = mAL.and.PPN1.not.vAB3.P1.outputs%>%pull(type)%>%unique()%>%as.character(),
                  all=mAL.and.PPN1.and.vAB3.P1.outputs%>%pull(type)%>%unique()%>%as.character())
ggVennDiagram(data_list, label = "count") +  # Ensure labels are shown
  scale_fill_gradient(low = "#FED9B7", high = "#0081A7") +  # Define color gradient
  ggtitle("Overlapping Outputs by Different P1 Populations defined by how they receive input from PPN1, vAB3, and mAL") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text = element_text(size = 8)  # Adjusts legend text size
  ) +
  theme(venn_label = element_text(size = 0.1))  # Make numbers smaller


#ratio P1 mAL vs vAB3 input

P1.downstream.vAB3.and.mAL <- rbind(vAB3.output.mAL.output.P1,PPN1.output.P1,vAB3.output.P1)




summarized_unique_cells <- P1.downstream.vAB3.and.mAL %>%
  group_by(post_id, pre_type) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE),
            type = first(type))%>%
  pivot_wider(names_from = pre_type, 
              values_from = total_weight)%>%
  replace_na(list('mAL' = 0,'vAB3'=0,'PPN1'=0))

ggplot(summarized_unique_cells, aes(x = mAL, y = vAB3)) +
  geom_point( shape = 21,aes(color = PPN1 > 0)) + # Points' size based on total_weight
  geom_text(aes(label = type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 10) + # Increase distance to the right
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend for clarity
  labs(x = "mAL total input", y = "vAB3 total input", title = "Ratio input on P1s by mAL vs vAB3 (unique cells)")+
  geom_abline(alpha=0.2)+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))


summarized_unique_types <- P1.downstream.vAB3.and.mAL %>%
  group_by(type, pre_type) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE),
            type = first(type))%>%
  pivot_wider(names_from = pre_type, 
              values_from = total_weight)%>%
  replace_na(list('mAL' = 0,'vAB3'=0,'PPN1'=0))

ggplot(summarized_unique_types, aes(x = mAL, y = vAB3)) +
  geom_point( shape = 21,aes(color = PPN1 > 0)) + # Points' size based on total_weight
  geom_text(aes(label = type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 10) + # Increase distance to the right
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend for clarity
  labs(x = "mAL total input", y = "vAB3 total input", title = "Ratio input on P1s by mAL vs vAB3 (unique types)")+
  geom_abline(alpha=0.2)+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))





