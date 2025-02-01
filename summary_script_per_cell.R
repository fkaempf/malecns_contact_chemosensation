library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)
library(igraph)
library(ggraph)

vAB3.id <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.id <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627','21309','20108')
PPN1.id <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
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


venn.plot <- venn.diagram(
  x = list(PPN1.P1.targets = PPN1.output.P1$post_id, 
           mAL.P1.targets = vAB3.output.mAL.output.P1$post_id, 
           vAB3.P1.targets = vAB3.output.P1$post_id),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping P1 targets of PPN1, vAB3, and vAB3 target mAls (unique cells)" # Title
)
grid.draw(venn.plot)


dev.off()
venn.plot <- venn.diagram(
  x = list(PPN1.P1.targets = PPN1.output.P1$type %>% unique(), 
           mAL.P1.targets = vAB3.output.mAL.output.P1$type %>% unique(), 
           vAB3.P1.targets = vAB3.output.P1$type %>% unique()),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping P1 targets of PPN1, vAB3, and vAB3 target mAls (unique type)" # Title
)
grid.draw(venn.plot)

#Venn diagramm overlap with DA1 PNs

PPN1.output.P1 <- cf_partners(cf_ids(malecns = PPN1.id),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='PPN1')


DA1.output.P1 <- cf_partners(cf_ids(malecns="/type:DA1.+"),partners='outputs',threshold = synapse_cutoff) %>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))%>%
  mutate(pre_type='DA1')


dev.off()
venn.plot <- venn.diagram(
  x = list(PPN1.P1.targets = PPN1.output.P1$post_id, 
           mAL.P1.targets = vAB3.output.mAL.output.P1$post_id, 
           vAB3.P1.targets = vAB3.output.P1$post_id,
           DA1.P1.targets = DA1.output.P1$post_id),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'black'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping P1 targets of DA1pns, PPN1, vAB3, and vAB3 target mAls (unique cells)" # Title
)
grid.draw(venn.plot)


dev.off()
venn.plot <- venn.diagram(
  x = list(PPN1.P1.targets = PPN1.output.P1$type %>% unique(), 
           mAL.P1.targets = vAB3.output.mAL.output.P1$type %>% unique(), 
           vAB3.P1.targets = vAB3.output.P1$type %>% unique(),
           DA1.P1.targets = DA1.output.P1$type%>% unique()),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'black'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping P1 targets of DA1pns, PPN1, vAB3, and vAB3 target mAls (unique type)" # Title
)
grid.draw(venn.plot)


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

venn.plot <- venn.diagram(
  x = list(vAB3.mAL.targets = unique(vAB3.output.mAL$type), 
           DA1.P1.mAL.targets = unique(DA1.output.P1.output.mAL$type)),
  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping P1 targets of DA1pns, PPN1, vAB3, and vAB3 target mAls (unique type)" # Title
)
grid.draw(venn.plot)


#looking at shared inputs of vAB3 and PPN1

vAB3.inputs <- cf_partners(cf_ids(malecns=unique(vAB3.id)),partners='inputs',threshold = synapse_cutoff)
PPN1.inputs <- cf_partners(cf_ids(malecns=unique(PPN1.id)),partners='inputs',threshold = synapse_cutoff)

vAB3.inputs.sensory <- vAB3.inputs %>% filter(superclass == 'sensory')
PPN1.inputs.sensory <- PPN1.inputs %>% filter(superclass == 'sensory')

dev.off()
venn.plot <- venn.diagram(
  x = list(PPN1.sensory.inputs = as.character(PPN1.inputs.sensory%>%pull(pre_id)),
           vAB3.sensory.inputs = as.character(vAB3.inputs.sensory%>%pull(pre_id))),
  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping inputs to vAB3 and PPN1 (unique cells)" # Title
)
grid.draw(venn.plot)

dev.off()
venn.plot <- venn.diagram(
  x = list(vAB3.all.inputs = vAB3.inputs%>%filter(!is.na(type))%>%pull(type)%>%unique(), 
           PPN1.all.inputs = PPN1.inputs%>%filter(!is.na(type))%>%pull(type)%>%unique(),
           PPN1.sensory.inputs = PPN1.inputs.sensory%>%filter(!is.na(type))%>%pull(type)%>%unique(),
           vAB3.sensory.inputs = vAB3.inputs.sensory%>%filter(!is.na(type))%>%pull(type)%>%unique()),
  # Define the four sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'black'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Overlaping inputs to vAB3 and PPN1 (unique types)" # Title
)
grid.draw(venn.plot)


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


dev.off()
venn.plot <- venn.diagram(
  x = list(only.mAL = mAL.not.vAB3.not.PPN1.P1.outputs$post_id,
           only.vAB3 = vAB3.not.mAL.not.PPN1.P1.outputs$post_id,
           vAB3.and.mAL =vAB3.and.mAL.not.PPN1.P1.outputs$post_id,
           mAL.and.PPN1 = mAL.and.PPN1.not.vAB3.P1.outputs$post_id,
           all=mAL.and.PPN1.and.vAB3.P1.outputs$post_id),
  # Define the four sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'black','magenta'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 1,                         # Text size inside the circles
  cat.cex = 0.5,                   # Category label text size
  cat.pos = c(0,0,180,180,0),                     # Position of category labels
  main = "Overlaping outputs by different P1 populations defined by CSS input (unique cells)" # Title
)
grid.draw(venn.plot)


dev.off()
venn.plot <- venn.diagram(
  x = list(only.mAL = mAL.not.vAB3.not.PPN1.P1.outputs%>%filter(!is.na(type))%>%pull(type)%>%unique(),
           only.vAB3 = vAB3.not.mAL.not.PPN1.P1.outputs%>%filter(!is.na(type))%>%pull(type)%>%unique(),
           vAB3.and.mAL = vAB3.and.mAL.not.PPN1.P1.outputs%>%filter(!is.na(type))%>%pull(type)%>%unique(),
           mAL.and.PPN1 = mAL.and.PPN1.not.vAB3.P1.outputs%>%filter(!is.na(type))%>%pull(type)%>%unique(),
           all=mAL.and.PPN1.and.vAB3.P1.outputs%>%filter(!is.na(type))%>%pull(type)%>%unique()),
  # Define the four sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'black','magenta'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 1,                         # Text size inside the circles
  cat.cex = 0.5,                   # Category label text size
  cat.pos = c(0,0,180,180,0),                     # Position of category labels
  main = "Overlaping outputs by different P1 populations defined by CSS input (unique types)" # Title
)
grid.draw(venn.plot)


#ratio P1 mAL vs vAB3 input

P1.downstream.vAB3.and.mAL <- rbind(vAB3.output.mAL.output.P1,PPN1.output.P1,vAB3.output.P1)




summarized_unique_cells <- P1.downstream.vAB3.and.mAL %>%
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





