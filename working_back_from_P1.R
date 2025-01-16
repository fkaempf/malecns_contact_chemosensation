library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)


P1.input <- cf_partner_summary(cf_ids(malecns="/P1.+"), partners = 'in', aggregate.query = F,normalise=T) %>%
  filter(weight>=0.05)
P1.input.isna <- P1.input %>%
  filter(is.na(type.pre))


#check the highest weight NA
aaa <- cf_partners(cf_ids(malecns='15329'),partners = 'in')
aaa %>% filter(superclass == 'ascending_neuron') %>% pull(pre_id) %>% paste(collapse = " ")


#find all ascending P1 inputs
P1.input.cf_partners <- cf_partners(cf_ids(malecns="/P1.+"), partners = 'in',threshold=5) %>% 
  filter(superclass=='ascending_neuron')

#cf_cosine_plot(P1.input.cf_partners$pre_key,interactive=T)

#vAB3 and PPN1 ideas extracted by cosine plot and visual inspection
vAB3.ids <- c('16302', '15645', '16535', '16830', '13693', '13341', '11998', '16747',  '512498', '524298', '14320', '922722')
PPN1.ids <- c('520195', '13399', '17503', '21035', '17416', '18164','17492','520195','18430','18696','200336','801269','18324','18627')

#find subgroup of P1 outputs of PPN1 and vAB3
PPN1.unique.output.P1.type <- cf_partners(cf_ids(malecns=PPN1.ids), partners = 'out',threshold=5) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique()

vAB3.unique.output.P1.type <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=5) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique()

#find mAL postsynaptic to vAB3 that input to P1
vAB3.output.mAL.id <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=5) %>% 
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(post_id) %>% 
  unique()

vAB3.output.mAL.type <- cf_partners(cf_ids(malecns=vAB3.ids), partners = 'out',threshold=5) %>% 
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(type) %>% 
  unique()

vAB3.output.mAL.unique.output.P1.type <- cf_partners(cf_ids(malecns=vAB3.output.mAL.id), partners = 'out',threshold=5) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique() 


vAB3.and.mAL.P1 <- intersect(vAB3.unique.output.P1.type, vAB3.output.mAL.unique.output.P1.type)
vab3.not.mAL.P1 <- setdiff(vAB3.unique.output.P1.type, vAB3.output.mAL.unique.output.P1.type)
mAL.not.vAB3.P1 <- setdiff(vAB3.output.mAL.unique.output.P1.type, vAB3.unique.output.P1.type)

PPN1.and.mAL.P1 <- intersect(PPN1.unique.output.P1.type, vAB3.output.mAL.unique.output.P1.type)
PPN1.not.mAL.P1 <- setdiff(PPN1.unique.output.P1.type, vAB3.output.mAL.unique.output.P1.type)
mAL.not.PPN1.P1 <- setdiff(vAB3.output.mAL.unique.output.P1.type, PPN1.unique.output.P1.type)

PPN1.and.vAB3.P1 <- intersect(PPN1.unique.output.P1.type, vAB3.unique.output.P1.type)
PPN1.not.vAB3.P1 <- setdiff(PPN1.unique.output.P1.type, vAB3.unique.output.P1.type)
vAB3.not.PPN1.P1 <- setdiff(vAB3.unique.output.P1.type, PPN1.unique.output.P1.type)

all.targets.ccs <- c(vAB3.unique.output.P1.type, vAB3.output.mAL.unique.output.P1.type,PPN1.unique.output.P1.type) %>% 
  unique() %>%
  sort()

a <- PPN1.unique.output.P1.type
b <- vAB3.output.mAL.unique.output.P1.type
c <- vAB3.unique.output.P1.type

dev.off()
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

# Display the Venn diagram
grid.draw(venn.plot)



#add the DA1 projection neurons and see if this adds a new dimension
DA1.unique.output.P1.type <- cf_partners(cf_ids(malecns="/type:DA1.+"), partners = 'outputs',threshold=5) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(type) %>% 
  unique()
DA1.unique.output.P1.id <- cf_partners(cf_ids(malecns="/type:DA1.+"), partners = 'outputs',threshold=5) %>% 
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type)) %>%
  pull(post_id) %>% 
  unique()


d <- DA1.unique.output.P1.type

dev.off()
venn.plot <- venn.diagram(
  x = list(PPN1.P1.targets = a, mAL.P1.targets = b, vAB3.P1.targets = c, DA.P1.targets=d),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6",'#000000'), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the P1 neurons the contact chemonsensation is convering onto + DA1 inputs" # Title
)

# Display the Venn diagram
grid.draw(venn.plot)



#look if any of the P1s go on to give input to the ccs circuit
DA1.output.P1.output.descending.id <- cf_partners(cf_ids(malecns=DA1.unique.output.P1.id), partners = 'out',threshold=5) %>% 
  filter(superclass=='descending_neuron') %>%
  pull(post_id) %>% 
  unique()

DA1.output.P1.output.descending.output <- cf_partners(cf_ids(malecns=DA1.output.P1.output.descending.id), partners = 'out',threshold=5)
intersect(PPN1.ids,DA1.output.P1.output.descending.output$post_id) #nope
intersect(vAB3.ids,DA1.output.P1.output.descending.output$post_id) #nope

#look if the mALs that are targeted by DA1.P1s are the same that are targeted by vAB3s 
DA1.out.P1.outout.unique.mAL.type <- cf_partners(cf_ids(malecns=DA1.unique.output.P1.id), partners = 'out',threshold=5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(type) %>% 
  unique()
  

a = vAB3.output.mAL.type
b = DA1.out.P1.outout.unique.mAL.type

dev.off()
venn.plot <- venn.diagram(
  x = list(vAB3.mAL=a, DA1.mAL=b),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1.5,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the mAL neurons that receive input from DA1 and from vAB3" # Title
)

# Display the Venn diagram
grid.draw(venn.plot)

#look if the P1 neurons that receive DA1 input connect to vAB3 or PPN1 neurons
DA1.output.P1.output <- cf_partners(cf_ids(malecns=DA1.unique.output.P1.id), partners = 'out',threshold=5)

intersect(PPN1.ids,DA1.output.P1.output$post_id) #nope
intersect(vAB3.ids,DA1.output.P1.output$post_id) #nope


#look at inputs of vAB3 and PPN1 
View(cf_partners(cf_ids(malecns=c(PPN1.ids)),partners = 'in'))
View(cf_partners(cf_ids(malecns=c(vAB3.ids)),partners = 'in'))
#Both connected to AN05B035 whatever this is
View(cf_partner_summary(cf_ids(malecns = 'AN05B035'),partners = 'out',normalise=T))
#AN05B025 --> PPN1
#AN05B023 --> PPN1
#AN09B017 --> vAB3

#since AN05B035 is also connecting to mAL lets look at the vAB3 and BA1 overlook

AN05B035.out.unique.mAL.type <- cf_partners(cf_ids(malecns = 'AN05B035'), partners = 'out',threshold=5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(type) %>% 
  unique()



a = vAB3.output.mAL.type
b = DA1.out.P1.outout.unique.mAL.type
c = AN05B035.out.unique.mAL.type
dev.off()
venn.plot <- venn.diagram(
  x = list(vAB3.mAL=a, DA1.P1.mAL=b,AN05B035.mAL=c),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97", "#ADF7B6"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the mAL neurons that receive input from DA1, AN05B035 and from vAB3.P1s" # Title
)

# Display the Venn diagram
grid.draw(venn.plot)




#look into sensory neuron input of vAB3s and PPN1s 

vAB3.sensory.input.unique.id <- cf_partners(cf_ids(malecns = vAB3.ids), partners = 'in',threshold=5)%>%
  filter(superclass == 'sensory')%>%
  pull(pre_id) %>%
  as.character() %>%
  unique()

vAB3.sensory.input.unique.type <- cf_partners(cf_ids(malecns = vAB3.ids), partners = 'in',threshold=5)%>%
  filter(superclass == 'sensory')%>%
  pull(type) %>%
  as.character() %>%
  na.omit() %>%
  unique()


PPN1.sensory.input.unique.id <- cf_partners(cf_ids(malecns = PPN1.ids), partners = 'in',threshold=5)%>%
  filter(superclass == 'sensory') %>%
  pull(pre_id) %>%
  as.character() %>%
  unique()

PPN1.sensory.input.unique.type <- cf_partners(cf_ids(malecns = PPN1.ids), partners = 'in',threshold=5)%>%
  filter(superclass == 'sensory') %>%
  pull(type) %>%
  as.character() %>%
  na.omit() %>%
  unique()

setdiff(PPN1.sensory.input.unique.id,vAB3.sensory.input.unique.id) %>% paste(collapse=' ')
setdiff(vAB3.sensory.input.unique.id,PPN1.sensory.input.unique.id) %>% paste(collapse=' ')
intersect(PPN1.sensory.input.unique.id,vAB3.sensory.input.unique.id) %>% paste(collapse=' ')

a = vAB3.sensory.input.unique.type
b = PPN1.sensory.input.unique.type

dev.off()
venn.plot <- venn.diagram(
  x = list(vAB3.input.types=a, PPN1.input.types=b),  # Define the three sets
  filename = NULL,                 # No file output
  fill = c("#FFC145", "#5B5F97"), # Colors for the circles
  alpha = 0.5,                     # Transparency of the circles
  cex = 2,                         # Text size inside the circles
  cat.cex = 1,                   # Category label text size
  cat.pos = 0,                     # Position of category labels
  main = "Venn Diagram of the input types of vAB3 and PPN1" # Title
)

grid.draw(venn.plot)
