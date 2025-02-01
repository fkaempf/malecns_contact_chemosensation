library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)
library(VennDiagram)
library(igraph)
library(ggraph)
library(ggplot2)

PPN1.id <- cf_meta(cf_ids(malecns='AN05B102')) %>% pull(id) #Bella, Billy, Lisa
vAB3.id <- c('14320','922722','16747','524298') #Bella, Billy, Lisa
synapse_cutoff <- 5
vAB3.output.mAL <- cf_partners(cf_ids(malecns=vAB3.id),partners='outputs',threshold=synapse_cutoff)%>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type))

vAB3.output.mAL.output.P1 <- cf_partners(cf_ids(malecns=unique(vAB3.output.mAL$post_id)),partners='outputs',threshold=synapse_cutoff)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))

vAB3.output.P1 <- cf_partners(cf_ids(malecns=vAB3.id),partners='outputs',threshold=synapse_cutoff)%>%
  filter(grepl("^P1_([0-9]|[1-9][0-9])[a-zA-Z]$", type))

vAB3.mAL.pair <- NULL

for (vAB3.iter.id in vAB3.id){
  temp.mAL.id <- vAB3.output.mAL %>% 
    filter(pre_id==vAB3.iter.id)%>%
    pull(post_id)%>%
    as.character()%>%
    unique()
  
  temp.vAB3.P1.id <- vAB3.output.P1 %>% 
    filter(pre_id==vAB3.iter.id) %>% 
    pull(post_id) %>% 
    as.character() %>% 
    unique()
  
  for (mAL.iter.id in temp.mAL.id){
  
    
    temp.mAL.P1.id <- vAB3.output.mAL.output.P1 %>% 
      filter(pre_id==mAL.iter.id) %>% 
      pull(post_id) %>% 
      as.character() %>% 
      unique()
    
    temp.all.P1<-c(temp.vAB3.P1.id,temp.mAL.P1.id)
    for (P1.iter.id in temp.all.P1){
      temp.mAL.weight <- vAB3.output.mAL.output.P1 %>% 
        filter(post_id==P1.iter.id) %>% 
        filter(pre_id==mAL.iter.id)%>%
        pull(weight)

      temp.vAB3.weight <- vAB3.output.P1 %>% 
        filter(post_id==P1.iter.id) %>% 
        filter(pre_id==vAB3.iter.id)%>%
        pull(weight)
      temp.P1.type <- vAB3.output.P1 %>% 
        filter(post_id==P1.iter.id) %>% 
        filter(pre_id==vAB3.iter.id) %>%
        pull(type)
      
      if(length(temp.P1.type)==0){temp.P1.type<-vAB3.output.mAL.output.P1 %>% 
        filter(post_id==P1.iter.id) %>% 
        pull(type)%>%unique()}
      if(length(temp.vAB3.weight)==0){temp.vAB3.weight<-as.integer(0)}
      if(length(temp.mAL.weight)==0){temp.mAL.weight<-as.integer(0)}
      if (is.null(vAB3.mAL.pair)) {
        vAB3.mAL.pair <- c(vAB3.iter.id, mAL.iter.id, P1.iter.id, temp.vAB3.weight, temp.mAL.weight, temp.P1.type)
      } else {
        vAB3.mAL.pair <- rbind(vAB3.mAL.pair, c(vAB3.iter.id, mAL.iter.id, P1.iter.id, temp.vAB3.weight, temp.mAL.weight, temp.P1.type))
      }
      if (temp.P1.type == '922722') {
        stop("Execution stopped because temp.P1.type is '922722'.")}
      
    }
  }
}



vAB3.mAL.pair.df <- data.frame(
  vAB3.id = vAB3.mAL.pair[,1]%>%as.integer(),
  mAL.id = vAB3.mAL.pair[,2]%>%as.integer(),
  P1.id = vAB3.mAL.pair[,3]%>%as.integer(),
  vAB3.weight = vAB3.mAL.pair[,4]%>%as.integer(),
  mAL.weight = vAB3.mAL.pair[,5]%>%as.integer(),
  mAL_vAB3_ratio = as.integer(vAB3.mAL.pair[,5])/as.integer(vAB3.mAL.pair[,4]),
  vAB3_mAL_ratio = as.integer(vAB3.mAL.pair[,4])/as.integer(vAB3.mAL.pair[,5]),
  P1.type = vAB3.mAL.pair[,6]
)

total.inputs.all.P1 <- cf_partners(cf_ids(malecns=vAB3.mAL.pair.df%>%
                                            pull(P1.id)%>%
                                            unique()),partners = 'inputs',threshold = synapse_cutoff)%>%
  group_by(post_id)%>%
  summarise(total.input.P1=sum(weight))

total.outputs.all.mAL <- cf_partners(cf_ids(malecns=vAB3.mAL.pair.df%>%
                                              pull(mAL.id)%>%
                                              unique()),partners = 'outputs',threshold = synapse_cutoff)%>%
  group_by(pre_id)%>%
  summarise(total.output.mAL=sum(weight))

total.outputs.all.vAB3 <- cf_partners(cf_ids(malecns=vAB3.mAL.pair.df%>%
                                               pull(vAB3.id)%>%
                                               unique()),partners = 'outputs',threshold = synapse_cutoff)%>%
  group_by(pre_id)%>%
  summarise(total.output.vAB3=sum(weight))

vAB3.mAL.pair.df <- vAB3.mAL.pair.df %>%
  left_join(total.outputs.all.mAL, by = c("mAL.id" = "pre_id"))%>%
  left_join(total.outputs.all.vAB3, by = c("vAB3.id" = "pre_id"))%>%
  left_join(total.inputs.all.P1, by = c("P1.id" = "post_id"))%>%
  mutate(mAL.ratio.of.output = mAL.weight / total.output.mAL,
         vAB3.ratio.of.output = vAB3.weight / total.output.vAB3,
         mAL.ratio.of.input = mAL.weight / total.input.P1,
         vAB3.ratio.of.input = vAB3.weight / total.input.P1)


vAB3.mAL.pair.df.dropinfand1<-vAB3.mAL.pair.df%>%
  filter(!is.infinite(mAL_vAB3_ratio))%>%
  filter(mAL_vAB3_ratio!=0)

#plotting absolute weight
ggplot(vAB3.mAL.pair.df, aes(x = vAB3.weight, y = mAL.weight)) +
  geom_point(color = "blue", size = 1,alpha=0.1) +  # Add points
  labs(
    title = "Scatter Plot of mAL and vAB3 pair input to P1s",
    x = 'vAB3.weight',
    y ='mAL.weight'
  ) +
  geom_abline(alpha=0.2)+
  theme_minimal()




ggplot(vAB3.mAL.pair.df%>%
         pivot_longer(cols = c(vAB3.weight, mAL.weight), names_to = "input_type", values_to = "weight")%>%
         arrange(P1.type), aes(x = P1.type, y = weight, fill =  input_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of pairwise input to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
       x = "P1.type",
       y = "vAB3.weight") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plotting of mean weight per P1.type
vAB3.mAL.pair.df%>%
  group_by(P1.type) %>%
  summarize(per_type_total_vAB3=mean(vAB3.weight),per_type_total_mAL=mean(mAL.weight)) %>%
  ggplot(., aes(x = per_type_total_vAB3, y = per_type_total_mAL)) +
  geom_point(color = "blue", size = 1,alpha=1) +  # Add points
  labs(
    title = "Scatter Plot of mean pairwise input to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
    x = 'vAB3.weight',
    y ='mAL.weight'
  ) +
  geom_text(aes(label = P1.type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 1) +
  geom_abline(alpha=0.2)+
  theme_minimal()



#plotting total weight ratio

ggplot(vAB3.mAL.pair.df.dropinfand1, aes(x=mAL_vAB3_ratio)) +
  geom_density(fill = "blue", alpha = 0.3) +  # KDE plot with color fill
  labs(
    title = "Kernel Density Estimate (KDE) of mAL input per vAB3 input",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()

ggplot(vAB3.mAL.pair.df.dropinfand1 %>% filter(P1.type=='P1_3c'), aes(x=vAB3_mAL_ratio)) +
  geom_density(fill = "blue", alpha = 0.3) +  # KDE plot with color fill
  labs(
    title = "Kernel Density Estimate (KDE) of vAB3 input per mAL input",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()




#plotting of input ratio mAL vs vAB3


ggplot(vAB3.mAL.pair.df, aes(x = vAB3.ratio.of.input, y = mAL.ratio.of.input)) +
  geom_point(color = "blue", size = 1,alpha=0.1) +  # Add points
  labs(
    title = "Scatter Plot of mAL and vAB3 pair input ratio to P1s",
    x = 'vAB3.ratio.of.input',
    y ='mAL.ratio.of.input'
  ) +
  geom_abline(alpha=0.2)+
  theme_minimal()




ggplot(vAB3.mAL.pair.df%>%
         pivot_longer(cols = c(vAB3.ratio.of.input,  mAL.ratio.of.input), names_to = "input_type", values_to = "weight")%>%
         arrange(P1.type), aes(x = P1.type, y = weight, fill =  input_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of pairwise input ratio to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
       x = "P1.type",
       y = "ratio.of.input") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plotting of mean input ratio per mAL vs vAB 
vAB3.mAL.pair.df%>%
  group_by(P1.type) %>%
  summarize(per_type_total_vAB3=mean(vAB3.ratio.of.input),per_type_total_mAL=mean(mAL.ratio.of.input)) %>%
  ggplot(., aes(x = per_type_total_vAB3, y = per_type_total_mAL)) +
  geom_point(color = "blue", size = 1,alpha=1) +  # Add points
  labs(
    title = "Scatter Plot of mean pairwise input ratio to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
    x = 'mean.vAB3.ratio.of.input',
    y ='mean.mAL.ratio.of.input'
  ) +
  geom_text(aes(label = P1.type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 0.001) +
  geom_abline(alpha=0.2)+
  theme_minimal()

#plotting summed input ratio per P1.type
vAB3.mAL.pair.df%>%
  group_by(vAB3.id) %>%
  summarize(per_type_total_vAB3=sum(vAB3.ratio.of.input),per_type_total_mAL=sum(mAL.ratio.of.input)) %>%
  ggplot(., aes(x = per_type_total_vAB3, y = per_type_total_mAL)) +
  geom_point(color = "blue", size = 1,alpha=1) +  # Add points
  labs(
    title = "Scatter Plot of summed pairwise input ratio to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
    x = 'vAB3.ratio.of.input',
    y ='summed.mAL.ratio.of.input'
  ) +
  geom_abline(alpha=0.2)+
  theme_minimal()





#plotting of output ratio mAL vs vAB3


ggplot(vAB3.mAL.pair.df, aes(x = vAB3.ratio.of.output, y = mAL.ratio.of.output)) +
  geom_point(color = "blue", size = 1,alpha=0.1) +  # Add points
  labs(
    title = "Scatter Plot of mAL and vAB3 pair output ratio to P1s",
    x = 'vAB3.ratio.of.output',
    y ='mAL.ratio.of.output'
  ) +
  geom_abline(alpha=0.2)+
  theme_minimal()




ggplot(vAB3.mAL.pair.df%>%
         pivot_longer(cols = c(vAB3.ratio.of.output,  mAL.ratio.of.output), names_to = "input_type", values_to = "weight")%>%
         arrange(P1.type), aes(x = P1.type, y = weight, fill =  input_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of pairwise output ratio to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
       x = "P1.type",
       y = "ratio.of.output") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plotting of mean output ratio per mAL vs vAB 
vAB3.mAL.pair.df%>%
  group_by(P1.type) %>%
  summarize(per_type_total_vAB3=mean(vAB3.ratio.of.output),per_type_total_mAL=mean(mAL.ratio.of.output)) %>%
  ggplot(., aes(x = per_type_total_vAB3, y = per_type_total_mAL)) +
  geom_point(color = "blue", size = 1,alpha=1) +  # Add points
  labs(
    title = "Scatter Plot of mean pairwise output ratio to P1.type (one 1x vAB3 with 1x vAB3 target mAL)",
    x = 'mean.vAB3.ratio.of.output',
    y ='mean.mAL.ratio.of.output'
  ) +
  geom_text(aes(label = P1.type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 0.0001) +
  geom_abline(alpha=0.2)+
  theme_minimal()


#vAB3 input ratio on P1 vs input of all their children onto that P1 type
vAB3.mAL.pair.df%>%
  group_by(P1.id,vAB3.id) %>%
  summarize(ratio.input.vAB3=max(vAB3.ratio.of.input),sum.ratio.input.mAL=sum(mAL.ratio.of.input),P1.type=P1.type) %>%
  ggplot(., aes(x = ratio.input.vAB3, y = sum.ratio.input.mAL)) +
  geom_point(color = "blue", size = 1,alpha=1) +  # Add points
  labs(
    title = "Scatter Plot of vAB3 input ratio onto a unique P1 vs the sum input ratio of the children of that vAB3",
    x = 'ratio.input.vAB3',
    y ='sum.ratio.input.mAL'
  ) +
  geom_text(aes(label = P1.type), size = 3, hjust = 0, vjust = 0.5, nudge_x = 0.002) +
  geom_abline(alpha=0.2)+
  theme_minimal()

#mean vAB3 input ratio on P1 vs input of all their children onto that P1 type
vAB3.mAL.pair.df%>%
  group_by(P1.id,vAB3.id) %>%
  summarize(ratio.input.vAB3=max(vAB3.ratio.of.input),sum.ratio.input.mAL=sum(mAL.ratio.of.input),P1.type=P1.type) %>%
  ungroup()%>%
  group_by(P1.type)%>%
  summarize(mean.ratio.input.vAB3=mean(ratio.input.vAB3),mean.sum.ratio.input.mAL=mean(sum.ratio.input.mAL),P1.type=P1.type) %>%
  ggplot(., aes(x = mean.ratio.input.vAB3, y = mean.sum.ratio.input.mAL)) +
  geom_point(color = "blue", size = 3,alpha=1) +  # Add points
  labs(
    title = "Scatter Plot of vAB3 input ratio onto a P1.type vs the sum input ratio of the children of that vAB3",
    x = 'mean.ratio.input.vAB3',
    y ='mean.sum.ratio.input.mAL'
  ) +
  geom_text(aes(label = P1.type), size = 5, hjust = 0, vjust = 0.5, nudge_x = 0.002) +
  geom_abline(alpha=0.2)+
  theme_minimal()





