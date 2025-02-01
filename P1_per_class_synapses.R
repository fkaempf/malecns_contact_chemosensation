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

synapse_cutoff = 5
P1_partners_per_id <- cf_partners(cf_ids(malecns="/P1.+"),partners='in',threshold=synapse_cutoff)%>%
  group_by(post_id)%>%
  summarize(synapse_count=n())

P1.mapping.id2type <- cf_meta(cf_ids(malecns=P1_partners_per_id %>% 
                                       pull(post_id)%>%unique()))%>%
  select(id,type)%>%
  mutate(id=as.integer(id))

result <- left_join(P1_partners_per_id, P1.mapping.id2type, by = c('post_id'='id'))
result_mean <- result%>%group_by(type)%>%summarize(mean_per_type=mean(synapse_count))

ggplot(result, aes(x = synapse_count)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Kernel Density Estimate (KDE) Plot", x = "Value", y = "Density") +
  geom_vline(xintercept = 48, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = 48, y = 0.018, label = "Mean synapses P1_5b", color = "black", angle = 90, vjust = -0.5, hjust = 1) +
  theme_minimal()

ggplot(result, aes(x = synapse_count)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7)  +
  labs(title = "Hist of input synapses", x = "Value", y = "n") +  
  geom_vline(xintercept = 48, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = 48, y = 10, label = "Mean synapses P1_5b", color = "black", angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(xintercept = 64, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = 64, y = 10, label = "Mean synapses P1_14a", color = "black", angle = 90, vjust = -0.5, hjust = 1) +
  theme_minimal()



ggplot(result_mean, aes(x = mean_per_type)) +
  geom_density(fill = "red", alpha = 0.4) +
  labs(title = "KDE of mean input synapses per P1 type", x = "Value", y = "Density") +  
  geom_vline(xintercept = 48, color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = 48, y = 0.018, label = "Mean synapses P1_5b", color = "black", angle = 90, vjust = -0.5, hjust = 1) +
  theme_minimal()

ggplot(result_mean, aes(x = mean_per_type)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black", alpha = 0.7)  +
  labs(title = "Hist of mean input synapses per P1 type", x = "Value", y = "Density") +  
  geom_vline(xintercept = 48, color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = 48, y = 7, label = "Mean synapses P1_5b", color = "black", angle = 90, vjust = -0.5, hjust = 1) +
  geom_vline(xintercept = 64, color = "blue", linetype = "dashed", size = 0.5) +
  annotate("text", x = 64, y = 10, label = "Mean synapses P1_14a", color = "black", angle = 90, vjust = -0.5, hjust = 1) +
  theme_minimal()
  