library(tidyr)
library(dplyr)
library(malecns)
library(arrow)
library(fafbseg)
library(coconatfly)

#to find the M-cells look at the inputs of mAL that input into P1
P1.input.mAL.unique.ids <- cf_partners(cf_ids(malecns="/P1.+"), partners = 'in',threshold=5) %>%
  filter(grepl("^mAL_(([a-zA-Z]([0-9]|[1-9][0-9])[a-zA-Z]?))$", type)) %>%
  pull(pre_id)

P1.input.mAL.input.ascending <- cf_partners(cf_ids(malecns=P1.input.mAL.unique.ids), partners = 'in',threshold=5) %>%
  filter(superclass=='ascending_neuron')
P1.input.mAL.input.sensory_ascending <- cf_partners(cf_ids(malecns=P1.input.mAL.unique.ids), partners = 'in',threshold=5) %>%
  filter(superclass=='sensory_ascending')