library(neuprintr)
library(arrow)
library(malecns)
library(bit64)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(Matrix)
library(cowplot)
library(coconat)
library(coconatfly)
library(ggrepel)
library(plotly)
library(orca)
library(reticulate)
library(plotly)
py_require(c("kaleido"))
py_require(c("plotly"))
synapse.threshold = 5
mba<-mcns_body_annotations()
mba.static<-mcns_body_annotations()%>%  
  mutate(type = str_remove_all(as.character(type), "[()]"))
mba<-mba%>%mutate(type=ifelse(type=='',NA,type))
mba <- mba %>%
  mutate(
    type = case_when(
      grepl("putative_ppk23", receptor_type) ~ "m-cell",
      grepl("putative_ppk25", receptor_type) ~ "f-cell",
      TRUE ~ type
    )
  )%>%
  mutate(
    type = str_remove_all(as.character(type), "[()]")
  )

vAB3.types <- c("AN09B017")
PPN1.types <- c('AN05B102')

vAB3.ids <- mba %>%filter(type %in% vAB3.types) %>% pull(bodyid)
PPN1.ids <- mba %>%filter(type %in% PPN1.types) %>% pull(bodyid)



bake.pie <- function(ids,
                     name='default',
                     threshold       = 1,
                     synapse.threshold = 5,
                     connection.partners = 'o',
                     other           = FALSE) {
  # 1) pull your partner summary with the correct synapse.threshold
  ps <- cf_partner_summary(
    cf_ids(malecns = ids),
    partners  = connection.partners,
    threshold = synapse.threshold
  ) %>%
    # 2) create a proper "type" column based on your scalar flag
    mutate(
      type = if (connection.partners == 'o') type.post
      else                 type.pre
    ) %>%
    # 3) group & roll up
    group_by(type) %>%
    summarize(
      weight = sum(weight),
      .groups = 'drop'
    ) %>%
    # 4) compute the percentages
    mutate(frac_weight = weight / sum(weight) * 100) %>%
    arrange(desc(frac_weight))
  
  # 5) flag the small slices to collapse
  ps <- ps %>% mutate(group = frac_weight <= threshold)
  
  # split into main vs. to-collapse
  main_slices  <- ps %>% filter(!group)
  collapsed    <- ps %>% filter(group)
  
  # 6) if there's anything to collapse, build an "other" row
  if (nrow(collapsed) > 0) {
    other_row <- collapsed %>%
      summarize(
        type        = "other",
        weight      = sum(weight),
        frac_weight = sum(frac_weight),
        .groups     = 'drop'
      )
    ps_collapsed <- bind_rows(main_slices, other_row) %>%
      arrange(desc(frac_weight))
  } else {
    ps_collapsed <- main_slices
  }
  
  # 7) choose which data.frame to plot
  df_to_plot <- if (other) ps_collapsed else main_slices
  slice_levels <- df_to_plot %>% 
    arrange(desc(frac_weight)) %>% 
    pull(type)
  df_to_plot <- df_to_plot %>%
    mutate(type = factor(type, levels = slice_levels))
  legend_levels <- sort(unique(slice_levels))
  

  

  p<-ggplot(df_to_plot, aes(x = "", y = frac_weight, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 
  
  fig <- plot_ly(type='pie', labels=df_to_plot$type, values=df_to_plot$frac_weight, 
                 textinfo='label+percent',
                 insidetextorientation='radial')%>% layout(title = paste(name,'Total synapses = ',sum(df_to_plot$weight)),  showlegend = F,
                                                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                           margin = list(
                                                             l = 100,    # left margin
                                                             r = 100,    # right margin
                                                             b = 100,    # bottom margin
                                                             t = 100     # top margin
                                                           ))

  
  fig <- config(
    fig,
    staticPlot    = TRUE,
    displayModeBar = FALSE
  )
  
  out_dir  <- file.path(getwd(), "pie.connectomics/imgs")
  outfile  <- file.path(out_dir, paste0(name, ".png"))
  saved_path <- save_image(fig, file = outfile, scale = 4,width=1000,height = 1000)

  
  return(list(fig,df_to_plot))
  

}

vAB3.upstream = bake.pie(vAB3.ids,name='vAB3 in',other=T,connection.partners = 'i',threshold = 2)
PPN1.upstream = bake.pie(PPN1.ids,name='PPN1 in',other=T,connection.partners = 'i',threshold = 2)
vAB3.downstream = bake.pie(vAB3.ids,name='vAB3 out',other=T,connection.partners = 'o',threshold = 2)
PPN1.downstream = bake.pie(PPN1.ids,name='PPN1 out',other=T,connection.partners = 'o',threshold = 1)
PPN1.downstream[[2]]$pre.type = PPN1.types
vAB3.downstream[[2]]$pre.type = vAB3.types
PPN1.downstream[[2]]$lvl = 0
vAB3.downstream[[2]]$lvl = 0
PPN1.upstream[[2]]$lvl = -1
vAB3.upstream[[2]]$lvl = -1
vAB3.upstream[[2]]<-vAB3.upstream[[2]]%>%
  rename(pre.type=type)%>%
  mutate(type=vAB3.types)
PPN1.upstream[[2]]<-PPN1.upstream[[2]]%>%
  rename(pre.type=type)%>%
  mutate(type=PPN1.types)


#downstream.type.lvl1 <- rbind(vAB3.downstream[[2]])%>% #,PPN1.downstream[[2]]
downstream.type.lvl1 <- rbind(PPN1.downstream[[2]])%>%
  filter(type!='other')%>%
  pull(type)%>%
  unique()

lvl1.cfps<-tb0 <- tibble(
  type = character(),
  weight = integer(),
  frac_weight = double(),
  group = logical(),
  pre.type=character()
)



for(t in downstream.type.lvl1){
  temp.ids <- mba %>%filter(type %in% c(t)) %>% pull(bodyid)
  if(length(temp.ids)==0){
    temp.ids <- mba %>%filter(flywire_type %in% c(t)) %>% pull(bodyid)
  }
  a = bake.pie(temp.ids,name=paste(t,'lvl1','out'),other=T,connection.partners = 'o',threshold = 2)
  a[[2]]$pre.type = t
  a[[2]]$lvl = 1

  lvl1.cfps<-rbind(lvl1.cfps,a[[2]])
  
  
}

downstream.type.lvl2 <- lvl1.cfps%>%
  filter(type!='other')%>%
  pull(type)%>%
  unique()

lvl2.cfps <- empty_tbl <- tibble(
  pre.type     = character(),    
  type   = character(), 
  weight  = integer(), 
  frac_weight = double(),
  group = logical()
)

for(t in downstream.type.lvl2){
  temp.ids <- mba %>%filter(type %in% c(t)) %>% pull(bodyid)
  if(length(temp.ids)==0){
    temp.ids <- mba %>%filter(flywire_type %in% c(t)) %>% pull(bodyid)
  }
  if(length(temp.ids)==0){
    temp.ids <- mba.static %>%filter(type %in% c(t)) %>% pull(bodyid)
  }
  
  a = bake.pie(temp.ids,name=paste(t,'lvl2','out'),other=T,connection.partners = 'o',threshold = 5)
  a[[2]]$pre.type = t
  a[[2]]$lvl = 2
  lvl2.cfps<-rbind(lvl2.cfps,a[[2]])
  
}


#all.cfps <- rbind(vAB3.upstream[[2]],PPN1.upstream[[2]],vAB3.downstream[[2]],PPN1.downstream[[2]],lvl1.cfps,lvl2.cfps)%>%
#all.cfps <- rbind(vAB3.upstream[[2]],vAB3.downstream[[2]],lvl1.cfps,lvl2.cfps)%>%
all.cfps <- rbind(PPN1.upstream[[2]],PPN1.downstream[[2]],lvl1.cfps,lvl2.cfps)%>%
  filter(!type=='other',!pre.type=='other')%>%
  rename(from=pre.type,to=type)%>%
  select(-group)%>%
  distinct()%>%
  group_by(from, to, weight, frac_weight) %>%
  slice_min(order_by = lvl, with_ties = FALSE) %>%
  ungroup()

nt <- mba.static %>%
  mutate(nt=coalesce(consensus_nt,predicted_nt,celltype_predicted_nt))%>%
  group_by(type, nt) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!is.na(nt)) %>%           # Remove NA before ranking
  group_by(type) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE) %>%  # Take the most frequent (tie-breaking arbitrarily)
  right_join(mba.static %>% distinct(type), by = "type") %>%         # Ensure all types included
  mutate(nt = ifelse(is.na(nt), NA, nt))%>%
  mutate(if_else(is.na(nt),'unclear',nt))

nt.fw <- mba.static %>%
  mutate(nt=coalesce(consensus_nt,predicted_nt,celltype_predicted_nt))%>%
  group_by(flywire_type, nt) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!is.na(nt)) %>%           # Remove NA before ranking
  group_by(flywire_type) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE) %>%  # Take the most frequent (tie-breaking arbitrarily)
  right_join(mba.static %>% distinct(flywire_type), by = "flywire_type") %>%         # Ensure all types included
  mutate(nt = ifelse(is.na(nt), NA, nt))%>%
  mutate(if_else(is.na(nt),'unclear',nt))




g<- graph_from_data_frame(d = all.cfps%>%select(from,to), directed = TRUE, vertices = NULL)
E(g)$frac_weight <- all.cfps$frac_weight
E(g)$weight <- all.cfps$weight

nt.map <- setNames(nt$nt, nt$type)
V(g)$neurotransmitter <- nt.map[V(g)$name]

nt.map.fw <- setNames(nt.fw$nt, nt.fw$flywire_type)
na.idx <- which(is.na(V(g)$neurotransmitter))
V(g)$neurotransmitter[na.idx] <- nt.map.fw[V(g)$name[na.idx]]

lvl <-all.cfps%>%group_by(from)%>%summarize(lvl=first(lvl))
lvl.map <- setNames(lvl$lvl, lvl$from)
V(g)$lvl <- lvl.map[V(g)$name]
na.idx <- which(is.na(V(g)$lvl))
repeat {
  na.idx <- which(is.na(V(g)$lvl))
  if (length(na.idx) == 0) break  # all done
  
  updated <- FALSE  # flag to track if we update anything this round
  
  for (v in na.idx) {
    incoming <- neighbors(g, v, mode = "in")
    parent_lvls <- V(g)$lvl[incoming]
    
    if (any(!is.na(parent_lvls))) {
      V(g)$lvl[v] <- min(parent_lvls, na.rm = TRUE) + 1
      updated <- TRUE
    }
  }
  
  if (!updated) break  # stop if no more progress is possible
}
edge_ends <- ends(g, E(g), names = FALSE)

# Extract levels of start and end nodes
from_lvls <- V(g)$lvl[edge_ends[, 1]]
to_lvls   <- V(g)$lvl[edge_ends[, 2]]

# Assign delta level to each edge
E(g)$delta_lvl <- to_lvls - from_lvls

RCy3::createNetworkFromIgraph(g)


