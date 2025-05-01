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
  
  out_dir  <- file.path(getwd(), "pie.connectomics")
  outfile  <- file.path(out_dir, paste0(name, ".png"))
  saved_path <- save_image(fig, file = outfile, scale = 4,width=1000,height = 1000)

  
  return(list(fig,df_to_plot))
  

}

bake.pie(vAB3.ids,name='vAB3 in',other=T,connection.partners = 'i',threshold = 2)
bake.pie(PPN1.ids,name='PPN1 in',other=T,connection.partners = 'i',threshold = 2)
vAB3.downstream = bake.pie(vAB3.ids,name='vAB3 out',other=T,connection.partners = 'o',threshold = 2)
PPN1.downstream = bake.pie(PPN1.ids,name='PPN1 out',other=T,connection.partners = 'o',threshold = 2)

downstream.type.lvl1 <- rbind(vAB3.downstream[[2]],PPN1.downstream[[2]])%>%
  filter(type!='other')%>%
  pull(type)%>%
  unique()

for(t in downstream.type.lvl1){
  temp.ids <- mba %>%filter(type %in% c(t)) %>% pull(bodyid)
  if(length(temp.ids)==0){
    temp.ids <- mba %>%filter(flywire_type %in% c(t)) %>% pull(bodyid)
  }
  bake.pie(temp.ids,name=paste(t,'lvl1','out'),other=T,connection.partners = 'o',threshold = 2)
}




