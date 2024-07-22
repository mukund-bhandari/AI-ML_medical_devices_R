library(ggplot2)
library(dplyr)
library(devtools)
library(ggsankey)

###
set.seed(111)
# Sample data
data_CT <- read_excel("date_diff.xlsx", sheet=1)
data_CT$days_passed <- as.numeric(difftime(data_CT$Decision_Date, data_CT$Date_Received,  units = "days"))

data_CT <- data_CT %>%
  select(2, 3,9, 28:36) %>%
  filter(!Study_Type == is.na(Study_Type)) %>%
  rename("Panel"=3)
#################
TotalCount = nrow(data_CT)
###############
table(data_CT$Panel)

data_CT$Panel <- factor(data_CT$Panel, levels= c("Cardiovascular", "Radiology", "Neurology", "Ophthalmic" , 
                                       "Clinical Chemistry" , "Hematology")
)

##for ggshankey
df<-data_CT%>%
  #make_long(Approval_Year, Panel)
  make_long(Approval_Year, Panel, Country_CT)

### tally
####
dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()

dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n/TotalCount)

# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

######
# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      , label = paste0(node," n=", n, '(',  round(pct* 100,1), '%)' ))
)

pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 2, color = "black", fill= "white", hjust = -0.1)

pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "FDA approved AIML Enabled Medical with Clinical Trial")
pl <- pl + labs(subtitle = "2023 Update")
pl <- pl + labs(caption = "Joshi et. al, 2022")
pCT5 <- pl + labs(fill = 'Nodes')



