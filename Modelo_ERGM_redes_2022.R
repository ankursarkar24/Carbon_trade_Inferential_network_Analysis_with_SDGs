#install.packages("statnet")
install.packages("xtable")
library("igraph")
library("tidyverse")
library("readxl")
library("openxlsx")
library("ergm")
library("statnet")
library("mice")
library("ggplot2")
library("GGally")
library("scales")
library("xtable")

# @Importando datos #######################################################################################################################################################################
data_net_2019_2020_2021_2022_2023<- read_excel("E:/ankursarkar-24/Red-de-comercio-mundial-main_trade_using_R/carbon trade sdg/data_net_2019_2020_2021_2022_2023.xlsx")
data_net_2019_2020_2021_2022_2023 <- data_net_2019_2020_2021_2022_2023

# Aristas
data_net_2022 <- data_net_2019_2020_2021_2022_2023 %>%
                 select("ReporterISO3", "PartnerISO3", "2022")

# Atributos 
Atributos_2022 <- read_excel("Paises_atributos_imputed_2022.xlsx")
Atributos_2022 <- Atributos_2022[ , !(names(Atributos_2022) %in% c("Time", "Time Code", "Country Name"))]

# @Modelo ERGM 2022 #######################################################################################################################################################################
set.seed(0)

to_network_2022 <- data_net_2022[, c("ReporterISO3", "PartnerISO3")]
to_network_2022$ReporterISO3 <- as.character(data_net_2022$ReporterISO3)
to_network_2022$PartnerISO3 <- as.character(data_net_2022$PartnerISO3)

g_igraph_2022 <- graph_from_data_frame(data_net_2022, directed = T)

adjacency_matrix_2022 <- get.adjacency(g_igraph_2022)

nw_2022 <- network(adjacency_matrix_2022, directed = TRUE)
class(nw_2022); nw_2022

x11()
par(mfrow = c(1,1), mar = 0.2*c(1,1,1,1))
set.seed(42)
plot(nw_2022, label = network.vertex.names(nw_2022))

x11(width = 12, height = 12) # Opens a much larger window to give nodes room to spread
par(mar = c(1, 1, 1, 1))     # Give a little padding so edge names aren't cut off

set.seed(42)
plot(nw_2022, 
     label = network.vertex.names(nw_2022),
     label.cex = 0.5,            # Make text significantly smaller
     label.pos = 5,              # Center labels directly inside/on the nodes
     vertex.cex = 2,             # Make nodes slightly larger to act as a backing
     vertex.col = rgb(0.2,0.4,0.6,0.3), # Make nodes semi-transparent
     edge.col = rgb(0.5,0.5,0.5,0.15),  # Make edges highly transparent so they don't bury text
     mode = "kamadakawai"        # Often spreads dense nodes better than Fruchterman-Reingold
)

library(GGally)
library(ggplot2)
library(ggrepel) # Crucial for preventing text overlap

set.seed(42)
ggnet2(nw_2022, 
       label = TRUE, 
       label.size = 0.01,          # Small, clean font
       node.size = 4, 
       node.color = "steelblue",
       edge.color = "grey90",     # Faint edges so labels stand out
       edge.size = 0.3) +
  geom_text_repel(aes(label = network.vertex.names(nw_2022)), 
                  size = 3, 
                  segment.color = "grey50", # Draws a tiny line from node to label if pushed far
                  force = 1) +              # Forces labels to repel each other
  theme_void()


## @Adding attributes #######################################################################################################################################################################

set.vertex.attribute(nw_2022, 'GDP per capita', as.numeric(Atributos_2022$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`))
set.vertex.attribute(nw_2022, 'GDP constant 2015', as.numeric(Atributos_2022$`GDP (constant 2015 US$) [NY.GDP.MKTP.KD]`))
set.vertex.attribute(nw_2022, 'Total greenhouse gases', as.numeric(Atributos_2022$`Total greenhouse gas emissions including LULUCF (Mt CO2e) [EN.GHG.ALL.LU.MT.CE.AR5]`))
set.vertex.attribute(nw_2022, 'Total greenhouse gases Pc', as.numeric(Atributos_2022$`Total greenhouse gas emissions excluding LULUCF per capita (t CO2e/capita) [EN.GHG.ALL.PC.CE.AR5]`))
set.vertex.attribute(nw_2022, 'Carbon dioxide emissions', as.numeric(Atributos_2022$`Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita) [EN.GHG.CO2.PC.CE.AR5]`))
set.vertex.attribute(nw_2022, 'Merchandise trade', as.numeric(Atributos_2022$`Merchandise trade (% of GDP) [TG.VAL.TOTL.GD.ZS]`))
set.vertex.attribute(nw_2022, 'Trade pct GDP', as.numeric(Atributos_2022$`Trade (% of GDP) [NE.TRD.GNFS.ZS]`))
set.vertex.attribute(nw_2022, 'Statistical capacity', as.numeric(Atributos_2022$`Statistical performance indicators (SPI): Overall score (scale 0-100) [IQ.SPI.OVRL]`))
set.vertex.attribute(nw_2022, 'Industry pct GDP', as.numeric(Atributos_2022$`Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]`))
set.vertex.attribute(nw_2022, 'Renewable energy', as.numeric(Atributos_2022$`Renewable energy consumption (% of total final energy consumption) [EG.FEC.RNEW.ZS]`))

nw_2022
ergm_model_2022 <- formula(nw_2022 ~ edges +
                             nodemain('GDP per capita') +
                             nodemain('GDP constant 2015') +
                             nodemain('Total greenhouse gases') +
                             nodemain('Total greenhouse gases Pc') +
                             nodemain('Carbon dioxide emissions') +
                             nodemain('Merchandise trade') +
                             nodemain('Trade pct GDP') +
                             nodemain('Statistical capacity') +
                             nodemain('Industry pct GDP') +
                             nodemain('Renewable energy') +
                             nodematch('GDP per capita') +
                             nodematch('GDP constant 2015') +
                             nodematch('Total greenhouse gases') +
                             nodematch('Carbon dioxide emissions') +
                             nodematch('Merchandise trade') +
                             nodematch('Trade pct GDP') +
                             nodematch('Statistical capacity') +
                             nodematch('Industry pct GDP') +
                             nodematch('Renewable energy'))
summary(ergm_model_2022)
set.seed(42)
ergm_fit_2022 <- ergm(formula = ergm_model_2022)
summary(ergm_fit_2022)
ergm_fit_2022$coefficients

anova(ergm_fit_2022)

## @Convergencia ############################################################################################

mcmc.diagnostics(ergm_fit_2022)

## @simulación ##############################################################################################

## @simulación ##############################################################################################
start_time <- proc.time()
sim <- simulate(object = ergm_fit_2022, nsim = 1000, seed = 42)
end_time <- proc.time()
time_taken <- end_time - start_time; time_taken
summary(sim)
rbind("Valoes obs." = summary(ergm_fit_2022),
      "Media  sim." = colMeans(attr(sim, "stats")))

# Bondad de ajuste del modelo
start_time <- proc.time()
ergm_gof_2022 <- gof(object = ergm_fit_2022)
end_time <- proc.time()
ergm_gof_2022
time_taken <- end_time - start_time; time_taken

# gráficos
x11()
par(mfrow = c(3,2), mar = c(4,4,4,2))
plot(ergm_gof_2022)


