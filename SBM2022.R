install.packages("igraph")
install.packages("sand")
install.packages("readxl")
install.packages("tidyverse")
install.packages("openxlsx")
library(igraph)
library(sand)
library("readxl")
library("tidyverse")
library("openxlsx")
setwd("C:/Users/USUARIO/Desktop/Estadística/Maestría en Estadística/Social_networks/Proyecto/data/Initial_data")

# Data loading
data_attributes_blocks_2022 <- read_excel("Paises_atributos_imputed_2022.xlsx")
data_net <- read_excel("data_net_2019_2020_2021_2022_2023.xlsx")

country_codes <- read_excel("country_codes.xlsx")

years = c('2019', '2020', '2021', '2022', '2023')
top_exporters = list()
for (year in years) {
  top_exporters[[year]] <- data_net %>% group_by(ReporterISO3) %>% summarise(total_exports = sum(!!sym(year), na.rm = TRUE)) 
  colnames(top_exporters[[year]]) <- c(names(country_codes[2]), "total_exports")
  top_exporters[[year]] <- top_exporters[[year]] %>% filter(total_exports != 0)
}

# For loop for data processing (commented out in original)
# for (year in years) {
#   data_trade_years[[year]] <- data_trade_blocks[, c("ReporterISO3", "PartnerISO3", year)]
#   data_trade_years[[year]] <- data_trade_years[[year]][complete.cases(data_trade_years[[year]]),] # remove NA rows
# }

# Generate graph
g <- list()
for (year in years) {
  year_data <- data_net %>% select(ReporterISO3, PartnerISO3, !!year)
  year_data <- year_data %>% filter(!is.na(!!sym(year)))
  year_data <- year_data %>% filter(!!sym(year) != 0)
#  year_data[[year]] <- (year_data[[year]] - mean(year_data[[year]])) / sd(year_data[[year]])
  g[[year]] <- graph_from_data_frame(
    d = year_data,
    vertices = unique(c(year_data$ReporterISO3, year_data$PartnerISO3)),
    directed = TRUE
  )
  E(g[[year]])$weight <- year_data[[year]]
}

# To get model results for 2018, 2020, and 2022, change the value of the "year" variable to '2018', '2020', and '2022' respectively.

# =====================================================
# @ Model Setup
# =====================================================
year = '2023'

trade_graph <- g[[year]]

vcount(trade_graph)
V(trade_graph)
is.weighted(trade_graph)

# Package to fit SBMs
install.packages('blockmodels')
suppressMessages(suppressWarnings(library(blockmodels)))

# Adjacency matrix
A <- as.matrix(igraph::as_adjacency_matrix(trade_graph))

# Model formulation
set.seed(777)
trade.sbm <- blockmodels::BM_bernoulli(membership_type = "SBM", adj = A, verbosity = 0, plotting = "")
# trade.sbm <- BM_gaussian(membership_type = "SBM", adj = B, plotting = "")

# Estimation
estimation <- trade.sbm$estimate()

# Integrated Classification Likelihood (ICL)
ICLs <- trade.sbm$ICL
ICLs

# Optimal number of groups (classes)
Q <- which.max(ICLs)
Q

# Plotting the ICL
x11()
par(mfrow = c(1,1), mar = c(2.75,2.75,1.5,0.5), mgp = c(1.7,0.7,0))
plot(trade.sbm$ICL, xlab = "Number of Classes (Q)", ylab = "ICL", type = "b", pch = 16,
     main = "ICL Calculation for Each Number of Classes")
lines(x = c(Q,Q), y = c(min(ICLs), max(ICLs)), col = "red1", lty = 2)
lines(x = c(4,4), y = c(min(ICLs), ICLs[4]), col = "grey40", lty = 2)

# Estimated community membership probabilities
Z <- trade.sbm$memberships[[4]]$Z

# Class assignments
labs <- apply(X = Z, MARGIN = 1, FUN = which.max)
head(x = labs, n = 10)
tail(x = labs, n = 10)
length(labs)

# Summary of maximum probabilities
summary(Z[cbind(1:vcount(trade_graph), labs)])

# Vertices data frame
vertices_df <- data.frame("Country Code" = V(trade_graph)$name)
colnames(vertices_df) <- names(country_codes[2])

vertices_df <- left_join(x = vertices_df, y = country_codes, by = names(country_codes[2]))

clustering <- cbind(vertices_df, as.data.frame(labs)) 
clustering <- left_join(x = clustering, y = top_exporters[[year]], by = 'Country code')

top_clustering <- clustering %>%
  group_by(labs) %>%
  arrange(desc(total_exports)) %>%
  slice_head(n = 15)

sorted_clustering <- clustering[order(-clustering$total_exports),]

# excel_name = paste("clustering_", year, ".xlsx", sep = "")
# write.xlsx(clustering, excel_name)
# write.xlsx(top_clustering, "representative_countries_2018.xlsx")

# Size of communities
table(labs)
cl.cnts <- as.vector(table(labs))
nv = vcount(trade_graph)

# Group probabilities
alpha <- table(labs)/vcount(trade_graph)
round(alpha, 3)

# Group probabilities (sorted)
round(alpha[order(alpha, decreasing = TRUE)], 3)

# Interaction probability matrix
Pi <- trade.sbm$model_parameters[[4]]$pi
round(Pi, 3)

# Plotting the interaction matrix
x11()
corrplot::corrplot(main = "Interaction Probability Matrix", corr = Pi, type = "full", col.lim = c(0,1), method = "shade", addgrid.col = "gray90", tl.col = "black")

edges <- as_edgelist(trade_graph, names = FALSE)
neworder <- order(labs)
m <- t(matrix(order(neworder)[as.numeric(edges)], 2))

x11()
plot(1, 1, xlim = c(0, nv + 1), ylim = c(nv + 1, 0),
     type = "n", axes = FALSE, xlab = "Classes",
     ylab = "Classes",
     main = "Adjacency Matrix Organized by Classes")

rect(m[,2]-0.5, m[,1]-0.5, m[,2]+0.5, m[,1]+0.5, col = "blue1")
rect(m[,1]-0.5, m[,2]-0.5, m[,1]+0.5, m[,2]+0.5, col = "blue1")

cl.lim <- cl.cnts
cl.lim <- cumsum(cl.lim)[1:(length(cl.lim)-1)]+0.5

clip(0, nv+1, nv+1, 0)
abline(v = c(0.5, cl.lim, nv+0.5),
       h = c(0.5, cl.lim, nv+0.5), col = "grey40")

# View interactions between clusters
Pi.mat <- trade.sbm$model_parameters[[4]]$pi
Pi.mat[1,]
Pi.mat[2,]
Pi.mat[3,]
Pi.mat[4,]

top_clustering <- clustering %>%
  group_by(labs) %>%
  arrange(desc(total_exports)) %>%
  slice_head(n = 6)

data_net <- data_net %>%
  filter(data_net$ReporterISO3 %in% top_clustering$"Country code")

data_net <- data_net %>%
  filter(data_net$PartnerISO3 %in% top_clustering$"Country code")

data <- data_net %>% select(ReporterISO3, PartnerISO3, year)

subgraph <- graph_from_data_frame(
  d = data,
  vertices = unique(c(data$ReporterISO3, data$PartnerISO3)),
  directed = TRUE)

# Plot subgraph of top exporting countries
vcount(subgraph)
V(subgraph)$labs <- top_clustering$labs[match(V(subgraph)$name, top_clustering$`Country code`)]

Vcol <- ifelse(V(subgraph)$labs == 1, "#d7191c",
               ifelse(V(subgraph)$labs == 2, "#fdae61",
                      ifelse(V(subgraph)$labs == 3, "#abdda4",
                             ifelse(V(subgraph)$labs == 4, "#2b83ba", "grey"))))
E(subgraph)$color <- adjustcolor(Vcol[match(ends(subgraph, E(subgraph))[, 1], V(subgraph)$name)], 0.55)

# le = layout_on_sphere(subgraph)
lf = layout_with_fr(subgraph)
set.seed(777)
x11()
par(mar = c(0, 0, 1.5, 0))
plot(subgraph, main = "International Trade Network with Notable Representatives from Each Class", layout = lf,
     vertex.size = 10, vertex.frame.color = "grey30",
     vertex.color = adjustcolor(Vcol, 0.8),
     edge.color = E(subgraph)$color, vertex.label.cex = 0.98, vertex.label.dist = 1.5,
     edge.arrow.size = 0.6, edge.arrow.width = 0.55, edge.width = 0.5,
     edge.label = NA, edge.label.cex = 0.73, vertex.label.color = 'black')
legend("topright", legend = c("1", "2", "3", "4"),
       fill = c("#d7191c", "#fdae61", "#abdda4", "#2b83ba"),
       title = "Class", cex = 1, horiz = FALSE, box.lty = 0)

install.packages("maps")
install.packages("ggplot2")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(maps)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

set.seed(777)
clustering <- clustering %>%
  mutate(labs = ifelse(labs == 2, 3, ifelse(labs == 3, 2, labs)))

world <- ne_countries(scale = "medium", returnclass = "sf")

# Ensure Column Names Match
colnames(clustering)[1] <- "iso_a3"  # Renaming 'Country.Code' to 'iso_a3' to match the world dataset

# Convert world to a data frame
world_df <- as.data.frame(world)

# Merge Your Data with World Map Data
world_merged <- left_join(world_df, clustering, by = "iso_a3")

# Convert back to sf object
world_merged <- st_as_sf(world_merged)

colors <- c("1" = "#d7191c", "2" = "#fdae61", "3" = "#abdda4", "4" = "#2b83ba")

set.seed(777)
x11()
ggplot(data = world_merged) +
  geom_sf(aes(fill = as.factor(labs))) +
  scale_fill_manual(values = alpha(colors, 0.7), na.value = "grey50", guide = guide_legend(title = "Classes")) +
  theme_minimal() +
  labs(title = paste("Map of Country Classes in the Trade Network. Year", year, sep = " "),
       subtitle = "Each color represents a class") +
  theme(legend.position = "bottom")