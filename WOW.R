# =================================================================================================
# 1. LIBRARIES & SETUP
# =================================================================================================
# If packages are missing, run: 
# install.packages(c("tidyverse", "igraph", "readxl", "openxlsx", "VIM", "knitr", 
#                    "kableExtra", "caret", "gridExtra", "visNetwork", "ggraph", "tidygraph", "viridis"))

library("tidyverse")
library("igraph")
library("readxl")
library("openxlsx")
library("VIM")
library("knitr")
library("kableExtra")
library("caret")
library("gridExtra")
library("visNetwork")
library("ggraph")
library("tidygraph")
library("viridis")

# Set working directory
setwd("C:/Users/USUARIO/Desktop/Estadística/Maestría en Estadística/Social_networks/Proyecto/data/Initial_data")

# =================================================================================================
# 2. DATA LOADING & PREPROCESSING
# =================================================================================================
initial_data  <- read_csv("TradeCarbonWorld.csv")
country_codes <- read_excel("country_codes.xlsx")
atributos     <- read_excel("P_Data_Extract_From_World_Development_Indicators_erg_sbm.xlsx")[1:749,]

# Process qualitative variables
cualitative_atributos <- read_excel("Cualitative_variables.xlsx") %>% 
  select('Country Code', 'landlocked', 'continent', 'langoff_1') %>%
  mutate(continent = recode(continent, "America" = 1, "Asia" = 2, 'Africa' = 3, 'Europe' = 4, 'Pacific' = 5)) %>%
  distinct(`Country Code`, .keep_all = TRUE)

# Merge attributes
atributos <- left_join(x = atributos, y = cualitative_atributos, by = 'Country Code') 

# Build edge-list dataset for 2019-2023
years <- list("2019" = "2019 in 1000 USD", "2020" = "2020 in 1000 USD", 
              "2021" = "2021 in 1000 USD", "2022" = "2022 in 1000 USD", "2023" = "2023 in 1000 USD")
data_net <- data.frame()

for (year in names(years)) {
  year_export <- years[[year]]
  year_data <- initial_data %>%
    filter(TradeFlowName == "Export") %>%
    group_by(ReporterISO3, PartnerISO3) %>%
    summarise(!!year := sum(!!sym(year_export)), .groups = 'drop')
  
  if (nrow(data_net) == 0) {
    data_net <- year_data
  } else {
    data_net <- full_join(data_net, year_data, by = c("ReporterISO3", "PartnerISO3"))
  }
}

# Filtering out non-matching/excluded countries
ISO <- country_codes$`Country code`
`%notin%` <- Negate(`%in%`)

data_net <- data_net %>%
  filter(ReporterISO3 %in% ISO & PartnerISO3 %in% ISO)

Paises_atributos_base_inicial <- atributos %>% 
  filter(`Country Code` %in% ISO)

# Segment attributes list by year
Paises_atributos_Segmentada_x_años <- list()
for (year in names(years)) {
  sub_df <- Paises_atributos_base_inicial[Paises_atributos_base_inicial$Time == as.character(year), ]
  sub_df <- sub_df[order(sub_df$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, decreasing = TRUE), ]
  Paises_atributos_Segmentada_x_años[[as.character(year)]] <- sub_df
}

# =================================================================================================
# 3. OPTIMAL k-NN IMPUTATION & NA FILLING (Example: Year 2022)
# =================================================================================================
df_target <- Paises_atributos_Segmentada_x_años[["2022"]]
df_target <- df_target[!is.na(df_target$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`), ]

set.seed(123)
folds <- createFolds(df_target$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, k = 10, list = TRUE, returnTrain = TRUE)

k_values <- 1:10
errors   <- numeric(length(k_values))
imputation_error <- function(original, imputed) sum((original - imputed)^2, na.rm = TRUE)

for (k in k_values) {
  fold_errors <- numeric(length(folds))
  for (i in 1:length(folds)) {
    train_idx <- folds[[i]]
    test_idx  <- setdiff(1:nrow(df_target), train_idx)
    
    train_data <- df_target[train_idx, ]
    test_data  <- df_target[test_idx, ]
    
    imverted_data <- kNN(train_data, k = k)
    imverted_test <- imverted_data[test_idx, 1:ncol(df_target)]
    
    fold_errors[i] <- imputation_error(test_data$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, 
                                       imverted_test$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`)
  }
  errors[k] <- mean(fold_errors)
}

optimal_k <- k_values[which.min(errors)]
print(paste("Optimal K found:", optimal_k))

# Complete Imputation for year 2022 using optimized parameters
Paises_atributos_2022 <- Paises_atributos_Segmentada_x_años[["2022"]]
Paises_atributos_imputed <- kNN(Paises_atributos_2022, k = optimal_k)
Paises_atributos_imputed <- Paises_atributos_imputed[, 1:ncol(Paises_atributos_2022)]
write.xlsx(Paises_atributos_imputed, "Paises_atributos_imputed_2022.xlsx")

# =================================================================================================
# 4. GRAPH OBJECT GENERATION & TOPOLOGY METRICS
# =================================================================================================
g <- list()
for (year in names(years)) {
  year_data <- data_net %>% select(ReporterISO3, PartnerISO3, !!year) %>% filter(!is.na(!!sym(year)))
  g[[year]] <- graph_from_data_frame(
    d = year_data,
    vertices = unique(c(year_data$ReporterISO3, year_data$PartnerISO3)),
    directed = TRUE
  )
  E(g[[year]])$weight <- year_data[[year]]
}

medidas <- data.frame()
for (year in names(g)) {
  graph <- g[[year]]
  medidas <- rbind(medidas, data.frame(
    Año                       = year,
    Cantidad_nodos            = vcount(graph),
    Cantidad_aristas           = ecount(graph),
    Grafo_dirigido            = is_directed(graph),
    Grafo_ponderado           = is_weighted(graph),
    Grado_salida_promedio     = mean(degree(graph, mode = "out")),
    Grado_entrada_promedio    = mean(degree(graph, mode = "in")),
    Mediana_grado             = median(degree(graph, mode = "in")),
    sd_salida                 = sd(degree(graph, mode = "out")),
    sd_entrada                = sd(degree(graph, mode = "in")),
    coef_variacion_in         = (sd(degree(graph, mode = "out")) / mean(degree(graph, mode = "in"))),
    Densidad                  = edge_density(graph),
    Correlacion               = cor(degree(graph, mode = "out"), degree(graph, mode = "in"), method = "pearson"),
    Peso_promedio             = mean(E(graph)$weight, na.rm = TRUE),
    Transitividad_global      = transitivity(graph, type = "global"),
    Reciprocidad_aristas      = reciprocity(graph, mode = "default"),
    Reciprocidad_diadas       = reciprocity(graph, mode = "ratio"),
    K_conectividad            = vertex_connectivity(graph),
    Componentes               = length(decompose(graph)),
    Tamaño_componente_gigante = max(sapply(decompose(graph), vcount)),
    Asortatividad             = assortativity_degree(graph),
    Numero_clanes             = clique.number(graph)
  ))
}
medidas_transpuesta <- as.data.frame(t(medidas))
colnames(medidas_transpuesta) <- medidas$Año
medidas_transpuesta <- medidas_transpuesta[-1, ]

# =================================================================================================
# 5. FILTERING TOP RELATIONSHIPS FOR PRESTIGE VISUALIZATION
# =================================================================================================
# Select the year to plot
target_year <- "2022" 

g_filtrado_x_peso <- data_net %>% 
  select(ReporterISO3, PartnerISO3, !!sym(target_year)) %>% 
  arrange(desc(!!sym(target_year))) %>% 
  distinct(ReporterISO3, .keep_all = TRUE) %>%
  head(80) %>%
  graph_from_data_frame(directed = TRUE)

# Calculate weights and communities
E(g_filtrado_x_peso)$weight <- as.data.frame(as_data_frame(g_filtrado_x_peso, what="edges"))[[target_year]]
c_im <- cluster_infomap(g_filtrado_x_peso)

# =================================================================================================
# 6. EPIC VISUALIZATION 1: INTERACTIVE WEB ENGINE (visNetwork)
# =================================================================================================
net_nodes <- data.frame(
  id    = V(g_filtrado_x_peso)$name,
  label = V(g_filtrado_x_peso)$name,
  title = paste0("<strong>Country:</strong> ", V(g_filtrado_x_peso)$name, 
                 "<br><strong>Total Out-Strength:</strong> ", round(strength(g_filtrado_x_peso, mode = "out"), 2)),
  value = strength(g_filtrado_x_peso, mode = "out"), 
  group = as.character(membership(c_im))             
)

net_edges <- data.frame(
  from  = as_data_frame(g_filtrado_x_peso, what = "edges")$from,
  to    = as_data_frame(g_filtrado_x_peso, what = "edges")$to,
  value = E(g_filtrado_x_peso)$weight,               
  title = paste0("Trade Volume: ", round(E(g_filtrado_x_peso)$weight, 2))
)

# Launch dynamic HTML interface
vis_interactive <- visNetwork(net_nodes, net_edges, 
           main = list(text = paste("Global Carbon Trade Network Map (", target_year, ")"), 
                       style = "font-family:Helvetica,Arial;color:#2C3E50;font-size:26px;text-align:center;font-weight:bold;"),
           submain = list(text = "Dynamic Physics Engine Topology partitioned by Infomap Clustering", 
                          style = "font-family:Helvetica,Arial;color:#7F8C8D;font-size:14px;text-align:center;margin-bottom:15px;")) %>%
  visEdges(
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.35)),
    smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.15),
    color  = list(color = "rgba(149, 165, 166, 0.4)", highlight = "#E74C3C")
  ) %>%
  visNodes(
    shape  = "dot",
    scaling = list(min = 18, max = 50),
    shadow = list(enabled = TRUE, size = 8)
  ) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = list(enabled = TRUE, style = "width: 180px; height: 30px; font-size: 13px; border-radius:5px;")
  ) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(gravitationalConstant = -120, centralGravity = 0.015, springLength = 120),
    stabilization = list(iterations = 200)
  ) %>%
  visInteraction(navigationButtons = TRUE)

# View Interactive Plot
print(vis_interactive)

# =================================================================================================
# 7. EPIC VISUALIZATION 2: CINEMA-GRADE STATIC PLOT (ggraph + tidygraph)
# =================================================================================================
tidy_graph <- as_tbl_graph(g_filtrado_x_peso) %>%
  mutate(
    Community    = as.factor(membership(c_im)),
    Export_Power = centrality_degree(mode = "out", weights = weight),
    Label        = name
  )

epic_static_plot <- ggraph(tidy_graph, layout = 'fr', start.temp = 100) + 
  # Curved paths to show incoming and outgoing paths separately without overlapping
  geom_edge_arc(
    aes(edge_width = weight, edge_alpha = weight),
    arrow    = arrow(length = unit(2.5, 'mm'), type = "closed"),
    color    = "#2C3E50",
    strength = 0.25, 
    show.legend = FALSE
  ) +
  # Beautiful plasma neon nodes matching communities
  geom_node_point(
    aes(size = Export_Power, color = Community),
    alpha = 0.90,
    show.legend = TRUE
  ) +
  # Repel labels safely away from center node points to avoid colliding text
  geom_node_text(
    aes(label = Label),
    repel    = TRUE,                 
    size     = 3.8,
    fontface = "bold",
    color    = "#1A252F"
  ) +
  # Custom design parameters
  scale_edge_width(range = c(0.3, 3.0)) +
  scale_edge_alpha(range = c(0.15, 0.75)) +
  scale_size_continuous(range = c(5, 18), name = "Global Export Volume") +
  scale_color_viridis_d(option = "plasma", name = "Economic Blocs (Infomap)") +
  labs(
    title    = "International Carbon Trade Flow Architecture",
    subtitle = paste("Top Global Trade Relationships Analytical Topology Matrix — Year:", target_year),
    caption  = "Data Source: TradeCarbonWorld Modeling via igraph & ggraph Engine"
  ) +
  theme_void() + 
  theme(
    plot.title    = element_text(size = 22, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, face = "italic", color = "#7F8C8D", hjust = 0.5, margin = margin(b = 25)),
    plot.caption  = element_text(size = 9, color = "#95A5A6", hjust = 0.95),
    legend.position = "right",
    legend.title  = element_text(size = 11, face = "bold"),
    legend.text   = element_text(size = 10),
    plot.margin   = margin(20, 20, 20, 20)
  )

# Render Cinema Static Masterpiece
x11(width = 14, height = 10)
print(epic_static_plot)