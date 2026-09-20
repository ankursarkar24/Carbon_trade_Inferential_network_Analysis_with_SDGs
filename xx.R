# ==============================================================================
# PREMIUM CINEMATIC VISUALIZATION SUITE FOR TRADE NETWORKS
# ==============================================================================
# Packages required: tidyverse, ggraph, tidygraph, sf, rnaturalearth, scico, patchwork

install.packages(c("scico", "patchwork"))
library(tidyverse)
library(ggraph)
library(tidygraph)
library(sf)
library(rnaturalearth)
library(scico)      # Scientific and premium color palettes
library(patchwork)  # For canvas layouts

# Global Cinematic Theme Constants
DARK_BG     <- "#0F172A" # Deep slate/navy dark mode
PANEL_BG    <- "#1E293B" 
TEXT_LIGHT  <- "#F8FAFC"
TEXT_MUTED  <- "#94A3B8"
GLOW_PALETTE <- c("1" = "#FF2E93", "2" = "#00F5FF", "3" = "#FFB800", "4" = "#38EF7D")

# ==============================================================================
# VISUALIZATION 1: THE DARK-MODE KINETIC GLOW NETWORK
# ==============================================================================

tidy_subgraph <- as_tbl_graph(subgraph) %>%
  activate(nodes) %>%
  mutate(
    Class = as.factor(top_clustering$labs[match(name, top_clustering$`Country code`)]),
    Exports = top_clustering$total_exports[match(name, top_clustering$`Country code`)]
  )

kinetic_network <- ggraph(tidy_subgraph, layout = 'stress') +
  # Glowing trade arcs with alpha mapping
  geom_edge_diagonal2(aes(edge_alpha = ..index..),
                      color = "#E2E8F0", 
                      width = 0.4,
                      arrow = arrow(length = unit(3, 'mm'), type = "open"),
                      show.legend = FALSE) +
  # Neon node glow
  geom_node_point(aes(color = Class, size = Exports), alpha = 0.2, show.legend = FALSE) +
  geom_node_point(aes(color = Class, size = Exports * 0.7), alpha = 0.8) +
  # High-contrast sharp typography labels
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 size = 3.8, 
                 fontface = "bold", 
                 color = TEXT_LIGHT,
                 bg.color = DARK_BG,
                 bg.r = 0.1) +
  scale_color_manual(values = GLOW_PALETTE) +
  scale_size_continuous(range = c(3, 16), labels = scales::label_number(suffix = "B", scale = 1e-9)) +
  labs(
    title = "KINETIC CORE OF GLOBAL TRADE",
    subtitle = paste("Macro-economic clusters & structural flow vectors • Year", year),
    color = "Latent SBM Group",
    size = "Export Weight"
  ) +
  theme_graph(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = DARK_BG, color = NA),
    panel.background = element_rect(fill = DARK_BG, color = NA),
    plot.title = element_text(face = "bold", size = 18, color = TEXT_LIGHT, letter_spacing = unit(2, "mm")),
    plot.subtitle = element_text(size = 11, color = TEXT_MUTED, margin = margin(b = 20)),
    legend.background = element_rect(fill = PANEL_BG, color = NA),
    legend.text = element_text(color = TEXT_LIGHT),
    legend.title = element_text(face = "bold", color = TEXT_LIGHT),
    legend.position = "right"
  )

x11(width = 12, height = 8.5)
print(kinetic_network)


# ==============================================================================
# VISUALIZATION 2: THE ABSTRACT MATRIX HEATMAP
# ==============================================================================

node_order <- data.frame(Country = V(trade_graph)$name, Class = labs) %>% arrange(Class)

matrix_df <- as.data.frame(A) %>%
  rownames_to_column(var = "From") %>%
  pivot_longer(-From, names_to = "To", values_to = "Connection") %>%
  filter(From %in% node_order$Country & To %in% node_order$Country) %>%
  mutate(
    From = factor(From, levels = node_order$Country),
    To = factor(To, levels = node_order$Country)
  )

abstract_matrix <- ggplot(matrix_df, aes(x = From, y = To, fill = as.factor(Connection))) +
  geom_tile(color = DARK_BG, size = 0.05) +
  # Vibrant dual tone mapping
  scale_fill_manual(values = c("0" = "#1E293B", "1" = "#00F5FF")) +
  labs(
    title = "STRUCTURAL BLOCK MATRIX INTERACTION",
    subtitle = "Clean mathematical block patterns extracted via Bernoulli SBM partitioning"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = DARK_BG, color = NA),
    panel.background = element_rect(fill = DARK_BG, color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, color = TEXT_LIGHT, letter_spacing = unit(1, "mm")),
    plot.subtitle = element_text(size = 10, color = TEXT_MUTED, margin = margin(b = 15)),
    legend.position = "none"
  )

x11(width = 8, height = 8)
print(abstract_matrix)


# ==============================================================================
# VISUALIZATION 3: THE HIGH-FIDELITY BORDERLESS CHOROPLETH
# ==============================================================================

world_robinson <- st_transform(world_merged, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

luxury_map <- ggplot(data = world_robinson) +
  # Subtle background graticules
  geom_sf(data = st_graticule(world_robinson), color = "#334155", size = 0.15) +
  # High-definition borderless country layers
  geom_sf(aes(fill = as.factor(labs)), color = DARK_BG, size = 0.1) +
  scale_fill_manual(
    values = GLOW_PALETTE, 
    na.value = "#334155", 
    guide = guide_legend(
      title = "Stochastic Block Model Taxonomy Classes",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  labs(
    title = paste("GLOBAL MACROECONOMIC STRATIFICATION (", year, ")", sep=""),
    subtitle = "Latent clustering maps geographic trade hegemony across the international landscape"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = DARK_BG, color = NA),
    plot.title = element_text(face = "bold", size = 18, color = TEXT_LIGHT, hjust = 0.5, letter_spacing = unit(1.5, "mm")),
    plot.subtitle = element_text(size = 11, color = TEXT_MUTED, hjust = 0.5, margin = margin(b = 25)),
    legend.position = "bottom",
    legend.text = element_text(size = 10, face = "bold", color = TEXT_LIGHT),
    legend.title = element_text(color = TEXT_MUTED, size = 9, face = "bold")
  )

x11(width = 13, height = 7.5)
print(luxury_map)