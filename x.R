# Modern Network & Spatial Visualizations for Trade Networks
# Optimized for High-Quality, Publication-Ready Output

# 1. Install & Load Modern Visualization Packages
install.packages(c("ggraph", "tidygraph", "viridis", "sf", "rnaturalearth", "tidyverse"))
library(igraph)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(viridis)
library(sf)
library(rnaturalearth)

# [Assuming your 'clustering', 'trade_graph', 'subgraph', 'top_clustering', and 'world_merged' 
#  objects are already loaded/calculated from your SBM model code]

# Custom High-End Color Palette (Sleek, vibrant, colorblind-friendly)
modern_colors <- c("1" = "#FF2E63", "2" = "#00ADB5", "3" = "#FFD369", "4" = "#252A34")

# ==============================================================================
# VISUALIZATION 1: Modern Network Representation (ggraph)
# ==============================================================================

# Convert igraph to a tidygraph object for advanced modern styling
tidy_subgraph <- as_tbl_graph(subgraph) %>%
  activate(nodes) %>%
  mutate(
    Class = as.factor(top_clustering$labs[match(name, top_clustering$`Country code`)]),
    Exports = top_clustering$total_exports[match(name, top_clustering$`Country code`)]
  )

modern_network_plot <- ggraph(tidy_subgraph, layout = 'graphopt') +
  # Draw clean, elegant curved trade flows with arrows
  geom_edge_arc(aes(alpha = ..index..), 
                color = "grey70", 
                arrow = arrow(length = unit(2.5, 'mm'), type = "closed"), 
                strength = 0.2, 
                show.legend = FALSE) +
  # Stylized country nodes sized by export strength and colored by SBM class
  geom_node_point(aes(color = Class, size = Exports), alpha = 0.9) +
  # Sleek, overlapping-protected text labels using a minimalist font
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 size = 3.5, 
                 fontface = "bold", 
                 color = "#222831") +
  # Modern scale modifications
  scale_color_manual(values = modern_colors) +
  scale_size_continuous(range = c(4, 12), labels = scales::comma) +
  labs(
    title = "International Trade Network Core Structure",
    subtitle = paste("Key class representatives sized by export volume • Year", year),
    color = "SBM Latent Class",
    size = "Total Exports (USD)"
  ) +
  theme_graph(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222831"),
    plot.subtitle = element_text(size = 11, color = "#393E46", margin = margin(b = 15)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10)
  )

# Output plot to screen or file
x11(width = 11, height = 8.5)
print(modern_network_plot)


# ==============================================================================
# VISUALIZATION 2: Modern Adjacency Matrix Heatmap
# ==============================================================================

# Convert adjacency matrix into long-format tibble for ggplot
matrix_df <- as.data.frame(A) %>%
  rownames_to_column(var = "From") %>%
  pivot_longer(-From, names_to = "To", values_to = "Connection")

# Match classes to order matrix elegantly
node_order <- data.frame(Country = V(trade_graph)$name, Class = labs) %>%
  arrange(Class)

matrix_df <- matrix_df %>%
  filter(From %in% node_order$Country & To %in% node_order$Country) %>%
  mutate(
    From = factor(From, levels = node_order$Country),
    To = factor(To, levels = node_order$Country)
  )

modern_matrix_plot <- ggplot(matrix_df, aes(x = From, y = To, fill = as.factor(Connection))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "#F9F9F9", "1" = "#00ADB5"), labels = c("No Trade", "Active Trade")) +
  labs(
    title = "Adjacency Matrix Organized by Structural Blocks",
    subtitle = "SBM sorting reveals clean block patterns and interaction dense areas",
    fill = "Status"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.text = element_blank(), # Hidden for macro pattern view; can be turned on if tiny
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16, color = "#222831"),
    plot.subtitle = element_text(size = 11, color = "#393E46", margin = margin(b = 15)),
    legend.position = "bottom"
  )

x11(width = 9, height = 8.5)
print(modern_matrix_plot)


# ==============================================================================
# VISUALIZATION 3: High-Fidelity Projection World Map
# ==============================================================================

# Transform to an elegant Robinson global map projection instead of flat coordinates
world_robinson <- st_transform(world_merged, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

modern_map_plot <- ggplot(data = world_robinson) +
  # Graticule layer for global depth aesthetic
  geom_sf(data = st_graticule(world_robinson), color = "#EAEAEA", size = 0.2) +
  # Main country polygon layers
  geom_sf(aes(fill = as.factor(labs)), color = "#FFFFFF", size = 0.15) +
  scale_fill_manual(
    values = modern_colors, 
    na.value = "#EEEEEE", 
    guide = guide_legend(
      title = "SBM Profile Class",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  labs(
    title = paste("Global Topography of Latent Trade Classes (", year, ")", sep = ""),
    subtitle = "Geographic distribution reveals macroeconomic clustering and regional hegemony patterns",
    caption = "Data Source: Project Database | Proj: Robinson"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.title = element_text(face = "bold", size = 18, color = "#222831", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "#393E46", hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_text(size = 8, color = "grey60", margin = margin(t = 15)),
    legend.position = "bottom",
    legend.box.margin = margin(t = 10),
    legend.text = element_text(size = 10, face = "bold")
  )

x11(width = 12, height = 7.5)
print(modern_map_plot)