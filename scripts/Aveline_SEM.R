#https://docs.google.com/spreadsheets/d/e/2PACX-1vQHXBBah_VLNcQIUaouCXzWnb2G47yWduBZfnXOx4S61ySn-rGlQqY0TTjGZSB_5AUDTFD2yttfOD5t/pub?gid=99131913&single=true&output=csv

#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Anderson 2007 dataset
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
install.packages("lavaan")
library(lavaan)
library(dplyr)

# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/1YPMq10k8PUEdWJN9VbP04FEBwD1infb6DjOHxsTNZ7A/edit?gid=99131913#gid=99131913")

# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQHXBBah_VLNcQIUaouCXzWnb2G47yWduBZfnXOx4S61ySn-rGlQqY0TTjGZSB_5AUDTFD2yttfOD5t/pub?gid=99131913&single=true&output=csv") 

names(SEMdata)

 # standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
names(SEMdatastd)
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% dplyr::select(dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% dplyr::select(dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
mulreg_std <- lm(woody ~ dist2river+elevation+CorProtAr+rainfall+cec+burnfreq+hills, data = SEMdatastd)

summary(mulreg_std)

#make a model
woody_model <- 'woody~dist2river+cec+rainfall+burnfreq+elevation
                rainfall~hills+elevation
                burnfreq~elevation+CorProtAr+rainfall
                dist2river~hills+rainfall
                cec~dist2river+rainfall+burnfreq
                elevation~hills'
woody_model

woody_fit <- lavaan::sem(woody_model, data = SEMdatastd)
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)

#another try
woody_model1 <- 'woody~dist2river+burnfreq+cec+elevation
                burnfreq~CorProtAr+rainfall
                cec~CorProtAr+rainfall
                CorProtAr~elevation
                rainfall~elevation
                dist2river~elevation'
woody_model1

woody_fit1 <- lavaan::sem(woody_model1, data = SEMdatastd)
summary(woody_fit1, standardized = T, fit.measures = T, rsquare = T)
# CFI: 0.852, TLI: 0.690, RMSEA: 0.211, SRMR: 0.086



#another try
woody_model2 <- 'woody~dist2river+burnfreq+cec
                burnfreq~CorProtAr+rainfall
                cec~CorProtAr+rainfall
                CorProtAr~elevation
                rainfall~elevation
                dist2river~elevation'
woody_model2

woody_fit2 <- lavaan::sem(woody_model2, data = SEMdatastd)
summary(woody_fit2, standardized = T, fit.measures = T, rsquare = T)



#CorProtAr could have an impact on cec, because those areas area are fertile
#low rain means higher nutrients, more rain means it washes away nutrients

#burn --> poisson
#CorProtAr --> binomial

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR


#######################################
#tryout causal map
install.packages("qgraph")
library(qgraph)

# Define nodes
nodes <- c("elevation", "dist2river", "rainfall", "CorProtAr", "cec", "burnfreq", "woody")

# Define edges (from, to, weight) based on SEM output
edges <- data.frame(
  from = c("elevation", "elevation", "elevation", "rainfall", "CorProtAr", "burnfreq", "cec", "cec", "dist2river"),
  to = c("rainfall", "dist2river", "CorProtAr", "cec", "burnfreq", "woody", "woody", "burnfreq", "woody"),
  weight = c(0.563, 0.263, -0.564, -0.244, 0.713, 0.149, -0.368, -0.275, -0.196),
  stringsAsFactors = FALSE
)

# Highlight significant relationships
edges$color <- ifelse(edges$weight > 0, "blue", "red")  # Positive: blue, Negative: red
edges$width <- abs(edges$weight) * 20  # Scale edge width
# Add edge labels (coefficients)
edges$label <- round(edges$weight, 3)  # Round to 3 decimal places


# Create the graph with edge labels
qgraph(
  edges[, c("from", "to")], 
  edge.color = edges$color,          # Edge colors for positive/negative relationships
  edge.width = edges$width,          # Edge widths scaled by magnitude
  labels = nodes,                    # Node labels
  edge.labels = edges$label,         # Add labels (coefficients) to the edges
  label.scale = 1.5,
  layout = "spring",                 # Spring layout for better visualization
  vsize = 15,                        # Node size
  asize = 6,                         # Arrow size
  directed = TRUE,                   # Arrows to indicate directionality
  title = "Causal Map from SEM with Coefficients"
)

##
num_nodes <- length(nodes)

layout <- qgraph::qgraph(
  edges[, c("from", "to")], 
  edge.color = edges$color,
  edge.width = edges$width,
  labels = nodes,
  edge.labels = edges$label,
  layout = "spring",
  vsize = 15,
  asize = 6,
  directed = TRUE
)$layout

# Adjust "woody" position manually in the layout matrix
layout[num_nodes, ] <- layout[num_nodes, ] + c(1, 0)  # Push it to the right

# Plot with adjusted layout
qgraph(
  edges[, c("from", "to")], 
  edge.color = edges$color,
  edge.width = edges$width,
  labels = nodes,
  edge.labels = edges$label,
  layout = layout,
  vsize = 15,
  asize = 6,
  directed = TRUE
)


#####

install.packages("ggraph")  # Install the package if you haven't already
library(ggraph)             # Load the package


# Prepare the R-squared values
r_squared <- data.frame(
  node = c("woody", "burnfreq", "cec", "CorProtAr", "rainfall", "dist2river"),
  R2 = c(0.145, 0.712, 0.248, 0.318, 0.317, 0.069)
)

# Create the plot with R^2 annotations
causal_plot <- ggplot() +
  geom_edge_link(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, 
                     label = round(label, 2)), 
                 data = edges, 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "black", 
                 angle_calc = "along", 
                 label_dodge = unit(2, "mm")) +
  geom_node_point(aes(x = x, y = y), 
                  data = nodes, 
                  size = 10, 
                  color = "skyblue") +
  geom_node_text(aes(x = x, y = y, label = label), 
                 data = nodes, 
                 size = 4, 
                 fontface = "bold") +
  geom_text(aes(x = x, y = y - 0.3, label = paste0("R² = ", round(R2, 2))), 
            data = merge(nodes, r_squared, by.x = "label", by.y = "node"), 
            size = 4, 
            color = "darkblue") +
  theme_void()

# Display the plot
print(causal_plot)




######################################

# Install and load required package
if (!require(DiagrammeR)) install.packages("DiagrammeR")
library(DiagrammeR)

# Define the nodes and edges
graph <- create_graph() %>%
  add_node(label = "Distance to River", id = "dist2river") %>%
  add_node(label = "Elevation", id = "elevation") %>%
  add_node(label = "Protected Area", id = "CorProtAr") %>%
  add_node(label = "Rainfall", id = "rainfall") %>%
  add_node(label = "Soil CEC", id = "cec") %>%
  add_node(label = "Burn Frequency", id = "burnfreq") %>%
  add_node(label = "Hills", id = "hills") %>%
  add_node(label = "Woody Cover", id = "woody") %>%
  
  # Add edges based on relationships
  add_edge(from = "dist2river", to = "elevation") %>%
  add_edge(from = "elevation", to = "rainfall") %>%
  add_edge(from = "rainfall", to = "cec") %>%
  add_edge(from = "rainfall", to = "burnfreq") %>%
  add_edge(from = "burnfreq", to = "woody") %>%
  add_edge(from = "cec", to = "woody") %>%
  add_edge(from = "hills", to = "woody") %>%
  add_edge(from = "CorProtAr", to = "woody") %>%
  add_edge(from = "elevation", to = "woody")

# Visualize the causal web
render_graph(graph)


### another new map tryout
# Install igraph if not already installed
if (!require(igraph)) install.packages("igraph", dependencies=TRUE)
library(igraph)

# Define the variables (nodes)
variables <- c("elevation", "rainfall", "CorProtAr", "dist2river", "burnfreq", "cec", "woody")

# Define the relationships (edges) with coefficients
edges <- data.frame(
  from = c("elevation", "elevation", "elevation", "elevation", "rainfall", "rainfall", 
           "CorProtAr", "CorProtAr", "burnfreq", "burnfreq", "dist2river"),
  to = c("rainfall", "CorProtAr", "dist2river", "woody", "burnfreq", "cec", 
         "burnfreq", "cec", "woody", "woody", "woody"),
  weight = c(0.563, -0.564, 0.263, -0.111, -0.275, -0.244, 0.713, 0.363, -0.090, -0.354, -0.111)
)

# Create the graph
g <- graph_from_data_frame(edges, vertices = variables, directed = TRUE)

# Define a custom layout for the nodes
layout_coords <- data.frame(
  node = variables,
  x = c(0, 1, 0.5, 0.5, 1, 1, 2),  # Position of nodes on x-axis (elevation on left, woody on right)
  y = c(3, 2, 2, 2.5, 1, 1, 0)    # Position of nodes on y-axis (arranged vertically)
)

# Convert to a matrix that igraph understands (coordinates for each node)
layout_matrix <- as.matrix(layout_coords[, c("x", "y")])

# Plot the graph
plot(g, 
     vertex.size = 30, 
     vertex.label.cex = 1.2, 
     vertex.label.dist = 1, 
     edge.arrow.size = 0.5, 
     vertex.color = "skyblue", 
     layout = layout_matrix, 
     edge.width = E(g)$weight * 2 + 1,  # Adjust edge width based on strength of the relationship
     main = "Causal Map"
)
