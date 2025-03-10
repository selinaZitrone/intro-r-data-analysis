# A new section --------------------
# https://natverse.org/
# install the neuprintr package
# to install the package, you need to have the devtools package installed
# If you are on Windows: you also need RTools installed. You can download and install RTools
# for Windows from here: https://cran.r-project.org/bin/windows/Rtools/
# Checkout the package documentation here: https://natverse.org/neuprintr/
devtools::install_github("natverse/neuprintr")

# Load necessary libraries
library(tidyverse) # Data manipulation and visualization
library(neuprintr) # Interface for neuPrint
library(broom) # Tidy model outputs

# Login to the neuprint server ------------------------------------------------
neuprint_server <- "https://neuprint.janelia.org"
# get your token: login to neuprint, go to your account, copy paste your Auth Token in to the console
neuprint_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InNlbGluYS5iYWxkYXVmQGZ1LWJlcmxpbi5kZSIsImxldmVsIjoibm9hdXRoIiwiaW1hZ2UtdXJsIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EvQUNnOG9jSjlySUx0MUY4VjVaVTJ3dU5mU2dXU0p5YTZJS29oZ0dRNjJWcE5tN3dmOGttblVBPXM5Ni1jP3N6PTUwP3N6PTUwIiwiZXhwIjoxOTIxNDQ2MDc0fQ.PRHnpfu7bHyoNBs5INxTM2TogXUBMwfuBgSCSw5F1Us"
# set the dataset you want to access
neuprint_dataset <- "hemibrain:v1.2.1"

# login
conn <- neuprint_login(
  server = neuprint_server,
  token = neuprint_token,
  dataset = neuprint_dataset
)
# which datasets are available?
neuprint_datasets()

# A new section --------------------
# Set up connection to a public neuPrint dataset
# (This example uses the 'hemibrain:v1.2' dataset. Change as required.)
set_dataset("hemibrain:v1.2")
# Optionally, set the server URL if different:
# set_server('neuprint.janelia.org')

# A new section --------------------
# Query neuron data from neuPrint
# Here we query for a small set of neurons to illustrate the process.
# For instance, let's retrieve a sample of neurons with basic attributes,
# including their morphology metrics (e.g., total cable length) and cell type.
neuron_query <- '
 MATCH (n:Neuron)
 WHERE n.status = \"Traced\" AND exists(n.type)
 RETURN n.bodyId AS body_id,
        n.type AS neuron_type,
        n.cableLength AS cable_length,
        n.synCount AS synapse_count,
        n.location AS region
 LIMIT 50
 '
# Execute the query
neuron_stats <- neuprint_read_neurons(neuron_query)

# Print the first few rows of the retrieved data
print(head(neuron_stats))
# Interpretation: This table shows a sample of neurons with their cable length, synapse counts, and cell type.

# A new section --------------------
# Basic analysis: Compare morphological features between neuron types
# For this example, we'll analyze if cable length varies across neuron types.
anova_model <- aov(cable_length ~ neuron_type, data = neuron_stats)
anova_summary <- summary(anova_model)
print(anova_summary)
# Interpretation: A significant F value (with a p-value < 0.05) would suggest that cable length significantly differs among neuron types.

# Tidy the ANOVA results
tidy_anova <- tidy(anova_model)
print(tidy_anova)

# A new section --------------------
# Analyze synapse counts by brain region (or location)
# We'll use a boxplot to visualize variation in synapse counts by region.
synapse_plot <- neuron_stats |>
  ggplot(aes(x = region, y = synapse_count, fill = neuron_type)) +
  geom_boxplot() +
  labs(
    title = "Synapse Counts by Brain Region",
    x = "Brain Region",
    y = "Synapse Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(synapse_plot)
# Interpretation: This plot illustrates the distribution of synapse counts in different brain regions, colored by neuron type.

# A new section --------------------
# If you're interested in connectivity patterns,
# you could query connection data with neuprintr (subject to permissions and dataset content).
# Below is a commented example of how you might structure a query for connections.
# \n# connection_query <- 'MATCH (n:Neuron)-[r:ConnectsTo]->(m:Neuron) RETURN n.bodyId AS source, m.bodyId AS target LIMIT 100'
# \n# connections <- neuprint_read_neurons(connection_query)
# \n# Then use packages like igraph to visualize: \n# connection_network <- graph_from_data_frame(connections)
# \n# plot(connection_network, vertex.size = 5, vertex.label = NA, main = 'Neuron Connectivity Network')
#
# This section can be expanded once you have a specific connectivity query or network in mind.
