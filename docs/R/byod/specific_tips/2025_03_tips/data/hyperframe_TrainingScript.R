#!/usr/bin/env Rscript 

library(spatstat)
library(here)
source(here('code', 'function_hyperframe.R'))

# Set variables for creating the hyperframe in a two-dimensional plane
xrange <- c(0,124.94)
yrange <- c(0,100.24)
unit <- "micron"
W <- owin(xrange, yrange, unitname=unit)
formula_syncom <- "~syncom + dpi + exp + img"
formula_strain <- "~syncom + dpi + exp + img + strain"
 
# Read coordinates file
coordinates <- readRDS(here('results', 'coordinates.rds'))
coordinates$channel <- factor(coordinates$channel) # channel must be converted to factors to be assigned as marks

# Create hyperframe with marked points for channels
H_syncom <- hyperframe_function(coordinates, xrange, yrange, unit, formula_syncom)
H_syncom_summary <- coordinates %>% 
    group_by(syncom, dpi, exp, img, channel) %>% 
    tally() %>% 
    pivot_wider(id_cols=c(syncom, exp, dpi, img), names_from = channel, values_from = n, values_fill = 0) %>% 
    mutate(rep = paste(syncom,dpi,exp,img, sep = "_"))
reord_index <- match(row.names(H_syncom), H_syncom_summary$rep)
H_syncom_summary <- H_syncom_summary[reord_index,]

# Add attributes to the hyperframe
H_syncom$C0 = H_syncom_summary$C0
H_syncom$C1 = H_syncom_summary$C1
H_syncom$C2 = H_syncom_summary$C2
H_syncom$sum = H_syncom$C0 + H_syncom$C1 + H_syncom$C2
H_syncom$syncom = H_syncom_summary$syncom
H_syncom$dpi = H_syncom_summary$dpi

# Export data
saveRDS(H_syncom, here('results', 'hyperframe_syncom.rds'))
saveRDS(H_syncom_summary, here('results', 'hyperframe_syncom_summary.rds'))


# Create hyperframe for each strain
H_strain <- hyperframe_function(coordinates, xrange, yrange, unit, formula_strain)
H_strain_summary <- coordinates %>% 
    group_by(syncom, dpi, exp, img, strain) %>% 
    tally() %>% 
    mutate(rep = paste(syncom,dpi,exp,img,strain, sep = "_"))
reord_index <- match(H_strain$rep, H_strain_summary$rep)
H_strain_summary <- H_strain_summary[reord_index,]

# Add attributes to the hyperframe
H_strain$strain = H_strain_summary$strain
H_strain$n = H_strain_summary$n
H_strain$syncom = H_strain_summary$syncom
H_strain$dpi = H_strain_summary$dpi

# Export data
saveRDS(H_strain, here('results', 'hyperframe_strain.rds'))
saveRDS(H_strain_summary, here('results', 'hyperframe_strain_summary.rds'))

