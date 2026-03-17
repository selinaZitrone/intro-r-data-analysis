## Scripts to hand out per student

### Mariona - proteomics analysis
- xlsx
- Give:
	- `general_tips/volcano_heatmap_rnaseq.R`: Heatmap, Volcano plot, PCA with RNA-seq data (same workflow applies to proteomics)
	- `general_tips/penguins_pca.R`: Details on PCA and plots
- Microscopy intensity plots with significance: covered today with `geom_signif()`
- Q: What is the difference between proteomics etc?

### Kirsten - Volcano plot
- xlsx
- Give:
	- `general_tips/volcano_heatmap_rnaseq.R`: Volcano plot with ggrepel labels + select columns
- Straightforward, fully covered

### Laura - Excretion profile
- xlsx
- Reproduce figure 5: https://www.sciencedirect.com/science/article/pii/S0960076021001710?via%3Dihub
 ![[Workshop March 2026.png]]
- Standard ggplot geom_line() + geom_point() with color by participant
- Give:
	- `general_tips/broken_axis_plot.R`: broken x-axis with ggbreak + patchwork alternative

### Kudor - UMAPS, heatmaps, boxplots (no data?)
- Give:
	- `general_tips/volcano_heatmap_rnaseq.R`: heatmaps
	- `2025_10_tips/anna_seurat.R`: Seurat object exploration, DimPlot (tSNE/UMAP)
	- Seurat tutorial for full scRNA-seq pipeline: https://satijalab.org/seurat/articles/pbmc3k_tutorial
- If no own data: point to RNA-seq example dataset or Olympics
- Q: Still unclear what data he has - ask on the day

### Amelia - Bacterial growth curves
- Multiple excel sheets, each sheet = one condition, columns = OD triplicates per strain
- Give:
	- `general_tips/read_and_combine_tables.R` (Example 2: multi-sheet Excel reading with readxl)
- Dummy data: run `2026_03_tips/create_dummy_growth_data.R` to create `data/dummy/multi_sheet_data.xlsx`
- Calculations (mean triplicates, subtract baseline): standard dplyr group_by/summarize/mutate
- Growth curve plot: ggplot geom_line() + facet_wrap()
- Max OD bar plot: ggplot geom_col()

### Jannik - Pharmacokinetics / PBPK modeling
- No own data, exploratory ("try building simple models")
- Data cleaning + concentration-time plots: standard workshop content
- Give:
	- `2025_10_tips/katharina_models.R`: full mrgsolve workflow (import estimates, simulate, VPC, scenarios)
	- Link to mrgsolve docs: https://mrgsolve.org/
- Tip: tell him to focus on Steps 3-4 (simulation + plots) if NONMEM import isn't relevant yet

### Natalia - Cell cytotoxicity bar graphs + stats
- No data yet, may bring similar xlsx
- Bar graphs: standard ggplot
- Significance annotations: covered today with geom_signif()
- If she has dose-response data:
	- Give: `general_tips/drc_data.R` (dose-response fitting + EC50 extraction + ggplot)
	- Tutorial: https://www.w3tutorials.net/blog/plotting-dose-response-curves-with-ggplot2-and-drc/

### Niall - Omics data (no own data)
- Give:
	- `general_tips/volcano_heatmap_rnaseq.R`: heatmaps + volcano plots
	- `2025_10_tips/anna_seurat.R`: Seurat DimPlot for tSNE/UMAP
	- Seurat tutorial for full scRNA-seq pipeline: https://satijalab.org/seurat/articles/pbmc3k_tutorial
- Suggest RNA-seq example dataset (byod/dnaseq.qmd) if no own data

### Efrosiniia - scRNA-seq (.mtx sparse matrix)
- NOT ATTENDING

### Mohammed - General data viz (no own data)
- Point to Olympics example dataset (most beginner-friendly)
- No special scripts needed

### Khashayar, Manuel, Christina - No response
- Assign example datasets on the day

---

## Webex message

- **Volcano plot/heatmaps/PCA**: `volcano_heatmap_rnaseq.R` (download)
- **PCA** examples + Plots using the penguins data: `penguins_pca.R`
- **Single-cell pipeline** (UMAP, tSNE, etc.) with the **Seurat** R pkg:
  - Follow-along tutorial for standard workflow: https://satijalab.org/seurat/articles/pbmc3k_tutorial
  - Overview of different workflows and tutorials possible with the package: https://satijalab.org/seurat/articles/get_started_v5_new

- **Laura**: 
  - `broken_axis_plot.R` for the cut-off x-axis
- **Amelia**: 
  - `read_and_combine_tables.R`, Example 2 (reading multiple Excel sheets and combining into one table)
- **Jannik**: 
  - `katharina_models.R` shows a PK modeling example from a previous workshop using `mrgsolve` 
  - `mrgsolve` R package docs with easier examples: https://mrgsolve.org/
- **Natália**: 
  - `drc_data.R` in case you need dose-response models and plotting
