## To do before Tuesday (2026-10-20)
- [ ] RNA-seq dataset page (`byod/dnaseq.qmd`): better tips with code examples instead of only function names (tried collapsible Hint/Solution callouts, didn't like it)
- [ ] Re-render `byod/dnaseq.qmd` (rendered version still has the callout test)
- [ ] Git: data moved from `specific_tips/2023_02/dnaseq/` to `solutions_for_my_datasets/data/` (shows as deleted/untracked), decide on `.gitignore`
- [x] Felicia: ask what kind of data she has, then write a small normalization example
	- What method/machine does the data come from (e.g. qPCR, ELISA, plate reader)?
	- What is the "reference": a control group, a reference sample or a reference/housekeeping gene?
	- What are the dilutions for: a standard curve or part of the experiment?
	- Can she send one example csv file (or a few rows)?
- [x] Svea: ask about her data, then adapt `svea_metabolites.R` (dummy script ready)
	- What is a row and what is a column in your Excel files?
	- What does each Excel file contain (one time point, one batch, one participant, ...)?
	- How many participants? Groups? Repeated measurements (time points)?
	- What do you want to compare: groups, time points or both?
	- How are values below the detection limit marked (e.g. "<LOQ", "n.d.", empty)?
- [x] Supriya: ask about her data, then adapt `supriya_find_sequences.R` (dummy script ready, covers tables and FASTQ)
	- What do the first lines of a txt file look like (table with columns or FASTQ, i.e. lines starting with @)?
	- Which sequence does she want to find (primer, barcode, adapter, motif)? Exact match or with mismatches?
	- "Filter them out": keep the reads with the sequence or remove them?
	- What is in the Excel file?
- [ ] Fix `specific_tips/2024_03/lme4_mixed_models.R` (uses `mod_light_cover`, which doesn't exist) or stop using it
- [ ] Re-render `sessions/byod.qmd` (added patchwork link)
- [x] Prepare remaining students: Svea, Supriya, Luzmila, Anna
- [ ] Add "Give:" lines for each student + Webex message

## Annelie - Nothing specific
- No own data
- Wants to learn from the other participants' use cases

## ?? - Bacterial growth curves ✔
- Multiple Excel sheets, each sheet = one condition
- Columns = OD measurements per strain (triplicates), first column = time point
- Calculations: mean of triplicates, subtract time point 0, correction factor (knows how to do this)
- Panel A: growth curve per strain across conditions, all time points
- Panel B: max OD per strain per condition

**To send:** growth_curves.R, data/dummy_growth_curves.xlsx

**Mail:**

- look at the xlsx file with dummy data to check how it's structured. I hope it's similar to your data.
- Save dummy_growth_curves.xlsx in the data/ folder of your R project and the R script also in your project
- Go through the R script and try to understand the steps
- In a new R script write the same for you own data
- Ask for questions and problems

Proably this figure:
![[Workshop March 2026-1.png]]

## Abi - Bulk RNA-seq ✔
- Excel file with bulk RNA-seq counts
- Normalize counts, DESeq2 analysis
- Volcano plot, heatmap
- Limited knowledge, needs step-by-step guidance

**To send:** volcano_heatmap_rnaseq.R

**Mail**: Pipeline from a dataset included in an R package. Go through the script from top to bottom. If PCA not interesting in the end, leave it. Once you roughly have an idea how a step works, try to apply to your own data.

## Felicia - Normalization, plots, stats ✔
- csv files, raw data, different groups and dilutions, multiple samples
- Normalization to a reference (log2, log2 fold change)
- Heatmaps, line plots, stacked line plots, bar plots
- Statistical tests between groups or against a reference
- Combine multiple plots into one figure with subplots
- Answer: MFI measured for each glycan-serum pair, each serum in different dilutions. Wants to compare the sera to a reference serum known for glycan binding

**Scripts:** felicia_glycan_mfi.R, general_tips/read_and_combine_tables.R

**Mail:**

- The script creates dummy data (sera incl. a reference serum, glycans, dilutions, triplicates, MFI). Check if this roughly fits your data structure
- To read your own data from multiple csv files into one table, check out read_and_combine_tables.R
- Normalization: log2 MFI and log2 fold change compared to the reference serum (same glycan and dilution)
- Plots: dilution curves (line plot per glycan), heatmap of the log2 fold change, bar plot
- Tests: Welch's t-test of each serum against the reference for each glycan and dilution, p-values adjusted for multiple testing. Only possible if you have replicates
- In the end: combined figure for one glycan with significance brackets (A) and the dilution curve (B)
- Go through the script with the dummy data first, then adapt it to your own data
- Ask for questions and problems

## Svea - Metabolite stats pipeline ✔
- LC-MS data, metabolite concentrations from human participants, collected in multiple Excel files
- Automated inferential tests (choose test based on normality test result for each parameter)
- Linear mixed models
- Add significance brackets from test results to plots
- Answer: single dose, group A had application 1, group B applications 1, 2 and 3. Calculates PK parameters (Cmax, Tmax, ...) from concentration-time curves. Independent comparison of application 1 between groups (automate Shapiro -> Levene -> Student/Welch/Wilcoxon), dependent comparison of the 3 applications in group B with a linear mixed model

**Scripts:** svea_metabolites.R, read_and_combine_tables.R

**Mail:**

- The script creates dummy data that follows your study design (concentration-time curves for group A and B and the 3 applications)
- Then it calculates the PK parameters (Cmax, Tmax and AUC with the PKNCA package) for each curve
- Part 1: automatic choice of the test (Shapiro-Wilk, Levene, then Student's, Welch's t-test or Wilcoxon) for group A vs. B at application 1
- Part 2: linear mixed model for the 3 applications in group B, pairwise comparisons with emmeans and 3 options to plot the results
- To read your own data from multiple Excel files into one table, check out read_and_combine_tables.R
- Go through the script with the dummy data first, then adapt it to your own data

- Note on the automatic test choice: choosing the test based on pre-tests (Shapiro-Wilk, Levene) is very common, but statisticians don't recommend it. Using Welch's t-test by default (on the log scale for Cmax and AUC) is usually the better choice. See e.g.
	- Rasch et al. (2011): https://doi.org/10.1007/s00362-009-0224-x
	- Delacre et al. (2017): https://doi.org/10.5334/irsp.82
- I am not an expert in mixed models, so take the code with a grain of salt. Maybe you can find some example use cases specifically for your type of data
- Resources on mixed models:
	- Winter (2013), beginner tutorial for lme4: https://arxiv.org/abs/1308.5499
	- Meteyard & Davies (2020), best practice and what to report: https://doi.org/10.1016/j.jml.2020.104092
- Resources for PK data:
	- EMA guideline on the investigation of bioequivalence (how PK parameters are usually analysed, e.g. log-transformation of Cmax and AUC, crossover designs): https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-investigation-bioequivalence-rev1_en.pdf
	- Overview of R packages for pharmacokinetics: https://cran.r-project.org/web/views/Pharmacokinetics.html


## Supriya - NGS sequence filtering ✔
- NGS data in txt files (+ Excel)
- Extract and analyze data, identify specific sequences and filter them out
- Automate analysis of multiple txt files in one folder
- Answer: txt files are already processed FASTQ files. Wants to get the %reads of a specific sequence and combine the %reads of all txt files into one output file

**To send:** supriya_find_sequences.R

**Mail:**

- The first part of the script creates dummy txt files in data/ngs_dummy (columns sequence, reads, percent_reads). Your files probably look a bit different, so check the column names
- Then the script reads all txt files from the folder into one table, finds the sequences that contain your sequence and sums up the %reads per file
- The result is saved as csv and Excel file
- Also included: how to look for the reverse complement and (in the end) how to allow mismatches with the Biostrings package
- Go through the script with the dummy data first, then adjust the folder, file pattern and sequence to your own data
- Ask for questions and problems

## Luzmila - Cluster analysis of categorical data ✔
- No own data, example from a paper (Bilalova et al., QCA case survey + expert survey)
- Excel: one table with raw data, one converted to 1/0 (true/false)
- How to organize categorical data for a cluster analysis to find patterns among observations using multiple variables

**Scripts:** luzmila_clustering_qca.R, data/bilalova_calibrated_data.csv
**Email:**
- Excel file is raw data
- Extracted calibrated data from the Appendix (Table A1.4) -> bilalova_calibrated_data.csv. Save it in the data/ folder of your R project
- Script includes example of 
	- Reading in the calibrated data
	- Turn table into  0/1 because this is what you have
	- Clustering example
		- Here I assumed some default methods to show the principle and potential plots. But take this with a grain of salt. It might not be the best clustering method for your research question
		- Optional check of the clustering: compare with another method and use the silhouette to choose the number of clusters
	- QCA example (with fuzzy and with 0/1 data), results are almost the same as in the paper
	- More info: QCA doc

## Anna - Flow cytometry (.fcs files) ✔
- Excel tables + fcs files
- Extract and analyze data from fcs files, automate copying the data into an Excel table
- Bar plots
- Answer: gating in FlowJo, can export the data from FlowJo as csv. Wants to automate the copying into Excel and the bar plots comparing her 2 treatments, because she has to repeat the copy & paste every time she changes a gate

**Scripts:** anna_flowjo_export.R

**Mail:**

- I was not sure what exactly you want to extract from the FlowJo export, so the script is general. It shows how to manipulate the table, and we can then see together how to apply it to your specific use case
- The script creates dummy data that looks roughly like a FlowJo table export (one row per sample, one column per statistic with "gating path | statistic")
- It brings the table into long format, splits the column names into gating path and statistic, extracts the population and the parent population and gets the treatment from the sample name
- Then it saves the table as Excel file. If you change a gate, just export the csv from FlowJo again and re-run the script
- For bar plots and tests, check out the course material from day 2 and 3
- If possible, bring one of your FlowJo csv exports so we can adapt the script together
