_your zenodo badge here_

# Di Vittorio et al 2024 Nature Sustainability

##Future land use under spatially explicit land protection and terrestrial carbon scenarios

Alan V. Di Vittorio<sup>1\*</sup>, Kanishka B. Narayan<sup>2</sup>,  and Michael I. Westphal<sup>2</sup>

<sup>1 </sup> Lawrence Berkeley National Laboratory, Berekeley, CA, USA

<sup>2 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA

\* corresponding author:  avdivittorio@lbl.gov

## Abstract
Nearly 200 countries have pledged to conserve 30% of terrestrial ecosystems. Achieving this goal requires scientific input that considers both ecological benefits and impacts to human systems. We use the Global Change Analysis Model (GCAM) to evaluate human system impacts of four land protection cases intersected with two terrestrial carbon cases. Protected land conversion pressure and food prices for spatially heterogeneous cases with 31% and 39% protected land, respectively, are lower than for uniformly distributed 30% protected land. Spatially distributed protection allows more suitable land available for conversion, compared to uniform protection, thus reducing land use conflicts. Valuing terrestrial carbon reduces land conversion pressure; in some places sufficiently to preserve the prescribed protected land amount. Global impacts of land additional protection are small, but regional impacts vary and may be considerable. The results indicate that spatially heterogeneous land protection could have less impact on human systems than uniform protection.

## Journal reference
Di Vittorio, A.V., K.B. Narayan, & M.I. Westphal (submitted). Future land use under spatially explicit land protection and terrestrial carbon scenarios. Nature Sustainability. DOI: TBD

## Supplemental information
All supplemental materials are included in this metarepo. Paper figures and data are in `paper_figures_data`. Supplemental figures and data are in `supplemental_figures_data`. Supplemental tables are in `supplemental_tables`.

## Model code reference
This is the code used for the simulations in this study.

| Model | Version | Repository Link |
|-------|---------|-----------------|
| GCAM	| 7.0	| <url here> |

## Data reference

### Output data
Model output data are in `outputs/project_files`. These files contain a subset of the simulation otuputs in a format readily accessible to the rgcam R package. These are the source files for the processing scripts in this metarepo.

## Reproduce the experiment

Maybe just describe this?

1. Download and install GCAM7 from [Model code reference](#Model-code-reference)
2. Use the protected area input data files in `pre-processing` to run GCAMDATA four times to get GCAM input files. The respective files are: `Allan_sce1_plus_default_separated_GCAM.csv`, `Allan_sce2_plus_default_seoarated_GCAM.csv`, `GCAM_Unavailable_separated_GCAM.csv`, `thirty_percent_plus_default_separated_GCAM.csv`. `pre-processing` also contains an R script that generates the four protected area input data files from source data in `other_data`.
3. Run GCAM 16 times with the different configuration files; 4 reference, 4 net-zero; then duplicated with the protected area not protected


## Reproduce the figures
There are four diagnostic R scripts at the top level of this metarepo that generate many figures and associated data files. The publication figures were selected from these diagnostics.

The R scripts have been developed using R 4.3.2. Required libraries are loaded at the beginning of each script using the `library()` function and must be installed as needed.

| Script Name | Description | How to Run |
| --- | --- | --- |
| `Biodiv_paper.Rmd` | An RStudio script with several chunks for generating diagnostics on: commodity prices, land values, land conversion pressure, land allocation, CO2 prices, and correlations between agricultural production, and price | Open with RStudio or R and execute the code. Outputs are written to `outputs/images` and `outputs/csv` |
| `land_alloc_agg.r` | An R script to plot global and regional time series and box plots of GCAM aggregated land allocation | Open with R and execute the code. Outputs are written by default to `outputs/land_allocation`, and this can be changed in the script |
| `proc_gcam_land_distribution_biodiv_temporal.r` | An R script to calculate detailed land allocation statistics, make maps, and perform regressions on land value and area | Open with R and execute the code. Outputs are written by default to `outputs/gcam_land_distribution`, and this can be changed in the script |
| `GCAM_Biodiv_R_scripts.R` | An R script to plot electricity generation/prices, energy consumption, biomass production, commodity prices, co2 prices, and land allocation | Open with R and execute the code. Outputs are written by default to `outputs/figures_westphal`, and this can be changed in the script |
