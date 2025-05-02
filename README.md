# Di Vittorio et al. 2025 Nature Sustainability

## Tradeoffs between land use and biodiversity-specific land protection with <2ºC global warming

Alan V. Di Vittorio<sup>1\*</sup>, Kanishka B. Narayan<sup>2</sup>,  and Michael I. Westphal<sup>2</sup>

<sup>1 </sup> Lawrence Berkeley National Laboratory, Berekeley, CA, USA

<sup>2 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA

\* corresponding author:  avdivittorio@lbl.gov

## Abstract
Nearly 200 countries have pledged to conserve 30% of terrestrial ecosystems to stop the global biodiversity crisis. However, biodiversity is not uniformly distributed across countries. Adequately addressing this crisis requires a scientific basis for selecting protected land that considers both ecological benefits and impacts to humans. We use the Global Change Analysis Model to evaluate land use tradeoffs of four land protection cases under two climate cases. We find that biodiversity-specific land protection up to 39% globally can reduce land use constraints and food prices compared to protecting 30% of land uniformly in each country (“30x30” initiative). Valuing terrestrial carbon for climate change mitigation reduces land conversion pressure and can complement protection strategies. Global impacts to agriculture of additional land protection are small, but regional impacts vary and may be considerable. Overall, biodiversity-specific land protection has greater potential than a uniform target to meet both ecological and human needs.

## Journal article reference
Di Vittorio, A.V., K.B. Narayan, & M.I. Westphal (submitted). Tradeoffs between land use and biodiversity-specific land protection with <2ºC global warming. DOI: TBD

## Data and Supplemental material
All paper materials are included in this metarepo. Paper figures, figure data, and tables are in `paper_figures_tables`. Extended data are in `extended_data` Supplemental figures, data, and tables are in `supplemental_material`.

## Model code reference
This is the code used for the simulations in this study.

| Model | Version | Repository Link |
|-------|---------|-----------------|
| GCAM	| 7.0	| https://github.com/JGCRI/gcam-core/releases/tag/gcam-v7.0 |

## Model experiment

### Protected area data and GCAM inputs

The protected area data are from:

Allan, JR, HP Possingham, SC Atkinson, A Waldron, M Di Marco, SHM Butchart, et al. (2022). The minimum land area requiring conservation attention to safeguard biodiversity. Science, 376:1094-1101. DOI: 10.1126/science.abl9127

These data were pre-processed to generate protection-case-specific protected area files for the gcamdata input processing system. All required source data and the pre-processing script are in the `pre-processing` folder. The gcamdata system was run four times to generate the GCAM input files for the four protection cases (CURRENT, UNIFORM30, BIODIV30, and BIODIV).

### Simulations

GCAM was configured for 16 different simulations

## Experiment Data

### Model output data
Relevant model output data are in `outputs/project_files`. These files contain a subset of the simulation outputs in a format readily accessible to the rgcam R package. Filename bases for each protection case are: `tables_gcam_default` (CURRENT), `tables_30perc` (UNIFORM30), `tables_allansce2` (BIODIV30), and `tables_allansce1` (BIODIV). There are four files for each protection case, corresponding with 16 simulations: two climate mitigation simulations with the the protected area either protected (`nz`) or not protected (`nznoprot`) and two reference simulations with the the protected area either protected (no tag) or not protected (`noprot`). These are the source files for the processing scripts in this metarepo.

## Reproduce the figures
There are five diagnostic R scripts at the top level of this metarepo that generate many figures and associated data files. The publication figures were selected from these diagnostics.

The R scripts have been developed using R 4.3.2. Required libraries are loaded at the beginning of each script using the `library()` function and must be installed as needed.

| Script Name | Description | How to Run |
| --- | --- | --- |
| `bio_paper_regional_protection.r` | An R script to plot spatial maps showing patterns in protection and suitability, including differences between cases | Open with R and execute the code. Outputs are written to `outputs/gcam_land_distribution`, and this can be changed in the script |
| `Biodiv_paper.Rmd` | An RStudio script with several chunks for generating diagnostics on: commodity prices, land values, land conversion pressure, land allocation, CO2 prices, and correlations between agricultural production, and price | Open with RStudio or R and execute the code. Outputs are written to `outputs/images` and `outputs/csv` |
| `GCAM_Biodiv_R_scripts.R` | An R script to plot electricity generation/prices, energy consumption, biomass production, commodity prices, co2 prices, and land allocation | Open with R and execute the code. Outputs are written by default to `outputs/figures_westphal`, and this can be changed in the script |
| `land_alloc_aggr.r` | An R script to plot global and regional time series and box plots of GCAM aggregated land allocation | Open with R and execute the code. Outputs are written by default to `outputs/land_allocation`, and this can be changed in the script |
| `proc_gcam_land_distribution_biodiv_temporal.r` | An R script to calculate detailed land allocation statistics, make maps, and perform regressions on land value and area | Open with R and execute the code. Outputs are written by default to `outputs/gcam_land_distribution`, and this can be changed in the script |

