# Figure plotting code and source data

This repository contains the R plotting code and exported source data for a study of gender differences in doctoral self-hiring, institutional hierarchy, career pathways and scholarly records before and after the first faculty appointment.

The plotting script covers Figures 1–4 and Extended Data Figures 1–5. Each figure section reads its corresponding source data before constructing the panels and assembling the figure.

## Scope

This repository provides the code required to visualize the exported results. Statistical estimates, confidence intervals, density coordinates and medians are read from the source-data files.

The script does not rerun regression models, coarsened exact matching, difference-in-differences analyses or event studies. The underlying individual-level analytical data and fitted model objects are not included.

## Repository structure

```text
.
├── plot_all_figures.R
├── source_data/
│   ├── 1a.xlsx
│   ├── 1b.xlsx
│   ├── ...
│   ├── ED4d.xlsx
│   ├── ED5b&5c.xlsx
│   └── ED5d&5e.xlsx
└── README.md
```

Files beginning with a figure number correspond to main-figure panels. Files beginning with `ED` correspond to Extended Data panels. Some workbooks contain multiple worksheets or results for more than one panel.

Worksheet names and column names should be preserved because the script uses them to identify the plotting inputs.

## Requirements

- R version 4.1.0 or later
- ggplot2 version 3.4.0 or later
- patchwork version 1.2.0 or later
- Cairo support for PDF export

Install the required R packages once:

```r
install.packages(c(
  "readxl",
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "patchwork",
  "scales",
  "gridExtra",
  "cowplot",
  "ggrepel",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata"
))
```

The script does not install packages automatically. The `grid` and `grDevices` packages are included with R.

## Running the code

Keep `plot_all_figures.R` beside the `source_data` directory.

To run all figures from the command line:

```bash
Rscript plot_all_figures.R
```

By default, outputs are written to a new `figures` directory. The script produces PDF and PNG files and a `plotting_validation.csv` report.

To select particular figures:

```bash
Rscript plot_all_figures.R --figures=Fig1,Fig4,EDFig1,EDFig4
```

To specify the input and output directories:

```bash
Rscript plot_all_figures.R --data-dir=source_data --output-dir=figure_outputs
```

To export PDFs only:

```bash
Rscript plot_all_figures.R --formats=pdf
```

Alternatively, run the script from R or RStudio:

```r
source("plot_all_figures.R")
run_figures(root = ".")
```

Use a new output directory for each run. Existing output files and validation reports are not overwritten.

## Outputs and validation

The script checks required inputs and confidence intervals before export. The validation report records whether each figure was exported, blocked by incomplete source data or affected by an execution error.

Incomplete figures are not exported by default. For inspection only, incomplete previews can be requested with:

```bash
Rscript plot_all_figures.R --allow-incomplete
```

These previews have filenames ending in `_INCOMPLETE` and display a warning. They should not be used as complete reproductions of the manuscript figures.

## Source-data items requiring completion

The currently supplied source data require the following additions or corrections for complete reproduction of all original panels and annotations:

| Figure or panel | Required addition or correction |
|---|---|
| Fig. 2b | Add `2b_probability.xlsx` with columns `hierarchy` and `total_pct`. The existing workbook contains pathway composition but not the overall self-hiring probabilities. |
| Fig. 2e | Correct the gender labels in the `Fig2e_left` worksheet of `2e.xlsx`. Both density curves are currently labelled `Men`. |
| Fig. 3d,e | Add `3de_slope_difference.xlsx` with columns `outcome`, `estimate`, `conf_low` and `conf_high`. The outcome labels must be `Publications` and `JIF`. |
| Extended Data Fig. 2c | Add `ED2c_baseline.xlsx` with columns `pathway` and `baseline`, containing the overall adjusted pathway percentages. |
| Extended Data Fig. 3a,b | Add a `display` worksheet to each workbook, containing `display_max`: the original upper display limit on the log-transformed outcome scale. |
| Extended Data Fig. 5a | Add `ED5a.xlsx` with columns `label`, `unmatched` and `matched`, containing absolute standardized mean differences. |

These values should be exported from the original analyses. The plotting script does not infer missing estimates from the published figures or substitute results from other models.

## Plotting conventions

- Women and men are generally shown in red and blue, respectively.
- Positive women-minus-men contrasts indicate higher estimates among women.
- Smaller global institutional rank numbers indicate higher-ranked institutions.
- Larger SpringRank prestige scores indicate greater prestige.
- Confidence intervals and outcome transformations follow the values supplied in the source data.
- Export dimensions are specified separately for each figure in the R script.

Font rendering and minor spacing may vary across operating systems and package versions.
