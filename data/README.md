This folder contains all datasets used in the project, both raw and formatted.

## Raw data

Original datasets as obtained from the source.**These files should never be edited manually.**

| File | Description | Source |



## Formatted data 

Datasets derived from the raw data after cleaning and transformation (e.g., renaming variables, handling missing values, recoding, merging). These are the files loaded by the analysis scripts.

| File | Description |
|------|-------------|
| `df_merged_final.csv` | file used for main_results |

The cleaning steps are performed by `[script_name].R` (located in `[path]`).

## Variables

| Variable | Type | Description, units|
|----------|------|-------------|
| `[organic_matter]` | [numeric] | [Description, units] |
| `[ilr_fines_vs_sand]` | [numeric] | [Description, units] |
| `[ilr_clay_vs_silt]` | [numeric] | [Description, units] |
| `[water_level]` | [numeric] | [Description, cm] |
| `[Surface]` | [numeric] | [Description, cm2] |
| `[distance to coastline]` | [numeric] | [Description, m] |
| `[Salinity]` | [numeric] | [Description, ppt] |
| `[Dry period duration ]` | [numeric] | [Description, days] |
| `[Reflooding date ]` | [numeric] | [Description, day of the year] |
