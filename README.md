# OSS Funding Effects Pipeline (Thesis MDS 2026)

![Research Code](https://img.shields.io/badge/status-research%20code-blue)
![R](https://img.shields.io/badge/language-R-276DC3)
![Python Helpers](https://img.shields.io/badge/python-helper%20scripts-3776AB)
![PostgreSQL](https://img.shields.io/badge/database-PostgreSQL-4169E1)

A reproducible research pipeline to estimate how funding relates to open-source project outcomes using panel data, synthetic control methods, and matched controls.

## What this project does

The project prepares treatment/control OSS repositories, runs multiple outcome experiments, and exports model results and plots for thesis reporting.

## Why this project is useful

- Runs one consistent pipeline across multiple OSS activity outcomes (issues, releases, PRs, commits, contributors).
- Uses two complementary causal inference approaches (`gsynth` and `augsynth`) for robustness checks.
- Produces publication-ready artifacts:
  - Excel outputs in `data/final/<timestamp>/`
  - Coefficient and counterfactual figures in `plots/<timestamp>/`
- Includes optional Python-based propensity score matching helpers to construct control sets.

## Repository layout

- `00_prep_experiment_repos.R`: builds funded vs. non-funded experiment repository table.
- `01_run_experiments.R`: sources and runs all experiment scripts in `experiments/`.
- `02_plotting_outputs.R`: reads exported model files and generates output plots.
- `experiments/`: one script per outcome.
- `src/`: reusable data-building, model, plotting, and setup functions.
- `python_psm_scripts/`: notebooks and Python helpers for matching-related steps.
- `data/proc/`: processed inputs (including matched/control lists).
- `data/final/`: timestamped model outputs.
- `plots/`: timestamped descriptive and counterfactual plots.

## How users can get started

### 1. Prerequisites

- R (recommended: 4.3+)
- PostgreSQL database with the required source tables (e.g., `repo_names`, `repo_groups`, `repo_metadata`, `funding_data`, `issues`, `prs`, `releases`, `commits`)
- (Optional) Python 3.10+ for `python_psm_scripts/`
- Collected data + PostgreSQL setup followed from the [`ecosystems_data_collection`](https://github.com/ldmnch/ecosystems_data_collection) repository. 
### 2. Clone and enter the project

```bash
git clone https://github.com/ldmnch/thesis_mds_2026.git
cd thesis_mds_2026
```

### 3. Configure environment variables

Create a local `.env` file in the repository root:

```dotenv
ECOSYSTEMS_DB_USER=your_postgres_user
POSTGRESQL_ECOSYSTEMS_DB=your_postgres_password
ECOSYSTEMS_DB_NAME=your_database_name
```

Notes:
- Database host is currently set to `localhost` in `src/setup.R`.
- Keep `.env` local only (already ignored by `.gitignore`).

### 4. Install R dependencies

Open R (or RStudio) in the project root and run:

```r
install.packages(c(
  "tidyverse", "dotenv", "readxl", "lubridate", "DBI", "RPostgres",
  "augsynth", "gsynth", "panelView", "janitor", "writexl", "stringr",
  "glue", "httr2", "jsonlite"
))
```

### 5. Run the full analysis pipeline

From the repository root:

```bash
Rscript 00_prep_experiment_repos.R
Rscript 01_run_experiments.R
```

This will:
- Build `data/proc/experiment_repos_with_funding.csv`
- Run all scripts in `experiments/`
- Save experiment outputs into a timestamped folder under `data/final/`
- Save descriptive and counterfactual plots under `plots/`

### 6. Regenerate coefficient plots from a chosen output folder

Edit `specific_folder_path` in `02_plotting_outputs.R`, then run:

```bash
Rscript 02_plotting_outputs.R
```

## Usage examples

### Run a single experiment script interactively

```r
source("src/setup.R")
source("src/build_experiment_data.R")
source("src/model_specifications.R")
source("src/model_plotting_and_formatting.R")
source("experiments/07_commits.R")
```

### Run all experiments in one command

```r
source("01_run_experiments.R")
```

## Python matching helpers (optional)

The `python_psm_scripts/` folder contains notebooks and utilities for propensity score matching and control-list workflows. This code runs connected to the local PostgreSQL database which contains scraped data from [ecosyste.ms](https://ecosyste.ms/), which can be retrieved following [this pipeline](https://github.com/ldmnch/ecosystems_data_collection/blob/main/README.md). 

Install common Python dependencies:

```bash
python -m pip install pandas numpy matplotlib seaborn sqlalchemy
```

## Reproducibility notes

- Results are timestamped by design (`data/final/<YYYY-MM-DD_HH-MM-SS>/`, `plots/<timestamp>/`).
- Several scripts assume local database access and populated source tables.
- If you change model settings in `src/model_specifications.R`, rerun the full pipeline for consistency.
