[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)

# cerebroAppLite

Interactive visualization of single-cell RNA-seq data, built on top of [Shiny](https://shiny.posit.co/).

This is a fork of the excellent [cerebroApp](https://github.com/romanhaa/cerebroApp) by [Roman Hillje](https://github.com/romanhaa).
For general usage, data preparation, and the original feature set, please refer to the official documentation:

> **<https://romanhaa.github.io/cerebroApp/>**

Everything described there (loading data, exploring projections, viewing marker genes, gene expression, etc.) works the same way in cerebroAppLite. The sections below only cover **what this fork adds or changes**.

## Installation

```r
remotes::install_github('duocang/cerebroAppLite')
```

## What's New in This Fork

### 1. `convertSeuratToCerebro()` — one-step data conversion

The original cerebroApp requires you to call `exportFromSeurat()` manually with many parameters. This fork adds a convenience wrapper that handles the entire process in a single call: reading the Seurat object (`.qs` or `.rds`), renaming grouping variables, loading marker gene tables, calculating most-expressed genes, extracting immune repertoire data, and saving a `.crb` file.

```r
library(cerebroAppLite)

convertSeuratToCerebro(
  seurat_file = "my_seurat.qs",        # or .rds
  result_dir  = "output/",
  assay       = "RNA",
  slot        = "data",
  experiment_name = "My Experiment",
  organism    = "Human",
  groups      = c("sample_id", "condition", "cell_type"),
  groups_naming = list(
    "sample_id" = "sample",
    "cell_type" = "cluster"
  ),
  marker_file = "markers.xlsx"         # optional: DE results (.xlsx/.csv/.tsv)
)
# → saves output/cerebro_my_seurat.crb
```

### 2. `createTraditionalShinyApp()` — generate a deployable Shiny app

Instead of running `launchCerebro()` interactively, you can generate a self-contained Shiny app directory with all data and source files bundled. This is useful for deploying to a Shiny server or sharing with collaborators.

```r
createTraditionalShinyApp(
  result_dir   = "my_app/",
  cerebro_data = c(
    `snRNAseq`  = "output/cerebro_snrnaseq.crb",
    `TCR-BCR`   = "output/cerebro_vdj.crb"
  ),
  welcome_message = "<h2>My Single-Cell Atlas</h2>",
  port = 8082
)
# → run with shiny::runApp("my_app/") or deploy to Shiny Server
```

You can pass multiple `.crb` files as a named vector — users will be able to switch between datasets in the app.

### 3. Immune Repertoire Tab (TCR/BCR)

If your Seurat object contains scRepertoire columns (e.g., from `combineExpression()`), or you provide external BCR/TCR files via `bcr_file` / `tcr_file`, an **Immune Repertoire** tab will appear automatically in the Cerebro interface.

This tab provides 19 visualization functions from [scRepertoire](https://github.com/ncborcherding/scRepertoire):

| Category         | Plots                                                             |
| ---------------- | ----------------------------------------------------------------- |
| Clonal structure | Abundance, Diversity, Homeostasis, Proportion, Quant, Rarefaction |
| Clone size       | Compare, Overlap, Scatter, SizeDist                               |
| CDR3 sequence    | Length, AA %, Entropy, Property, K-mer                            |
| Gene usage       | Gene usage, vizGenes, percentGenes, percentVJ                     |

Each plot includes:

- A concise explanation panel describing what the plot shows
- An **Example** button that generates a demo plot from built-in data
- Dynamic plot heights that adjust to your data
- Filtering by chain (TRA, TRB, IGH, etc.) and grouping variable

### 4. Spatial Transcriptomics Tab

If your Seurat object contains spatial data (Visium, Xenium, or other coordinate-based platforms), a **Spatial** tab will appear automatically in the Cerebro interface. The tab is hidden when no spatial data is present.

**Two visualization modes:**

| Mode             | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| ImageDimPlot     | Color cells by metadata (cell type, sample, cluster, etc.)   |
| ImageFeaturePlot | Color cells by gene expression with a continuous color scale |

**Interactive features:**

- **Box / Lasso selection** — select regions of interest and view composition tables and plots for the selected cells
- **Tissue image overlay** — optional background image with adjustable opacity and flip/scale controls
- **Group filtering** — filter displayed cells by any combination of grouping variables
- **Cell subsampling** — "Show % of cells" slider for performance on large datasets
- **Point customization** — size, opacity, border, and group label toggles
- **2D / 3D auto-detection** — automatically adapts to the coordinate dimensions

**Supported coordinate formats:**

| Platform     | Columns used           |
| ------------ | ---------------------- |
| Visium       | `imagerow`, `imagecol` |
| Xenium / FOV | `x`, `y` (centroids)   |

### 5. Authentication

`createTraditionalShinyApp()` supports optional login protection via the `enable_auth` parameter. Two authentication backends are available:

| Backend          | Style                    | Speed  | Shiny Server in Docker | Look & Feel |
| ---------------- | ------------------------ | ------ | ---------------------- | ----------- |
| `"custom"`       | Static HTML + JS + CSS   | Fast   | Not compatible         | Modern      |
| `"shinymanager"` | R package (shinymanager) | Slower | Compatible             | Basic       |

**Custom backend** (`auth_style = "custom"`, default):

- A static HTML login page is rendered immediately — no waiting for Shiny to initialize
- Passwords are hashed with SHA-256 + salt and stored in an RDS file (never in plain text)
- Rate limiting (max 5 attempts per minute) prevents brute-force attacks
- On success the login overlay fades out and the main app appears seamlessly

**shinymanager backend** (`auth_style = "shinymanager"`):

- Uses the [shinymanager](https://github.com/datastorm-open/shinymanager) package with an SQLite credentials database
- Works in Docker containers (Shiny Server)

```r
createTraditionalShinyApp(
  cerebro_data = c(`My data` = "cerebro.crb"),
  enable_auth  = TRUE,
  auth_style   = "custom",        # or "shinymanager"
  admin_user   = "admin",
  admin_pass   = "s3cret",
  users        = c("viewer1"),
  users_pass   = c("pass123")
)
```

### 6. Other Improvements

- **Seurat v5 / BPCells support** — works with on-disk expression matrices for large datasets
- **HDF5 matrix support** — via `HDF5Array` for memory-efficient storage
- Loading spinners on all plot outputs

## License

MIT — see [LICENSE.md](LICENSE.md). Original cerebroApp © Roman Hillje.
