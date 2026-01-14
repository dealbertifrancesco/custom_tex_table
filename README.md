# customtextable

Custom LaTeX Table Generator for R

## Installation

Install directly from GitHub using `devtools`:

```r
# Install devtools if needed
install.packages("devtools")

# Install customtextable
devtools::install_github("dealbertifrancesco/custom_tex_table")
```

## Quick Start

```r
library(customtextable)

# Create a simple regression table
create_tex_table(
  filename = "results.tex",
  rows = c("Variable A", "Variable B"),
  cols = c("Model 1", "Model 2"),
  cells = list(
    est(0.452, 0.100),      # Coefficient with SE and stars
    est(0.380, 0.120),
    est_ci(1.2, 0.9, 1.5),  # Coefficient with CI
    est_ci(1.1, 0.8, 1.4)
  ),
  caption = "Regression Results",
  ind = TRUE,
  stats = c("R-Squared", "N"),
  stat_cells = list(0.89, 0.91, 1250, 1250),
  note = "Notes: * p<0.1; ** p<0.05; *** p<0.01."
)
```

## Features

- **Multi-panel support**
- **Confidence intervals or significance stars** 
- **Flexible cell content**

## Multi-Panel Tables

```r
create_tex_table(
  filename = "multi_panel.tex",
  cols = c("Model 1", "Model 2"),
  panels = list(
    list(
      name = "Panel A: Main Results",
      rows = c("Variable A", "Variable B"),
      cells = list(
        est(0.45, 0.1), est(0.38, 0.12),
        est(0.34, 0.09), est(0.31, 0.11)
      )
    ),
    list(
      name = "Panel B: Robustness Checks",
      rows = c("Control X", "Control Y"),
      cells = list(
        est(0.23, 0.08), est(0.21, 0.09),
        est(0.19, 0.07), est(0.18, 0.08)
      )
    )
  ),
  caption = "Multi-Panel Results"
)
```

## Required LaTeX Packages

Include these in your LaTeX preamble:

```latex
\usepackage{booktabs}
\usepackage{threeparttable}
\usepackage{array}
```

## License

MIT License - see LICENSE file for details

