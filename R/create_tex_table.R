#' Create an estimate object with Standard Error
#'
#' Helper function to create a cell containing a coefficient estimate with
#' standard error in parentheses and significance stars.
#'
#' @param b Numeric. The coefficient estimate
#' @param se Numeric. The standard error
#' @param p_val Numeric. Optional p-value override. If NULL, calculated from Normal distribution
#'
#' @return An object of class 'cell_est'
#' @export
#'
#' @examples
#' # Create an estimate with auto-calculated significance
#' est(0.452, 0.100)
#'
#' # Create an estimate with custom p-value
#' est(0.452, 0.100, p_val = 0.03)
est <- function(b, se, p_val = NULL) {
  structure(list(b = b, se = se, p = p_val), class = "cell_est")
}

#' Create an estimate object with Confidence Intervals
#'
#' Helper function to create a cell containing a coefficient estimate with
#' confidence interval in brackets below.
#'
#' @param b Numeric. The coefficient estimate
#' @param lower Numeric. Lower bound of confidence interval
#' @param upper Numeric. Upper bound of confidence interval
#'
#' @return An object of class 'cell_est_ci'
#' @export
#'
#' @examples
#' est_ci(1.2, 0.9, 1.5)
est_ci <- function(b, lower, upper) {
  structure(list(b = b, l = lower, u = upper), class = "cell_est_ci")
}

#' Generate a Custom LaTeX Table
#'
#' Creates professionally formatted LaTeX tables with support for multi-panel layouts,
#' nested statistics, confidence intervals, and significance stars. Ideal for
#' academic regression tables and research output.
#'
#' @param filename String. Output filename (e.g., "table1.tex")
#' @param rows Character vector. Default row names (used if panels don't specify their own)
#' @param cols Character vector. Column names
#' @param cells List. Cell contents for single-panel tables (backward compatibility)
#' @param panels List of lists. Each panel contains: name, cells, and optionally rows.
#'   Example: list(list(name="Panel A", cells=list(...)), list(name="Panel B", rows=c("X","Y"), cells=list(...)))
#' @param caption String. Table caption
#' @param ind Logical. If TRUE, adds (1), (2), ... numbering below column names
#' @param stats Character vector. Names of statistics rows (e.g., "R-squared", "N")
#' @param stat_cells List. Content for statistics cells (length = n_stats * n_cols)
#' @param data_source Data frame. Optional data source for coordinate-based cell references
#' @param note String. Table footnote
#' @param col_width String. LaTeX width for content columns (e.g., "2.5cm")
#' @param font_size String. LaTeX font size command (e.g., "\\small", "\\scriptsize")
#'
#' @return NULL (writes table to file)
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple single-panel table
#' create_tex_table(
#'   filename = "results.tex",
#'   rows = c("Variable A", "Variable B"),
#'   cols = c("Model 1", "Model 2"),
#'   cells = list(
#'     est(0.452, 0.100), est(0.380, 0.120),
#'     est_ci(1.2, 0.9, 1.5), est_ci(1.1, 0.8, 1.4)
#'   ),
#'   caption = "Regression Results",
#'   ind = TRUE,
#'   stats = c("R-Squared", "N"),
#'   stat_cells = list(0.89, 0.91, 1250, 1250),
#'   note = "Notes: * p<0.1; ** p<0.05; *** p<0.01."
#' )
#'
#' # Multi-panel table
#' create_tex_table(
#'   filename = "multi_panel.tex",
#'   cols = c("Model 1", "Model 2"),
#'   panels = list(
#'     list(
#'       name = "Panel A: Main Results",
#'       rows = c("Variable A", "Variable B"),
#'       cells = list(est(0.45, 0.1), est(0.38, 0.12), est(0.34, 0.09), est(0.31, 0.11))
#'     ),
#'     list(
#'       name = "Panel B: Robustness",
#'       rows = c("Control X", "Control Y"),
#'       cells = list(est(0.23, 0.08), est(0.21, 0.09), est(0.19, 0.07), est(0.18, 0.08))
#'     )
#'   ),
#'   caption = "Multi-Panel Results"
#' )
#' }
create_tex_table <- function(filename, 
                             rows = NULL, 
                             cols, 
                             cells = NULL,
                             panels = NULL,
                             caption = "",
                             ind = FALSE, 
                             stats = NULL, 
                             stat_cells = NULL,
                             data_source = NULL, 
                             note = NULL,
                             col_width = "2.2cm",
                             font_size = "\\normalsize") {
  
  # --- 1. Sanitize Inputs ---
  
  tex_clean <- function(x) {
    if (is.numeric(x)) return(as.character(x))
    x <- gsub("\\%", "\\\\%", x)
    x <- gsub("\\_", "\\\\_", x)
    x <- gsub("\\&", "\\\\&", x)
    return(x)
  }
  
  n_cols <- length(cols)
  
  # Handle backward compatibility
  if (is.null(panels)) {
    if (is.null(cells) || is.null(rows)) {
      stop("Error: Must provide either 'panels' argument or both 'rows' and 'cells' arguments.")
    }
    panels <- list(list(name = NULL, rows = rows, cells = cells))
  }
  
  # Validate each panel
  for (p_idx in seq_along(panels)) {
    panel <- panels[[p_idx]]
    
    if (is.null(panel$rows)) {
      if (is.null(rows)) {
        stop(paste("Error: Panel", p_idx, "has no rows specified and no default rows provided."))
      }
      panels[[p_idx]]$rows <- rows
    }
    
    panel_rows <- panels[[p_idx]]$rows
    n_rows <- length(panel_rows)
    expected_cells <- n_rows * n_cols
    
    if (length(panel$cells) != expected_cells) {
      stop(paste("Error: Panel", p_idx, "has", length(panel$cells), "cells, but needs", 
                 expected_cells, "(", n_rows, "rows x", n_cols, "cols)."))
    }
  }
  
  # --- 2. Process Cells Function ---
  
  format_cell <- function(content) {
    # A. Handle Coordinate Lookup
    if (!is.null(data_source) && is.numeric(content) && length(content) == 2 && !inherits(content, "cell_est")) {
      tryCatch({
        content <- data_source[content[1], content[2]]
      }, error = function(e) {
        stop("Coordinate lookup failed in data_source.")
      })
    }
    
    # B. Handle est() - Estimate + SE + Stars
    if (inherits(content, "cell_est")) {
      b <- content$b
      se <- content$se
      
      if (is.null(content$p)) {
        z <- abs(b / se)
        pval <- 2 * (1 - pnorm(z))
      } else {
        pval <- content$p
      }
      
      stars <- ""
      if (pval < 0.01) stars <- "$^{***}$"
      else if (pval < 0.05) stars <- "$^{**}$"
      else if (pval < 0.1) stars <- "$^{*}$"
      
      val_str <- sprintf("%.3f%s", b, stars)
      se_str  <- sprintf("(%.3f)", se)
      return(paste0("\\begin{tabular}{@{}c@{}}", val_str, " \\\\ ", se_str, "\\end{tabular}"))
    }
    
    # C. Handle est_ci() - Estimate + [CI]
    if (inherits(content, "cell_est_ci")) {
      b <- content$b
      val_str <- sprintf("%.3f", b)
      ci_str  <- sprintf("[%s, %s]", round(content$l, 3), round(content$u, 3))
      return(paste0("\\begin{tabular}{@{}c@{}}", val_str, " \\\\ {}", ci_str, "\\end{tabular}"))
    }
    
    # D. Handle Vectors (Stacking content)
    if (length(content) > 1) {
      clean_content <- sapply(content, tex_clean)
      return(paste0("\\begin{tabular}{@{}c@{}}", paste(clean_content, collapse = " \\\\ "), "\\end{tabular}"))
    }
    
    # E. Handle Primitives
    if (is.numeric(content)) return(round(content, 3))
    if (is.character(content)) return(tex_clean(content))
    if (is.na(content)) return("")
    
    return(as.character(content))
  }
  
  # --- 3. Build LaTeX Content ---
  
  lines <- c()
  lines <- c(lines, "\\begin{table}[htbp]")
  lines <- c(lines, "\\centering")
  lines <- c(lines, "\\begin{threeparttable}")
  lines <- c(lines, paste0("\\caption{", caption, "}"))
  
  col_def <- paste0("l ", paste(rep(paste0(">{\\centering\\arraybackslash}p{", col_width, "}"), n_cols), collapse = " "))
  lines <- c(lines, paste0(font_size, " \\begin{tabular}{", col_def, "}"))
  lines <- c(lines, "\\toprule")
  
  # Headers
  header_row <- paste(c("", sapply(cols, tex_clean)), collapse = " & ")
  lines <- c(lines, paste0(header_row, " \\\\"))
  
  if (ind) {
    ind_nums <- paste0("(", 1:n_cols, ")")
    ind_row <- paste(c("", ind_nums), collapse = " & ")
    lines <- c(lines, paste0(ind_row, " \\\\"))
  }
  lines <- c(lines, "\\midrule")
  
  # --- 4. Process Each Panel ---
  
  for (p_idx in seq_along(panels)) {
    panel <- panels[[p_idx]]
    
    if (!is.null(panel$name) && panel$name != "") {
      total_cols <- n_cols + 1
      panel_header <- paste0("\\multicolumn{", total_cols, "}{l}{\\textit{", 
                            tex_clean(panel$name), "}} \\\\")
      lines <- c(lines, panel_header)
      lines <- c(lines, "\\midrule")
    }
    
    panel_rows <- panel$rows
    n_rows <- length(panel_rows)
    cell_idx <- 1
    
    for (i in 1:n_rows) {
      row_content <- c(tex_clean(panel_rows[i]))
      
      for (j in 1:n_cols) {
        curr_cell <- panel$cells[[cell_idx]]
        formatted <- format_cell(curr_cell)
        row_content <- c(row_content, formatted)
        cell_idx <- cell_idx + 1
      }
      
      lines <- c(lines, paste0(paste(row_content, collapse = " & "), " \\\\"))
      lines <- c(lines, "\\addlinespace[0.2cm]")
    }
    
    if (p_idx < length(panels)) {
      lines <- c(lines, "\\midrule")
    }
  }
  
  lines <- c(lines, "\\midrule")
  
  # --- 5. Stats Rows ---
  
  if (!is.null(stats)) {
    n_stats <- length(stats)
    if (length(stat_cells) != n_stats * n_cols) stop("Stat cells count mismatch.")
    
    stat_idx <- 1
    for (i in 1:n_stats) {
      s_row <- c(tex_clean(stats[i]))
      for (j in 1:n_cols) {
        curr_stat <- stat_cells[[stat_idx]]
        s_row <- c(s_row, format_cell(curr_stat)) 
        stat_idx <- stat_idx + 1
      }
      lines <- c(lines, paste0(paste(s_row, collapse = " & "), " \\\\"))
    }
    lines <- c(lines, "\\bottomrule")
  } else {
    lines <- c(lines, "\\bottomrule")
  }
  
  lines <- c(lines, "\\end{tabular}")
  
  if (!is.null(note)) {
    lines <- c(lines, "\\begin{tablenotes}")
    lines <- c(lines, "\\footnotesize")
    lines <- c(lines, paste0("\\item ", tex_clean(note)))
    lines <- c(lines, "\\end{tablenotes}")
  }
  
  lines <- c(lines, "\\end{threeparttable}")
  lines <- c(lines, "\\end{table}")
  
  # --- 6. Write to File ---
  writeLines(lines, filename)
  message(paste("Table successfully saved to:", filename))
}

