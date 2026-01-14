# Load necessary libraries
if (!require("stats")) install.packages("stats")

# -------------------------------------------------------------------------
# 1. Helper Functions for Cell Formats
# -------------------------------------------------------------------------

#' Create an estimate object with Standard Error
#' @param b The coefficient estimate (float)
#' @param se The standard error (float)
#' @param p_val (Optional) Override p-value. If NULL, calculated from Normal dist.
est <- function(b, se, p_val = NULL) {
  structure(list(b = b, se = se, p = p_val), class = "cell_est")
}

#' Create an estimate object with Confidence Intervals
#' @param b The coefficient estimate (float)
#' @param lower Lower bound of CI
#' @param upper Upper bound of CI
est_ci <- function(b, lower, upper) {
  structure(list(b = b, l = lower, u = upper), class = "cell_est_ci")
}

# -------------------------------------------------------------------------
# 2. The Main Function
# -------------------------------------------------------------------------

#' Generate a Custom LaTeX Table
#'
#' @param filename String. Output filename (e.g., "table1.tex").
#' @param rows Vector of strings. Default row names (used if panels don't specify their own).
#' @param cols Vector of strings. Column names.
#' @param cells List of cell contents. For single-panel tables (backward compatibility).
#' @param panels List of panels. Each panel is a list with: name, cells, and optionally rows.
#'               Example: list(list(name="Panel A", cells=list(...)), list(name="Panel B", rows=c("X","Y"), cells=list(...)))
#' @param caption String. Title of the table.
#' @param ind Logical. If TRUE, adds (1), (2)... numbering below column names.
#' @param stats Character vector. Names of statistics rows (e.g., "R-squared").
#' @param stat_cells List. Content for the stats cells (length = n_stats * n_cols).
#' @param data_source Optional data.frame. Used if cells contain coordinate vectors c(r, c).
#' @param note String. Bottom note.
#' @param col_width String. LaTeX width for content columns (e.g., "2.5cm").
#' @param font_size String. LaTeX font size (e.g., "\\small", "\\scriptsize").
#'
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
  
  # Helper to escape special LaTeX characters
  tex_clean <- function(x) {
    if (is.numeric(x)) return(as.character(x))
    x <- gsub("\\%", "\\\\%", x)
    x <- gsub("\\_", "\\\\_", x)
    x <- gsub("\\&", "\\\\&", x)
    return(x)
  }
  
  n_cols <- length(cols)
  
  # Handle backward compatibility: if panels is NULL, create single panel from cells
  if (is.null(panels)) {
    if (is.null(cells) || is.null(rows)) {
      stop("Error: Must provide either 'panels' argument or both 'rows' and 'cells' arguments.")
    }
    panels <- list(list(name = NULL, rows = rows, cells = cells))
  }
  
  # Validate each panel
  for (p_idx in seq_along(panels)) {
    panel <- panels[[p_idx]]
    
    # If panel doesn't have rows specified, use default
    if (is.null(panel$rows)) {
      if (is.null(rows)) {
        stop(paste("Error: Panel", p_idx, "has no rows specified and no default rows provided."))
      }
      panels[[p_idx]]$rows <- rows
    }
    
    # Validate cell count for this panel
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
    # A. Handle Coordinate Lookup c(row, col) if data_source exists
    if (!is.null(data_source) && is.numeric(content) && length(content) == 2 && !inherits(content, "cell_est")) {
      # Try to fetch from data source
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
      
      # Significance
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
      # Stack elements vertically
      clean_content <- sapply(content, tex_clean)
      return(paste0("\\begin{tabular}{@{}c@{}}", paste(clean_content, collapse = " \\\\ "), "\\end{tabular}"))
    }
    
    # E. Handle Primitives (String/Float)
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
  
  # Define Column Format
  col_def <- paste0("l ", paste(rep(paste0(">{\\centering\\arraybackslash}p{", col_width, "}"), n_cols), collapse = " "))
  lines <- c(lines, paste0(font_size, " \\begin{tabular}{", col_def, "}"))
  lines <- c(lines, "\\toprule")
  
  # Headers
  header_row <- paste(c("", sapply(cols, tex_clean)), collapse = " & ")
  lines <- c(lines, paste0(header_row, " \\\\"))
  
  # Optional Indicator Row (1) (2) ...
  if (ind) {
    ind_nums <- paste0("(", 1:n_cols, ")")
    ind_row <- paste(c("", ind_nums), collapse = " & ")
    lines <- c(lines, paste0(ind_row, " \\\\"))
  }
  lines <- c(lines, "\\midrule")
  
  # --- 4. Process Each Panel ---
  
  for (p_idx in seq_along(panels)) {
    panel <- panels[[p_idx]]
    
    # Panel header (if named)
    if (!is.null(panel$name) && panel$name != "") {
      total_cols <- n_cols + 1  # Include the row label column
      panel_header <- paste0("\\multicolumn{", total_cols, "}{l}{\\textit{", 
                            tex_clean(panel$name), "}} \\\\")
      lines <- c(lines, panel_header)
      lines <- c(lines, "\\midrule")
    }
    
    # Panel Body Rows
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
    
    # Add separator between panels (except after last panel)
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
  
  # Note
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