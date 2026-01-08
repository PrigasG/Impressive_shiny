
#libraries-------------------
library(shiny)
library(bs4Dash)
library(shinyjs)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(readr)



# EXPECTED HEADERS------------
FACILITIES_HEADERS <- c(
  "FACILITY_TYPE",
  "LICENSE NUMBER",
  "FACILITY NAME",
  "GENERIC EMAIL ADDRESS",
  "ADDRESS",
  "CITY",
  "STATE",
  "ZIP CODE",
  "COUNTY",
  "TELEPHONE",
  "ALL_LTC.FACEMAIL"
)

NURSING_HEADERS <- c(
  "Facility Type",
  "NJID",
  "Provider Name",
  "Facility webpage",
  "Medicare profile page",
  "Financial statements"
)


# SAFE HELPERS ---------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

is_effectively_empty <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(is.na(x))
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(is.na(x))
  
  y <- as.character(x)
  y <- str_replace_all(y, "\u00A0", " ")
  y <- str_trim(y)
  y2 <- str_to_lower(y)
  
  is.na(y) | y == "" | y2 %in% c("na", "n/a", "null", "none", "nan")
}


trim_trailing_empty_rows <- function(df) {
  if (nrow(df) == 0) return(df)
  row_empty <- apply(df, 1, function(r) all(is_effectively_empty(r)))
  if (!any(!row_empty)) return(df[0, , drop = FALSE])
  last_nonempty <- max(which(!row_empty))
  df[seq_len(last_nonempty), , drop = FALSE]
}

drop_nan_artifact_rows <- function(df, id_col) {
  if (nrow(df) == 0) return(df)
  
  all_empty <- apply(df, 1, function(r) all(is_effectively_empty(r)))
  
  if (!is.null(id_col) && id_col %in% names(df)) {
    id_empty <- is_effectively_empty(df[[id_col]])
    mostly_empty <- apply(df, 1, function(r) mean(is_effectively_empty(r)) >= 0.90)
    drop <- all_empty | (id_empty & mostly_empty)
  } else {
    drop <- all_empty
  }
  
  df[!drop, , drop = FALSE]
}


normalize_id_generic <- function(x, force_prefix_NJ = FALSE) {
  y <- as.character(x)
  y <- str_replace_all(y, "\u00A0", " ")
  y <- str_trim(y)
  y <- str_replace(y, ",+$", "") # trailing comma artifacts
  y[str_to_lower(y) %in% c("", "na", "n/a", "null", "none", "nan")] <- NA_character_
  
  y
}


audit_row <- function(file, severity, check, detail) {
  tibble(file = file, severity = severity, check = check, detail = detail)
}


# FILE DETECTION -------------
norm_hdr <- function(x) {
  x %>%
    str_replace_all("\u00A0", " ") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9\\.]+", " ") %>%
    str_squish()
}


detect_file_type <- function(colnames_vec) {
  got <- norm_hdr(colnames_vec)
  fac <- norm_hdr(FACILITIES_HEADERS)
  nur <- norm_hdr(NURSING_HEADERS)
  
  fac_hits <- sum(fac %in% got)
  nur_hits <- sum(nur %in% got)
  
  # choose best match (ties -> unknown)
  if (fac_hits >= 6 && fac_hits > nur_hits) return("facilities")
  if (nur_hits >= 4 && nur_hits > fac_hits) return("nursing_urls")
  return("unknown")
}


# FACILITIES PIPELINE --------
standardize_facilities_headers <- function(df) {
  n0 <- names(df)
  
  norm <- function(s) {
    s %>%
      str_replace_all("\u00A0", " ") %>%
      str_trim() %>%
      str_to_upper() %>%
      str_replace_all("[^A-Z0-9\\.]+", " ") %>%  # keep dot
      str_squish()
  }
  
  n_norm <- norm(n0)
  expected_norm <- norm(FACILITIES_HEADERS)
  
  find_col <- function(target_norm) {
    hits <- which(n_norm == target_norm)
    if (length(hits) == 0) NA_integer_ else hits[[1]]
  }
  
  out <- list()
  for (i in seq_along(FACILITIES_HEADERS)) {
    idx <- find_col(expected_norm[[i]])
    out[[FACILITIES_HEADERS[[i]]]] <- if (!is.na(idx)) df[[idx]] else NA
  }
  as_tibble(out)
}

apply_ltc_filter_facilities <- function(df) {
  if (!("FACILITY_TYPE" %in% names(df))) return(df)
  ft <- str_to_upper(str_trim(as.character(df$`FACILITY_TYPE`)))
  keep <- !is.na(ft) & ft == "LONG TERM CARE FACILITY"
  df[keep, , drop = FALSE]
}


validate_facilities <- function(df_fixed, file_label, raw_names = NULL) {
  issues <- list()
  
  has_nan_tokens <- any(str_to_lower(trimws(as.character(unlist(df_fixed)))) == "nan", na.rm = TRUE)
  if (isTRUE(has_nan_tokens)) issues <- append(issues, list(
    audit_row(file_label, "WARN", "NaN tokens detected", "Found literal 'nan' tokens; historically breaks merges.")
  ))
  
  if ("FACILITY_TYPE" %in% names(df_fixed)) {
    other <- sum(!is.na(df_fixed$`FACILITY_TYPE`) &
                   str_to_upper(str_trim(as.character(df_fixed$`FACILITY_TYPE`))) != "LONG TERM CARE FACILITY")
    if (other > 0) issues <- append(issues, list(
      audit_row(file_label, "WARN", "Non-LTC rows present",
                paste0(other, " row(s) are not LONG TERM CARE FACILITY."))
    ))
  }
  
  if ("LICENSE NUMBER" %in% names(df_fixed)) {
    miss <- sum(is.na(df_fixed$`LICENSE NUMBER`) | str_trim(as.character(df_fixed$`LICENSE NUMBER`)) == "", na.rm = TRUE)
    if (miss > 0) issues <- append(issues, list(
      audit_row(file_label, "WARN", "Missing LICENSE NUMBER", paste0(miss, " row(s) missing LICENSE NUMBER."))
    ))
    
    dups <- df_fixed %>%
      filter(!is.na(`LICENSE NUMBER`), str_trim(as.character(`LICENSE NUMBER`)) != "") %>%
      count(`LICENSE NUMBER`) %>% filter(n > 1)
    
    if (nrow(dups) > 0) issues <- append(issues, list(
      audit_row(file_label, "WARN", "Duplicate LICENSE NUMBER",
                paste0("Duplicated IDs: ", nrow(dups),
                       " (examples: ", paste(head(dups$`LICENSE NUMBER`, 5), collapse = ", "), ")"))
    ))
  }
  
  if ("ALL_LTC.FACEMAIL" %in% names(df_fixed)) {
    if (all(is_effectively_empty(df_fixed$`ALL_LTC.FACEMAIL`))) {
      issues <- append(issues, list(
        audit_row(file_label, "INFO", "ALL_LTC.FACEMAIL", "Present (empty is expected).")
      ))
    }
  }
  
  if (length(issues) == 0) return(audit_row(file_label, "INFO", "Validation", "No issues detected."))
  bind_rows(issues) %>% arrange(factor(severity, levels = c("ERROR","WARN","INFO")))
}



process_facilities <- function(df_raw, ltc_only) {
  before <- list(rows = nrow(df_raw), cols = ncol(df_raw))
  
  missing_required_added <- count_missing_required_cols(names(df_raw), FACILITIES_HEADERS)
  
  df1 <- trim_trailing_empty_rows(df_raw)
  df2 <- standardize_facilities_headers(df1)
  df3 <- drop_nan_artifact_rows(df2, id_col = "LICENSE NUMBER")
  df4 <- if (isTRUE(ltc_only)) apply_ltc_filter_facilities(df3) else df3
  df4$`LICENSE NUMBER` <- normalize_id_generic(df4$`LICENSE NUMBER`, force_prefix_NJ = FALSE)
  
  after <- list(rows = nrow(df4), cols = ncol(df4))
  
  # extras based on normalized header match
  extra_cols <- names(df_raw)[ !(norm_hdr(names(df_raw)) %in% norm_hdr(FACILITIES_HEADERS)) ]
  
  fixes <- list(
    trailing_rows_removed     = before$rows - nrow(df1),
    artifact_rows_removed     = nrow(df2) - nrow(df3),
    ltc_filter_applied        = isTRUE(ltc_only),
    ltc_rows_removed          = if (isTRUE(ltc_only)) rows_removed(nrow(df3), nrow(df4)) else 0L,
    extra_cols_dropped        = length(extra_cols),
    missing_required_added    = missing_required_added
  )
  
  list(df = df4, before = before, after = after, fixes = fixes)
}



# NURSING URLS PIPELINE ------
standardize_nursing_headers <- function(df) {
  n0 <- names(df)
  
  n_norm <- norm_hdr(n0)
  expected_norm <- norm_hdr(NURSING_HEADERS)
  
  find_col <- function(target_norm) {
    hits <- which(n_norm == target_norm)
    if (length(hits) == 0) NA_integer_ else hits[[1]]
  }
  
  out <- list()
  for (i in seq_along(NURSING_HEADERS)) {
    idx <- find_col(expected_norm[[i]])
    out[[NURSING_HEADERS[[i]]]] <- if (!is.na(idx)) df[[idx]] else NA
  }
  as_tibble(out)
}


validate_nursing <- function(df_fixed, file_label, raw_names = NULL) {
  issues <- list()
  
  if ("NJID" %in% names(df_fixed)) {
    miss <- sum(is.na(df_fixed$NJID) | str_trim(as.character(df_fixed$NJID)) == "", na.rm = TRUE)
    if (miss > 0) issues <- append(issues, list(
      audit_row(file_label, "WARN", "Missing NJID", paste0(miss, " row(s) missing NJID."))
    ))
    
    bad <- sum(!is.na(df_fixed$NJID) & !str_detect(toupper(df_fixed$NJID), "^NJ[0-9]+$"), na.rm = TRUE)
    if (bad > 0) issues <- append(issues, list(
      audit_row(file_label, "WARN", "NJID format",
                paste0(bad, " row(s) not in expected NJ##### pattern. Note and proceed if irrelevant."))
    ))
  }
  
  has_nan_tokens <- any(str_to_lower(trimws(as.character(unlist(df_fixed)))) == "nan", na.rm = TRUE)
  if (isTRUE(has_nan_tokens)) issues <- append(issues, list(
    audit_row(file_label, "WARN", "NaN tokens detected", "Found literal 'nan' tokens; treat as ingestion artifacts.")
  ))
  
  if (length(issues) == 0) return(audit_row(file_label, "INFO", "Validation", "No issues detected."))
  bind_rows(issues) %>% arrange(factor(severity, levels = c("ERROR","WARN","INFO")))
}


process_nursing <- function(df_raw) {
  before <- list(rows = nrow(df_raw), cols = ncol(df_raw))
  
  missing_required_added <- count_missing_required_cols(names(df_raw), NURSING_HEADERS)
  
  df1 <- trim_trailing_empty_rows(df_raw)
  df2 <- standardize_nursing_headers(df1)
  df3 <- drop_nan_artifact_rows(df2, id_col = "NJID")
  df3$NJID <- normalize_id_generic(df3$NJID)
  
  after <- list(rows = nrow(df3), cols = ncol(df3))
  
  extra_cols <- names(df_raw)[ !(norm_hdr(names(df_raw)) %in% norm_hdr(NURSING_HEADERS)) ]
  
  fixes <- list(
    trailing_rows_removed  = before$rows - nrow(df1),
    artifact_rows_removed  = nrow(df2) - nrow(df3),
    extra_cols_dropped     = length(extra_cols),
    missing_required_added = missing_required_added
  )
  
  list(df = df3, before = before, after = after, fixes = fixes)
}


# READERS --------------------
read_xlsx_safely <- function(path) {
  tryCatch(readxl::read_xlsx(path), error = function(e) {
    structure(list(error = TRUE, message = conditionMessage(e)), class = "read_error")
  })
}

read_csv_safely <- function(path) {
  tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) {
    structure(list(error = TRUE, message = conditionMessage(e)), class = "read_error")
  })
}


# CLEANING LEDGER----------------
new_clean_log <- function() {
  tibble::tibble(step = character(), metric = character(), value = character())
}

log_step <- function(log, step, metric, value) {
  dplyr::bind_rows(log, tibble::tibble(step = step, metric = metric, value = as.character(value)))
}


# --- Header drift using normalized header matching ---
schema_drift_report <- function(raw_names, expected_names, file_label, strict_missing = TRUE) {
  raw_norm <- norm_hdr(raw_names)
  exp_norm <- norm_hdr(expected_names)
  
  missing_norm <- setdiff(exp_norm, raw_norm)
  extra_norm   <- setdiff(raw_norm, exp_norm)
  
  exp_map <- stats::setNames(expected_names, exp_norm)
  
  missing_pretty <- unname(exp_map[missing_norm])
  extra_pretty   <- raw_names[raw_norm %in% extra_norm]
  
  issues <- tibble::tibble()
  
  if (length(missing_pretty)) {
    issues <- dplyr::bind_rows(issues, audit_row(
      file_label,
      if (isTRUE(strict_missing)) "ERROR" else "WARN",
      "Headers missing in upload",
      paste(missing_pretty, collapse = ", ")
    ))
  }
  
  if (length(extra_pretty)) {
    issues <- dplyr::bind_rows(issues, audit_row(
      file_label,
      "WARN",
      "Unexpected columns in upload (dropped in export)",
      paste(extra_pretty, collapse = ", ")
    ))
  }
  
  list(
    missing = missing_pretty,
    extra   = extra_pretty,
    issues  = issues
  )
}

# --- count missing required cols
count_missing_required_cols <- function(raw_names, expected_names) {
  raw_norm <- norm_hdr(raw_names)
  exp_norm <- norm_hdr(expected_names)
  length(setdiff(exp_norm, raw_norm))
}

# --- ltc filter
rows_removed <- function(before_rows, after_rows) {
  as.integer(before_rows %||% 0) - as.integer(after_rows %||% 0)
}


has_strict_header_error <- function(issues_tbl) {
  if (is.null(issues_tbl) || !nrow(issues_tbl)) return(FALSE)
  any(issues_tbl$severity == "ERROR" & issues_tbl$check %in% c("Headers missing in upload"))
}

# Markdown table renderer 
md_table <- function(df) {
  if (is.null(df) || !nrow(df)) return("_None._\n")
  df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))
  
  header <- paste0("| ", paste(names(df), collapse = " | "), " |\n")
  sep    <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |\n")
  rows   <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |\n"))
  
  paste0(header, sep, paste(rows, collapse = ""))
}





# UI THEME -------------------
app_theme_css <- tags$head(tags$style(HTML("
  :root{
    --bg: #f6f7fb;
    --card: #ffffff;
    --border: #e6e8ef;
    --text: #111827;
    --muted: #6b7280;
    --muted2: #9ca3af;

    --brand: #2f6f8f;
    --brand-2: #295d78;

    --chip-bg: #eef2ff;
    --chip-text: #3730a3;
    --chip-dot: #6366f1;

    --danger-bg: #fee2e2;
    --warn-bg: #ffedd5;
    --info-bg: #e0f2fe;
  }

  /* Page */
  .content-wrapper { background: var(--bg) !important; }
  .content { padding: 18px 18px 70px 18px !important; }

  /* Navbar */
  .main-header.navbar,
  .main-header .navbar {
    background: #fff !important;
    border-bottom: 1px solid var(--border) !important;
  }

  /* Sidebar: soften */
  .main-sidebar {
    background: #ffffff !important;
    border-right: 1px solid var(--border) !important;
  }
  .nav-sidebar .nav-item > .nav-link {
    border-radius: 10px;
    margin: 2px 10px;
    color: #374151;
    font-weight: 600;
  }
  .nav-sidebar .nav-item > .nav-link:hover {
    background: #f3f4f6;
    color: #111827;
  }
  .nav-sidebar .nav-item > .nav-link.active {
    background: rgba(47,111,143,0.12) !important;
    color: #1f2937 !important;
  }

  /* Brand link */
  .brand-link {
    background: #fff !important;
    border-bottom: 1px solid var(--border) !important;
  }

  /* Cards */
  .card {
    background: var(--card) !important;
    border: 1px solid rgba(17,24,39,0.04) !important;
    box-shadow: 0 8px 22px rgba(17,24,39,0.06) !important;
    border-radius: 14px !important;
  }
  .card-header {
    background: #fff !important;
    border-bottom: 1px solid var(--border) !important;
    border-radius: 14px 14px 0 0 !important;
    padding: 14px 16px !important;
  }
  .card-title {
    font-size: 15px !important;
    font-weight: 750 !important;
    letter-spacing: -0.02em !important;
    color: var(--text) !important;
    margin: 0 !important;
    display: inline-flex;
    align-items: center;
    gap: 8px;
  }
  .card-body { padding: 16px !important; }

  /* Kill loud status headers if bs4Dash sets them */
  .card-header.bg-primary,
  .card-header.bg-info,
  .card-header.bg-success,
  .card-header.bg-warning,
  .card-header.bg-danger,
  .card-header.bg-secondary {
    background: #fff !important;
    color: var(--text) !important;
  }
  .card-header .card-title, .card-header .card-title i {
    color: var(--text) !important;
  }

  /* KPI boxes */
  .small-box, .value-box, .bs4-value-box {
    border-radius: 14px !important;
    border: 1px solid rgba(17,24,39,0.04) !important;
    box-shadow: 0 8px 22px rgba(17,24,39,0.06) !important;
  }
  .small-box h3, .small-box p { color: #fff !important; }

  /* Buttons */
  .btn { border-radius: 12px !important; font-weight: 750 !important; }
  .btn-primary {
    background: var(--brand) !important;
    border-color: var(--brand) !important;
    color: #fff !important;
    box-shadow: 0 10px 18px rgba(47,111,143,0.20) !important;
  }
  .btn-primary:hover, .btn-primary:focus {
    background: var(--brand-2) !important;
    border-color: var(--brand-2) !important;
    transform: translateY(-1px);
  }
  .btn-primary i { opacity: 1 !important; }

  .btn-outline-primary{
    background: #fff !important;
    border-color: #cbd5e1 !important;
    color: #111827 !important;
  }
  .btn-outline-primary:hover, .btn-outline-primary:focus{
    background: #f8fafc !important;
    border-color: #94a3b8 !important;
  }

  /* Upload helper text */
  .muted { color: var(--muted); font-size: 13px; line-height: 1.35; }
  .kpi-sub { color: var(--muted); font-size: 12px; margin-top: 6px; }

  /* Pills */
  .pill { display:inline-block; padding:2px 10px; border-radius:999px; font-size:12px; margin-right:6px; font-weight: 750; }
  .pill-err { background: var(--danger-bg); }
  .pill-warn { background: var(--warn-bg); }
  .pill-info { background: var(--info-bg); }

  /* Detection badge */
  .badge-soft {
    display:inline-flex; align-items:center; gap:8px;
    padding:7px 12px; border-radius:999px;
    font-size:12px; font-weight:800;
    background: var(--chip-bg); color: var(--chip-text);
  }
  .badge-soft .dot { width:8px; height:8px; border-radius:999px; background: var(--chip-dot); display:inline-block; }

  /* Upload area spacing */
  .upload-block { display:flex; flex-direction:column; gap:10px; }
  .upload-actions { display:flex; flex-wrap:wrap; gap:10px; margin-top: 6px; }
  .divider-soft { height: 1px; background: var(--border); margin: 10px 0; }

  /* Responsive action stacking */
  @media (max-width: 575.98px){
    .upload-actions > * { width: 100%; }
    .upload-actions > * + * { margin-top: 6px; }
  }

  /* DT styling: cleaner */
  table.dataTable thead th {
    background: #f8fafc !important;
    border-bottom: 1px solid var(--border) !important;
    color: #111827 !important;
    font-weight: 800 !important;
    font-size: 12px !important;
    text-transform: none !important;
  }
  table.dataTable tbody td {
    border-top: 1px solid #f1f5f9 !important;
    font-size: 13px !important;
  }
  table.dataTable tbody tr:hover { background: #f8fafc !important; }
  
  
  #ltc_hint {
  color: #6b7280;
  }
  #ltc_hint:hover {
    color: #2f6f8f;
    text-decoration: none;
  }


")))

navbar_title_hide <- tags$head(tags$style(HTML("
  .sidebar-mini.sidebar-collapse .brand-link span{display:none!important;}
  .sidebar-mini.sidebar-collapse .main-sidebar:hover .brand-link span,
  .sidebar-mini.sidebar-collapse:not(.sidebar-collapse) .brand-link span,
  body:not(.sidebar-collapse) .brand-link span{display:inline!important;}
  .brand-link i{display:inline-block!important;}
  .brand-link, .brand-title { display: inline-flex; align-items: center; gap: 8px; }
  .brand-title i { line-height: 1; }
  .brand-link { justify-content: center; }
")))


navbar_title_css <- tags$head(tags$style(HTML("
  .main-header .navbar-nav { width: 100%; }
  
  .main-header .nav-item {
    display: flex !important;
    align-items: center !important;
  }
  
  .navbar-title-right{
    margin-left: 0 !important;
    margin-right: 220px;
    display: flex;
    align-items: center;
    gap: 10px;
    padding-right: 16px;
    white-space: nowrap;
    height: 100%;
  }
  
  .navbar-title-right .navbar-icon{
    font-size: 18px;
    color: #4f46e5;
    opacity: 0.95;
  }
  
  .navbar-title-right .navbar-text-wrapper{
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    justify-content: center;
    line-height: 1.05;
  }
  
  .navbar-title-right .navbar-title{
    font-size: 14px;
    font-weight: 900;
    letter-spacing: -0.02em;
    color: #111827;
    margin: 0;
  }
  
  .navbar-title-right .navbar-subtitle{
    font-size: 11px;
    color: #9ca3af;
    font-weight: 800;
    text-transform: uppercase;
    letter-spacing: 0.6px;
    margin: 0;
  }
  
  .nav-link.disabled,
  .nav-link:disabled {
    opacity: 0.45;
    pointer-events: none;
    cursor: not-allowed;
  }
  
  @media (max-width: 768px){
    .navbar-title-right{ margin-right: 12px; }
  }
")))


sidebar_active_fix_css <- tags$head(tags$style(HTML("
  /* --- Sidebar active item (normal) --- */
  .main-sidebar .nav-sidebar .nav-link.active{
    border-radius: 12px;
    margin: 6px 10px;
    background: rgba(47, 111, 143, 0.12) !important;  /* soft */
    color: #0f172a !important;
    box-shadow: 0 1px 2px rgba(0,0,0,0.06);
  }
  .main-sidebar .nav-sidebar .nav-link.active i{
    color: #2f6f8f !important;
  }

  .main-sidebar .nav-sidebar .nav-link.active::before{
    content: \"\";
    position: absolute;
    left: 8px;
    top: 10px;
    bottom: 10px;
    width: 4px;
    border-radius: 6px;
    background: #2f6f8f;
    opacity: 0.85;
  }
  .main-sidebar .nav-sidebar .nav-link{
    position: relative;
  }

  .sidebar-mini.sidebar-collapse .main-sidebar .nav-sidebar .nav-link{
    margin: 8px 8px !important;
    border-radius: 14px !important;
    justify-content: center;
  }
  .sidebar-mini.sidebar-collapse .main-sidebar .nav-sidebar .nav-link.active{
    background: rgba(47, 111, 143, 0.16) !important;
    box-shadow: 0 2px 6px rgba(0,0,0,0.10);
  }
  .sidebar-mini.sidebar-collapse .main-sidebar .nav-sidebar .nav-link.active::before{
    /* hide the left bar in collapsed mode (it can look weird) */
    display: none;
  }
")))


fileinput_polish_css <- tags$head(tags$style(HTML("
/*input */
  .shiny-input-container .input-group{
    border-radius: 12px;
    overflow: hidden; /* forces consistent corners */
    box-shadow: 0 1px 2px rgba(0,0,0,0.06);
  }

  /* Browse button */
  .shiny-input-container .input-group .input-group-btn .btn{
    border: 0 !important;
    background: #2f6f8f !important;
    color: #fff !important;
    font-weight: 700 !important;
    padding: 10px 14px !important;
  }
  .shiny-input-container .input-group .input-group-btn .btn:hover{
    background: #295d78 !important;
  }

  .shiny-input-container .input-group .form-control{
    border: 0 !important;
    background: #f1f5f9 !important;
    color: #111827 !important;
    padding: 10px 12px !important;
  }

  .shiny-input-container:has(input[type='file']){
    padding: 12px;
    border-radius: 14px;
    border: 1px dashed #cbd5e1;
    background: rgba(241,245,249,0.55);
  }

  .shiny-input-container .input-group:focus-within{
    outline: none;
    box-shadow: 0 0 0 3px rgba(47,111,143,0.18);
  }
")))


ltc_inline_css <- tags$head(tags$style(HTML("
  .ltc-row{
    display:flex;
    align-items:center;
    gap:10px;
    flex-wrap:wrap;
    margin-top:2px;
    transition: opacity 0.2s ease;
  }

  /* Disabled state styling */
  .disabled-section {
    opacity: 0.4;
    pointer-events: none;
  }

  /* checkbox alignment + sizing */
  .ltc-row .ltc-box{
    width:16px;
    height:16px;
    margin:0;
    transform: translateY(-1px);
    flex: 0 0 auto;
  }

  .ltc-row .ltc-label{
    font-weight:600;
    line-height:1.1;
    white-space:nowrap;
  }

  .ltc-row .ltc-why{
    font-size:13px;
    font-weight:600;
    color:#6b7280;
    white-space:nowrap;
    text-decoration:none;
  }
  .ltc-row .ltc-why:hover{
    color:#2f6f8f;
    text-decoration:none;
  }
  
  
  /* Make checkboxInput behave like your custom inline checkbox */
.ltc-row .shiny-input-container{
  margin: 0 !important;
  padding: 0 !important;
}
.ltc-row .shiny-input-container label{
  margin: 0 !important;
  padding: 0 !important;
  display: flex !important;
  align-items: center !important;
}

/* size + align checkbox itself */
.ltc-row input[type='checkbox']{
  width:16px;
  height:16px;
  margin:0;
  transform: translateY(-1px);
}


  @media (max-width: 480px){
    .ltc-row .ltc-label{ white-space:normal; }
  }
")))


qa_preview <- tags$head(tags$style(HTML(
  "
.report-overlay{
  position: fixed;
  inset: 0;
  background: rgba(15, 23, 42, 0.55);
  z-index: 99999;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 18px;
}

.report-panel{
  width: min(980px, 96vw);
  max-height: 90vh;
  background: #fff;
  border-radius: 16px;
  box-shadow: 0 18px 60px rgba(0,0,0,0.30);
  overflow: hidden;
  border: 1px solid rgba(17,24,39,0.10);
}

.report-header{
  display:flex;
  align-items:center;
  justify-content:space-between;
  padding: 12px 14px;
  border-bottom: 1px solid var(--border);
  background: #fff;
}

.report-title{
  display:flex;
  align-items:center;
  gap: 10px;
  font-weight: 900;
  color: #111827;
  letter-spacing: -0.02em;
}

.report-body{
  padding: 14px 16px;
  overflow: auto;
  max-height: calc(90vh - 56px);
  font-size: 13px;
  color: #111827;
}

.report-body h1{ font-size: 18px; margin: 0 0 10px 0; }
.report-body h2{ font-size: 15px; margin: 14px 0 8px 0; }
.report-body code{
  background: #f1f5f9;
  padding: 2px 6px;
  border-radius: 8px;
}
.report-body table{
  width: 100%;
  border-collapse: collapse;
  margin-top: 8px;
}
.report-body th, .report-body td{
  border: 1px solid #e5e7eb;
  padding: 8px;
  vertical-align: top;
}
.report-body th{
  background: #f8fafc;
  font-weight: 900;
}

  "
)))


reset_ui <- tags$head(tags$style(HTML(
  "
  /* Navbar Reset Button */
.reset-btn{
  display: inline-flex !important;
  align-items: center;
  gap: 6px;

  padding: 8px 14px !important;
  border-radius: 999px;

  font-weight: 800 !important;
  letter-spacing: -0.01em;

  color: #374151 !important;
  border: 1.5px solid #d1d5db;
  background: #ffffff;

  transition: all 0.15s ease-in-out;
}

/* Icon styling */
.reset-btn i{
  font-size: 14px;
  opacity: 0.85;
}

.reset-btn:hover{
  background: #f8fafc;
  border-color: #2f6f8f;
  color: #2f6f8f !important;
  box-shadow: 0 4px 12px rgba(47,111,143,0.18);
  transform: translateY(-1px);
}

.reset-btn:active{
  transform: translateY(0);
  box-shadow: none;
}

.reset-btn:focus-visible{
  outline: none;
  box-shadow: 0 0 0 3px rgba(47,111,143,0.25);
}

  "
)))


sidebar_alignment <- tags$head(tags$style(HTML(
  "
:root{
  --topbar-h: 57px;
}

.main-header.navbar,
.main-header .navbar{
  min-height: var(--topbar-h) !important;
  border-bottom: 1px solid var(--border) !important;
}

.main-sidebar .brand-link{
  height: var(--topbar-h) !important;
  display: flex !important;
  align-items: center !important;

  padding: 0 14px !important;
  border-bottom: 1px solid var(--border) !important;


  line-height: 1 !important;
}

.sidebar-mini.sidebar-collapse .main-sidebar .brand-link{
  height: var(--topbar-h) !important;
  padding: 0 !important;
  justify-content: center !important;
}

.main-sidebar{
  padding-top: 0 !important;
  border-right: 1px solid var(--border) !important;
}


.main-sidebar .brand-link,
.main-header.navbar{
  box-sizing: border-box !important;
}

  "
  
)))


buttons_val <- tags$head(tags$style(HTML(
  
"
.btn.disabled,
.btn:disabled,
a.btn.disabled,
a.btn[disabled]{
  pointer-events: none !important;
  opacity: 0.45 !important;
  cursor: not-allowed !important;
}
"
  
)))


#UI--------------
ui <- bs4DashPage(
  title = "OLTCR Files Checker",
  help = NULL,
  dark = NULL,
  
  header = bs4DashNavbar(
    skin = "light",
    status = "white",
    border = TRUE,
    
    title = tagList(
      icon("file-shield"),
      span("OLTCR", style="font-weight:900; letter-spacing:-0.02em;")
    ),
    
    rightUi = tagList(
      
      tags$li(
        class = "nav-item dropdown",
        tags$div(
          class = "navbar-title-right",
          icon("shield-halved", class = "navbar-icon"),
          tags$div(
            class = "navbar-text-wrapper",
            tags$p(class = "navbar-title", "OLTCR Files Checker"),
            tags$p(class = "navbar-subtitle", "Facilities + Nursing URLs")
          )
        )
      ),
      
      tags$li(
        class = "nav-item dropdown",
        style = "margin-left: 10px;",
        tags$a(
          class = "nav-link reset-btn",
          href = "#",
          onclick = "Shiny.setInputValue('reset_app', Math.random());",
          icon("arrow-rotate-left"),
          tags$span("Reset")
        )
      )
      
    )
    
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "white",
    elevation = 0,
    title = "OLTCR",
    bs4SidebarMenu(id = "tabs", 
      bs4SidebarMenuItem("Ingestion QA", tabName = "qa", icon = icon("clipboard-check")),
      bs4SidebarMenuItem("Help", tabName = "help", icon = icon("circle-question")),
      tags$div(style = "display:none;",
        bs4SidebarMenuItem("QA Report Preview",tabName = "qa_preview",icon = icon("file-lines")
        ))
    )
  ),
  
  body = bs4DashBody(
    shinyjs::useShinyjs(),
    app_theme_css, navbar_title_hide, navbar_title_css,
    fileinput_polish_css, sidebar_active_fix_css, ltc_inline_css, reset_ui, sidebar_alignment,
    buttons_val,
    #qa_preview,
    
    tags$head(tags$script(HTML("
  Shiny.addCustomMessageHandler('showToast', function(msg) {
    $(document).Toasts('create', {
      class: msg.class,
      title: msg.title,
      body: msg.body,
      autohide: true,
      delay: 3000
    })
  });
"))),
    
    
    bs4TabItems(
      #main page----------------
      bs4TabItem(
        tabName = "qa",
        
        # KPIs
        fluidRow(
          bs4ValueBoxOutput("kpi_rows", width = 3),
          bs4ValueBoxOutput("kpi_cols", width = 3),
          bs4ValueBoxOutput("kpi_warn", width = 3),
          bs4ValueBoxOutput("kpi_err",  width = 3)
        ),
        
        # Main row
        fluidRow(
          bs4Card(
            title = tagList(icon("file-arrow-up"), "Upload + Validate"),
            solidHeader = TRUE,
            width = 5,
            status = NULL,
            
            div(
              class = "upload-block",
              
              fileInput("file_any", "Choose file", accept = c(".xlsx", ".csv")),
              
              div(
                uiOutput("file_detect_badge"),
                div(class="muted", style="margin-top:6px;", uiOutput("file_detect_text"))
              ),
              
              tags$div(class="divider-soft"),
              div(
                id = "ltc_section",
                class = "ltc-row",
                
                # real Shiny checkbox (reliable TRUE/FALSE from the start)
                checkboxInput("ltc_only", label = NULL, value = TRUE),
                
                tags$span(class = "ltc-label", "LTC facilities only"),
                
                actionLink("ltc_hint",
                           label = tagList(icon("circle-info"), "Why?"),
                           class = "ltc-why"
                )
              ),
              
              shinyjs::hidden(
                div(
                  id = "ltc_hint_text",
                  class = "muted",
                  style = "margin-top:6px;",
                  "Prevents non-LTC facilities from being included in final data."
                )
              ),
              div(
                class = "upload-actions",
                actionButton(
                  "run",
                  label = tagList(icon("play"), "Process & Validate"),
                  class = "btn btn-primary"
                ),
                downloadButton(
                  "download_fixed",
                  label = tagList( "Export Clean Output"),
                  class = "btn btn-outline-primary"
                ),
                uiOutput("export_hint"),
                
                downloadButton(
                  "download_report",
                  "Download QA Report",
                  class = "btn btn-outline-secondary",
                  icon = icon("file-lines")
                ),
                
                actionButton(
                  "preview_report",
                  "Preview QA Report",
                  class = "btn btn-outline-secondary",
                  icon = icon("eye")
                )
              ),
              
              div(
                style="margin-top:10px;",
                uiOutput("audit_pills"),
                tags$div(class="kpi-sub", uiOutput("audit_text"))
              )
            ),
            
            tags$div(class="divider-soft"),
            
            bs4Card(
              title = tagList(icon("wand-magic-sparkles"), "Fixes applied"),
              solidHeader = TRUE,
              width = 12,
              status = NULL,
              uiOutput("fixes_applied")
            )
          ),
          
          bs4Card(
            title = tagList(icon("list-check"), "Validation Report"),
            solidHeader = TRUE,
            width = 7,
            status = NULL,
            DTOutput("issues_tbl")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = uiOutput("preview_title"), 
            solidHeader = TRUE,
            width = 12,
            status = NULL,
            div(style="width:100%; overflow-x:auto;", DTOutput("preview_tbl"))
          )
        )
      ),
      #help ---------------------
      bs4TabItem(
        tabName = "help",
        bs4Card(
          title = tagList(icon("circle-question"), "How detection + exports work"),
          solidHeader = TRUE,
          width = 12,
          status = NULL,
          
          tags$div(
            class="muted",
            "This tool is a data-quality gate for monthly OLTCR ingestion. It standardizes headers, removes trailing blank rows, and drops nan/empty artifact rows without altering real data fields."
          ),
          tags$div(class="divider-soft"),
          
          tags$ul(
            tags$li(tags$b("Facilities file:"), " input ", tags$code(".xlsx"), " → output ", tags$code("OLTCR Facilities.xlsx")),
            tags$li(tags$b("Nursing URLs file:"), " input ", tags$code(".xlsx/.csv"), " → output ", tags$code("Nursing Home URLs.csv")),
            tags$li(tags$b("Safe fixes only:"), " trailing blank rows removed, nan/empty artifact rows removed, and header standardization."),
            tags$li(tags$b("Why this matters:"), " prevents facility drops and merge failures downstream.")
          )
        )
      ),
      #qa preview ---------------
      bs4TabItem(
        tabName = "qa_preview",
        
        fluidRow(
          bs4Card(
            title = tagList(icon("file-lines"), "QA Report Preview"),
            width = 12,
            solidHeader = TRUE,
            
            div(
              style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
              
              tags$span(
                class = "muted",
                "Preview of the QA report that can be downloaded and shared"
              ),
              
              actionButton(
                "qa_preview_back",
                label = tagList(icon("arrow-left"), "Back to Ingestion QA"),
                class = "btn btn-outline-secondary btn-sm"
              )
            ),
            
            tags$hr(),
            
            uiOutput("report_preview_ui")
          )
        )
      )
      
    )
  ),
  
  footer = bs4DashFooter(
    fixed = TRUE,
    left  = tags$span("NJDOH • OLTCR • Data Quality Gate", style="color:#6b7280; font-size:13px;"),
    right = tags$span(format(Sys.Date(), "%B %d, %Y"), style="color:#9ca3af; font-size:13px;")
  )
)


#SERVER---------------
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 1000 * 1024^2)
  
  rv <- reactiveValues(
    df_fixed   = NULL,
    issues     = tibble(),
    before     = NULL,
    after      = NULL,
    file_label = NULL,
    file_type  = NULL,
    fixes      = NULL,
    warn_log   = character(0),
    error_log  = NULL,
    cleaning_applied = FALSE
  )
  
  # --- LTC checkbox  ---
  ltc_is_on <- reactive({
    v <- input$ltc_only
    isTRUE(v) || identical(v, "on") || identical(v, 1L) || identical(v, 1)
  })
  
  
  make_report_lines <- function() {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    file_name <- rv$file_label %||% "(none)"
    file_type <- rv$file_type %||% "(not detected)"
    
    # ---- issues table as markdown ----
    issues_tbl <- rv$issues
    issues_md <- ""
    if (!is.null(issues_tbl) && nrow(issues_tbl)) {
      issues_tbl <- issues_tbl %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))
      header <- paste0("| ", paste(names(issues_tbl), collapse = " | "), " |\n")
      sep    <- paste0("| ", paste(rep("---", ncol(issues_tbl)), collapse = " | "), " |\n")
      rows   <- apply(issues_tbl, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |\n"))
      issues_md <- paste0(header, sep, paste(rows, collapse = ""))
    } else {
      issues_md <- "_No issues recorded._\n"
    }
    
    # ---- cleaning steps  ----
    steps <- tibble::tibble()
    if (!is.null(rv$fixes) && length(rv$fixes)) {
      
      add_step <- function(step, metric, value) {
        tibble::tibble(step = step, metric = metric, value = as.character(value))
      }
      
      steps <- dplyr::bind_rows(
        steps,
        add_step("trim_trailing_empty_rows", "rows_removed", rv$fixes$trailing_rows_removed %||% 0),
        add_step("standardize_headers", "extra_cols_dropped", rv$fixes$extra_cols_dropped %||% 0),
        add_step("standardize_headers", "missing_required_added", rv$fixes$missing_required_added %||% 0),
        add_step("drop_nan_artifact_rows", "rows_removed", rv$fixes$artifact_rows_removed %||% 0),
        
        if (identical(rv$file_type, "facilities")) {
          dplyr::bind_rows(
            add_step("apply_ltc_filter", "applied", isTRUE(rv$fixes$ltc_filter_applied)),
            add_step("apply_ltc_filter", "rows_removed", rv$fixes$ltc_rows_removed %||% 0)
          )
        } else NULL,
        
        if (identical(rv$file_type, "facilities")) {
          add_step("normalize_id", "column", "LICENSE NUMBER")
        } else if (identical(rv$file_type, "nursing_urls")) {
          add_step("normalize_id", "column", "NJID")
        } else NULL
      )
    }
    
    steps_md <- ""
    if (nrow(steps)) {
      header <- "| step | metric | value |\n"
      sep    <- "| --- | --- | --- |\n"
      rows   <- apply(steps, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |\n"))
      steps_md <- paste0(header, sep, paste(rows, collapse = ""))
    } else {
      steps_md <- "_No cleaning steps recorded._\n"
    }
    
    # ---- warnings + errors ----
    warns <- unique(rv$warn_log)
    warn_md <- if (length(warns)) paste0(paste0("- ", warns), collapse = "\n") else "_No warnings captured._"
    err_md  <- if (!is.null(rv$error_log)) paste0("**Unhandled error:** ", rv$error_log) else "_No unhandled errors captured._"
    
    # ---- before / after ----
    before_txt <- if (!is.null(rv$before)) paste0(rv$before$rows, " rows, ", rv$before$cols, " cols") else "(n/a)"
    after_txt  <- if (!is.null(rv$after))  paste0(rv$after$rows,  " rows, ", rv$after$cols,  " cols") else "(n/a)"
    
    # ---- export gating in report ----
    export_allowed <- isTRUE(rv$cleaning_applied)
    
    # ---- env ----
    pkgs <- c("shiny","bs4Dash","DT","readxl","openxlsx","readr","dplyr","stringr","tidyr","shinyjs","markdown")
    pkg_versions <- vapply(pkgs, function(p) {
      if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "not installed"
    }, FUN.VALUE = character(1))
    pkg_md <- paste0(paste0("- ", pkgs, ": ", pkg_versions), collapse = "\n")
    
    c(
      "# **OLTCR QA Report**",
      "",
      paste0("- **Generated:** ", ts),
      paste0("- **Uploaded file:** ", file_name),
      paste0("- **Detected type:** ", file_type),
      "",
      "## **Run summary**",
      paste0("- **Before:** ", before_txt),
      paste0("- **After:** ", after_txt),
      paste0("- **LTC-only filter (Facilities only):** ", ifelse(ltc_is_on(), "ON", "OFF")),
      paste0("- **Export allowed:** ", ifelse(export_allowed, "YES", "NO")),
      "",
      "## **Cleaning steps applied**",
      steps_md,
      "",
      "## **Validation issues**",
      issues_md,
      "",
      "## **Unhandled error**",
      err_md,
      ""
    )
  }
  
  
  # --- Enable/disable buttons  ---
  observe({
    has_output <- !is.null(rv$df_fixed) && nrow(rv$df_fixed) > 0
    
    if (!has_output) {
      shinyjs::disable("download_fixed")
      shinyjs::disable("download_report")
      shinyjs::disable("preview_report")
      return()
    }
    
    shinyjs::enable("download_report")
    shinyjs::enable("preview_report")
    
    if (isTRUE(rv$cleaning_applied)) shinyjs::enable("download_fixed")
    else shinyjs::disable("download_fixed")
  })
  
  session$onFlushed(function() {
    shinyjs::disable("download_fixed")
    shinyjs::disable("download_report")
    shinyjs::disable("preview_report")
  }, once = TRUE)
  
  # --- action buttons availability ---
  observe({
    if (is.null(rv$file_type)) {
      shinyjs::disable("ltc_only")
      shinyjs::disable("ltc_hint")
      shinyjs::addClass("ltc_section", "disabled-section")
    } else if (rv$file_type == "facilities") {
      shinyjs::enable("ltc_only")
      shinyjs::enable("ltc_hint")
      shinyjs::removeClass("ltc_section", "disabled-section")
    } else {
      shinyjs::disable("ltc_only")
      shinyjs::disable("ltc_hint")
      shinyjs::addClass("ltc_section", "disabled-section")
    }
  })
  
  
  # File detection badge/text -------
  
  output$file_detect_badge <- renderUI({
    if (is.null(rv$file_type)) return(NULL)
    lbl <- switch(
      rv$file_type,
      facilities   = "Detected: OLTCR Facilities file",
      nursing_urls = "Detected: Nursing Home URLs file",
      unknown      = "Detected: Unknown file"
    )
    tags$span(class = "badge-soft", tags$span(class = "dot"), lbl)
  })
  
  output$file_detect_text <- renderUI({
    if (is.null(rv$file_type)) return("Upload a file and click Process & Validate.")
    if (rv$file_type == "facilities") {
      HTML(paste0(
        "Expected output headers: <br><code>",
        paste(FACILITIES_HEADERS, collapse = " | "),
        "</code>"
      ))
    } else if (rv$file_type == "nursing_urls") {
      HTML(paste0(
        "Expected output headers: <br><code>",
        paste(NURSING_HEADERS, collapse = " | "),
        "</code>"
      ))
    } else {
      "We could not confidently detect the schema from headers. Please verify you selected the correct file."
    }
  })
  
  # --- Hint toggle ---
  observeEvent(input$ltc_hint, {
    shinyjs::toggle("ltc_hint_text")
  })
  
  # --- Ingest + validate ---
  observeEvent(input$run, {
    shinyjs::disable("run")
    shinyjs::html("run", HTML('<i class="fa fa-spinner fa-spin"></i> Processing...'))
    
    on.exit({
      shinyjs::enable("run")
      shinyjs::html("run", HTML('<i class="fa fa-play"></i> Process & Validate'))
    })
    
    if (is.null(input$file_any)) {
      showNotification("Please upload a file first.", type = "warning", duration = 3)
      return()
    }
    
    rv$warn_log  <- character(0)
    rv$error_log <- NULL
    rv$cleaning_applied <- FALSE
    
    ext <- tolower(tools::file_ext(input$file_any$name))
    rv$file_label <- input$file_any$name
    
    tryCatch(
      withCallingHandlers({
        
        # ----- Read -----
        df0 <- NULL
        if (ext == "xlsx") df0 <- read_xlsx_safely(input$file_any$datapath)
        else if (ext == "csv") df0 <- read_csv_safely(input$file_any$datapath)
        else {
          rv$file_type <- "unknown"
          rv$df_fixed <- NULL
          rv$fixes <- NULL
          rv$issues <- audit_row(rv$file_label, "ERROR", "File type", "Only .xlsx or .csv supported.")
          return(NULL)
        }
        
        if (inherits(df0, "read_error")) {
          rv$file_type <- "unknown"
          rv$df_fixed <- NULL
          rv$fixes <- NULL
          rv$issues <- audit_row(rv$file_label, "ERROR", "Read file", df0$message)
          return(NULL)
        }
        
        raw_names <- names(df0)
        
        # ----- Detect schema -----
        rv$file_type <- detect_file_type(raw_names)
        
        # ----- Facilities -----
        if (rv$file_type == "facilities") {
          
          if (ext != "xlsx") {
            rv$df_fixed <- NULL
            rv$fixes <- NULL
            rv$issues <- audit_row(rv$file_label, "ERROR", "Facilities upload", "Facilities file must be .xlsx.")
            return(NULL)
          }
          
          # Strict drift check, normalized matching
          drift <- schema_drift_report(raw_names, FACILITIES_HEADERS, rv$file_label, strict_missing = TRUE)
          
          res <- process_facilities(df0, ltc_only = ltc_is_on())
          rv$df_fixed <- res$df
          rv$before   <- res$before
          rv$after    <- res$after
          rv$fixes    <- res$fixes
          
          # Cleaning rule
          rv$cleaning_applied <- isTRUE(
            (rv$fixes$trailing_rows_removed %||% 0) > 0 ||
              (rv$fixes$artifact_rows_removed %||% 0) > 0 ||
              (rv$fixes$extra_cols_dropped %||% 0) > 0 ||
              (rv$fixes$missing_required_added %||% 0) > 0 ||
              (isTRUE(rv$fixes$ltc_filter_applied) && ((rv$fixes$ltc_rows_removed %||% 0) > 0))
          )
          
          audit <- dplyr::bind_rows(
            audit_row(rv$file_label, "INFO", "Detected file", "OLTCR Facilities file"),
            audit_row(
              rv$file_label, "INFO", "Cleanup summary",
              paste0(
                "Rows ", rv$before$rows, " \u2192 ", rv$after$rows,
                "; Cols ", rv$before$cols, " \u2192 ", rv$after$cols,
                " (ALL_LTC.FACEMAIL preserved)."
              )
            ),
            if (ltc_is_on()) audit_row(rv$file_label, "INFO", "LTC-only filter", "Kept only LONG TERM CARE FACILITY rows.") else NULL,
            
            # for missing required cols, warn user
            if ((rv$fixes$missing_required_added %||% 0) > 0) audit_row(
              rv$file_label, "WARN", "Schema repaired (review required)",
              paste0(
                "Added ", rv$fixes$missing_required_added,
                " missing required column(s) with blank rows. Please review the Preview/Clean Output."
              )
            ) else NULL
          )
          
          # Cdrift issues + audit + post-standardization validation
          rv$issues <- dplyr::bind_rows(
            audit,
            drift$issues,
            validate_facilities(rv$df_fixed, rv$file_label)
          )
          
          return(TRUE)
        }
        
        # ----- Nursing URLs -----
        if (rv$file_type == "nursing_urls") {
          
          drift <- schema_drift_report(raw_names, NURSING_HEADERS, rv$file_label, strict_missing = TRUE)
          
          res <- process_nursing(df0)
          rv$df_fixed <- res$df
          rv$before   <- res$before
          rv$after    <- res$after
          rv$fixes    <- res$fixes
          
          rv$cleaning_applied <- isTRUE(
            (rv$fixes$trailing_rows_removed %||% 0) > 0 ||
              (rv$fixes$artifact_rows_removed %||% 0) > 0 ||
              (rv$fixes$extra_cols_dropped %||% 0) > 0 ||
              (rv$fixes$missing_required_added %||% 0) > 0
          )
          
          audit <- dplyr::bind_rows(
            audit_row(rv$file_label, "INFO", "Detected file", "Nursing Home URLs file"),
            audit_row(
              rv$file_label, "INFO", "Cleanup summary",
              paste0(
                "Rows ", rv$before$rows, " \u2192 ", rv$after$rows,
                "; Cols ", rv$before$cols, " \u2192 ", rv$after$cols,
                " (output will be CSV)."
              )
            ),
            if ((rv$fixes$missing_required_added %||% 0) > 0) audit_row(
              rv$file_label, "WARN", "Schema repaired (review required)",
              paste0(
                "Added ", rv$fixes$missing_required_added,
                " missing required column(s) with blank rows. **Review Preview/Clean Output file**."
              )
            ) else NULL
          )
          
          rv$issues <- dplyr::bind_rows(
            audit,
            drift$issues,
            validate_nursing(rv$df_fixed, rv$file_label)
          )
          
          return(TRUE)
        }
        
        # ----- Unknown -----
        rv$df_fixed <- NULL
        rv$fixes <- NULL
        rv$cleaning_applied <- FALSE
        rv$issues <- audit_row(
          rv$file_label, "ERROR", "Schema detection",
          "Unknown schema. Header match was insufficient for Facilities or Nursing URLs."
        )
        return(NULL)
        
      }, warning = function(w) {
        rv$warn_log <- c(rv$warn_log, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
      error = function(e) {
        rv$error_log <- conditionMessage(e)
        rv$df_fixed <- NULL
        rv$fixes <- NULL
        rv$cleaning_applied <- FALSE
        rv$file_type <- ifelse(is.null(rv$file_type), "unknown", rv$file_type)
        rv$issues <- audit_row(rv$file_label %||% "unknown_file", "ERROR", "Unhandled error", conditionMessage(e))
        return(NULL)
      }
    )
    
    # Toasts
    if (!is.null(rv$df_fixed)) {
      if (isTRUE(rv$cleaning_applied)) {
        session$sendCustomMessage('showToast', list(
          class = 'bg-success',
          title = 'Processed',
          body = paste0('Output ready (', nrow(rv$df_fixed), ' rows). Export enabled.')
        ))
      } else {
        session$sendCustomMessage('showToast', list(
          class = 'bg-info',
          title = 'No cleaning needed',
          body = 'File looks clean — Export disabled (nothing changed). You can still download the QA report.'
        ))
      }
    }
  })
  
  # --- detect schema on file upload  ---
  observeEvent(input$file_any, {
    req(input$file_any)
    
    # label shows up in report + allows more helpful messaging later
    rv$file_label <- input$file_any$name
    
    ext <- tolower(tools::file_ext(input$file_any$name))
    
    # read just enough to detect schema by headers
    df0 <- NULL
    if (ext == "xlsx") df0 <- read_xlsx_safely(input$file_any$datapath)
    if (ext == "csv")  df0 <- read_csv_safely(input$file_any$datapath)
    
    if (is.null(df0) || inherits(df0, "read_error")) {
      rv$file_type <- "unknown"
      return()
    }
    
    rv$file_type <- detect_file_type(names(df0))
    
    rv$df_fixed <- NULL
    rv$issues <- tibble()
    rv$before <- NULL
    rv$after <- NULL
    rv$fixes <- NULL
    rv$warn_log <- character(0)
    rv$error_log <- NULL
    rv$cleaning_applied <- FALSE
  })
  
  
  # --- Fixes applied card---
  output$fixes_applied <- renderUI({
    if (is.null(rv$fixes) || is.null(rv$file_type)) {
      return(tags$div(class = "muted", "No ingestion run yet."))
    }
    
    if (rv$file_type == "facilities") {
      tags$ul(
        class = "fixes-list",
        tags$li(tags$strong("Trailing blank rows removed:"), paste0(" ", rv$fixes$trailing_rows_removed)),
        tags$li(tags$strong("NaN/Empty artifact rows removed:"), paste0(" ", rv$fixes$artifact_rows_removed)),
        tags$li(tags$strong("Extra columns dropped:"), paste0(" ", rv$fixes$extra_cols_dropped %||% 0)),
        tags$li(tags$strong("Missing required columns added:"), paste0(" ", rv$fixes$missing_required_added %||% 0)),
        tags$li(tags$strong("ID-only cleanup:"), " Trailing commas removed"),
        tags$li(tags$strong("Header enforcement:"), " Columns fixed to required schema"),
        tags$li(tags$strong("LTC-only filter:"), if (isTRUE(rv$fixes$ltc_filter_applied)) " applied" else " not applied")
      )
    } else if (rv$file_type == "nursing_urls") {
      tags$ul(
        class = "fixes-list",
        tags$li(tags$strong("Trailing blank rows removed:"), paste0(" ", rv$fixes$trailing_rows_removed)),
        tags$li(tags$strong("NaN/empty artifact rows removed:"), paste0(" ", rv$fixes$artifact_rows_removed)),
        tags$li(tags$strong("Extra columns dropped:"), paste0(" ", rv$fixes$extra_cols_dropped %||% 0)),
        tags$li(tags$strong("Missing required columns added:"), paste0(" ", rv$fixes$missing_required_added %||% 0)),
        tags$li(tags$strong("ID-only cleanup:"), " NJID trimmed (no prefix enforced)"),
        tags$li(tags$strong("Header enforcement:"), " output columns fixed to required schema"),
        tags$li(tags$strong("Output format:"), " CSV (always)")
      )
    } else {
      tags$div(class = "muted", "No fixes available (unknown file type).")
    }
  })
  
  
  # ---- KPIs ----
  output$kpi_rows <- renderbs4ValueBox({
    n <- if (!is.null(rv$df_fixed)) nrow(rv$df_fixed) else 0
    bs4ValueBox(value = format(n, big.mark = ","), subtitle = "Rows",
                icon = icon("lines-leaning"), color = "info")
  })
  
  output$kpi_cols <- renderbs4ValueBox({
    n <- if (!is.null(rv$df_fixed)) ncol(rv$df_fixed) else 0
    bs4ValueBox(value = n, subtitle = "Columns",
                icon = icon("table-columns"), color = "primary")
  })
  
  output$kpi_warn <- renderbs4ValueBox({
    n <- if (nrow(rv$issues)) sum(rv$issues$severity == "WARN") else 0
    bs4ValueBox(value = n, subtitle = "Warnings",
                icon = icon("triangle-exclamation"),
                color = if (n > 0) "warning" else "success")
  })
  
  output$kpi_err <- renderbs4ValueBox({
    n <- if (nrow(rv$issues)) sum(rv$issues$severity == "ERROR") else 0
    bs4ValueBox(value = n, subtitle = "Errors",
                icon = icon("circle-xmark"),
                color = if (n > 0) "danger" else "success")
  })
  
  # ---- Audit pills + text ----
  output$preview_title <- renderUI({
    base_title <- tagList(icon("table"), "Preview (Cleaned / Standardized)")
    if (!is.null(rv$df_fixed)) {
      tagList(base_title, tags$span(class = "badge badge-light ml-2",
                                    paste0(format(nrow(rv$df_fixed), big.mark = ","), " rows")))
    } else base_title
  })
  
  output$audit_pills <- renderUI({
    if (!nrow(rv$issues)) return(NULL)
    
    sev_counts <- rv$issues %>%
      count(severity) %>%
      complete(severity = c("ERROR", "WARN", "INFO"), fill = list(n = 0))
    
    pill <- function(sev, n) {
      cls <- switch(sev, "ERROR"="pill pill-err", "WARN"="pill pill-warn", "INFO"="pill pill-info")
      tags$span(class = cls, paste0(sev, ": ", n))
    }
    
    tags$div(
      pill("ERROR", sev_counts$n[sev_counts$severity == "ERROR"]),
      pill("WARN",  sev_counts$n[sev_counts$severity == "WARN"]),
      pill("INFO",  sev_counts$n[sev_counts$severity == "INFO"])
    )
  })
  
  output$audit_text <- renderUI({
    if (is.null(rv$before) || is.null(rv$after) || is.null(rv$file_type)) {
      return(tags$span("No ingestion run yet."))
    }
    kind <- if (rv$file_type == "facilities") "Facilities" else if (rv$file_type == "nursing_urls") "Nursing URLs" else "Unknown"
    tags$span(paste0(kind, ": Rows ", rv$before$rows, " → ", rv$after$rows,
                     " | Cols ", rv$before$cols, " → ", rv$after$cols))
  })
  
  # ---- Tables ----
  output$issues_tbl <- renderDT({
    if (!nrow(rv$issues)) {
      return(datatable(
        tibble(Message = "Upload a file and click Process & Validate."),
        selection = list(mode = "single", target = "row"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    datatable(rv$issues, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$preview_tbl <- renderDT({
    if (is.null(rv$df_fixed)) {
      return(datatable(
        tibble(Message = "Process a file to see preview"),
        selection = list(mode = "single", target = "row"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    datatable(head(rv$df_fixed, 300), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Export clean output ----
  output$download_fixed <- downloadHandler(
    filename = function() {
      if (is.null(rv$file_type)) return("clean_output")
      if (rv$file_type == "facilities") return("OLTCR Facilities.xlsx")
      if (rv$file_type == "nursing_urls") return("Nursing Home URLs.csv")
      "clean_output"
    },
    content = function(file) {
      req(rv$df_fixed)
      
      # Hard-stop if strict schema issues
      if (isTRUE(rv$strict_block_export)) {
        stop("Export disabled: missing required headers in upload. See QA report.")
      }
      
      if (rv$file_type == "facilities") {
        wb <- createWorkbook()
        addWorksheet(wb, "Facilities")
        writeDataTable(wb, "Facilities", rv$df_fixed, withFilter = TRUE)
        freezePane(wb, "Facilities", firstRow = TRUE)
        setColWidths(wb, "Facilities", cols = 1:ncol(rv$df_fixed), widths = "auto")
        saveWorkbook(wb, file, overwrite = TRUE)
      } else if (rv$file_type == "nursing_urls") {
        readr::write_csv(rv$df_fixed, file)
      } else {
        stop("Unknown file type; cannot export.")
      }
    }
  )
  
  # Hint under export button
  output$export_hint <- renderUI({
    req(!is.null(rv$df_fixed))
    
    if (isTRUE(rv$strict_block_export)) {
      return(tags$div(class="muted", style="margin-top:6px;",
                      icon("triangle-exclamation"),
                      " Export disabled due to missing required headers in the upload."))
    }
    
    if (!isTRUE(rv$cleaning_applied)) {
      return(tags$div(class="muted", style="margin-top:6px;",
                      icon("circle-info"),
                      " Export disabled because no cleaning was applied."))
    }
    
    NULL
  })
  
  # ---- QA report preview ----
  output$report_preview_ui <- renderUI({
    if (is.null(rv$file_label) && is.null(rv$df_fixed) && !nrow(rv$issues)) {
      return(tags$div(class="muted", "Run a validation to preview the QA report."))
    }
    
    md_lines <- make_report_lines()
    html <- markdown::markdownToHTML(
      text = paste(md_lines, collapse = "\n"),
      fragment.only = TRUE
    )
    HTML(html)
  })
  
  observeEvent(input$preview_report, {
    if (is.null(rv$df_fixed)) {
      showNotification("Run Process & Validate first.", type = "warning")
      return()
    }
    updateTabItems(session, "tabs", selected = "qa_preview")
  })
  
  observeEvent(input$qa_preview_back, {
    updateTabItems(session, "tabs", selected = "qa")
  })
  
  # ---- Reset ----
  observeEvent(input$reset_app, {
    rv$df_fixed   <- NULL
    rv$issues     <- tibble()
    rv$before     <- NULL
    rv$after      <- NULL
    rv$file_label <- NULL
    rv$file_type  <- NULL
    rv$fixes      <- NULL
    rv$clean_log  <- NULL
    rv$warn_log   <- character(0)
    rv$error_log  <- NULL
    rv$cleaning_applied <- FALSE
    rv$strict_block_export <- FALSE
    
    shinyjs::reset("file_any")
    shinyjs::hide("ltc_hint_text")
    
    shinyjs::disable("download_fixed")
    shinyjs::disable("download_report")
    shinyjs::disable("preview_report")
    
    session$sendCustomMessage('showToast', list(
      class = 'bg-info',
      title = 'Reset',
      body = 'Cleared current file and results'
    ))
    
    gc()
  })
  
  # ---- Download QA Report (Markdown) ----
  output$download_report <- downloadHandler(
    filename = function() {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("OLTCR_QA_Report_", ts, ".md")
    },
    content = function(file) {
      writeLines(make_report_lines(), file)
    }
  )
  
}


shinyApp(ui, server)
