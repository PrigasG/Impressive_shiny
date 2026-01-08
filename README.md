# OLTCR Files Checker

The **OLTCR Files Checker** is a lightweight **R Shiny application** designed to validate, standardize, and quality-check structured data files before they are used downstream. It acts as a **data ingestion gate**, ensuring files meet expected schemas while clearly surfacing anything that requires user review.

This project focuses on **safe, transparent cleaning** rather than silent modification, giving users confidence in what changed—and what did not.

---

## What This App Does

The app performs three core functions:

1. **Detect** the type of file based on column headers  
2. **Validate** the structure against a strict expected schema  
3. **Clean** only well-defined, safe artifacts and document every change  

Anything outside these rules is **flagged and left for the user to resolve**.

---

## Supported File Types

- `.xlsx`
- `.csv`

The app currently recognizes:
- **Facilities Generic Email files**
- **Nursing Home URL files**

Detection is based on header similarity, not filenames.

---

## Processing Workflow

1. Upload a file  
2. File type is detected immediately and displayed to the user  
3. User optionally selects domain filters (e.g., LTC-only)  
4. Click **Process & Validate**  
5. Review:
   - Validation report
   - Fixes applied
   - Cleaned data preview
6. Export results if applicable

---

## Cleaning Rules (Safe by Design)

The app applies **only deterministic, reversible fixes**:

- Remove trailing empty rows
- Drop NaN / mostly-empty artifact rows
- Normalize identifier fields (trim, remove artifacts)
- Enforce required headers and column order
- Apply optional domain filters (e.g., LTC-only rows)

No values are inferred, filled, or transformed beyond normalization.

---

## Schema Enforcement

- Required headers are strictly enforced  
- Missing required columns are **added empty and flagged**  
- Unexpected columns are **reported to the user**  
- Schema changes always trigger a **review warning**

Schema enforcement **allows export**, but explicitly signals the user to review the output.

---

## Validation & QA Reporting

Each run produces:

- **KPIs**
  - Rows
  - Columns
  - Warnings
  - Errors

- **Validation table**
  - ERROR / WARN / INFO severity levels
  - Human-readable explanations

- **Fixes Applied summary**
  - Exactly what was changed
  - What filters were applied
  - What was enforced

- **QA Report**
  - Markdown export
  - In-app preview

---

## Export Rules

- **Clean Output Export**
  - Enabled only when cleaning or schema enforcement occurred
  - Disabled when the file is already clean

- **QA Report Export**
  - Always available after processing

When export is disabled, the UI explains *why*.

---

## User Feedback & Guardrails

- File type is displayed immediately after upload
- Buttons are enabled/disabled based on state
- Toast notifications explain outcomes:
  - Successful cleaning
  - No cleaning needed
  - Review recommended

The app is designed to **prevent false confidence**.

---

## Technology Stack

- R
- Shiny / bs4Dash
- DT
- dplyr / tidyr / stringr
- readxl / readr
- openxlsx
- shinyjs

---

## Running the App

```r
shiny::runApp()
