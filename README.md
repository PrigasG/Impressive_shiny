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
```

## Portable / Packaged Deployment

For portable or packaged deployments, the app can be launched using helper scripts that allow non-technical users to run the application without manually configuring R or paths.

These helpers are optional but recommended for shared or distributed use.

### What the Launcher Does

The launcher workflow is designed to be self-contained and predictable:

- **Automatically detects R**
  - Prefers a bundled/portable R installation if present
  - Falls back to a system-installed R if available
  - Prompts the user only if R cannot be found

- **Configures local paths**
  - Sets a single shared data directory
  - Ensures all outputs, logs, and cached data are written consistently
  - Avoids hard-coded user-specific paths

- **Launches the app**
  - Opens the Shiny app in the default browser
  - Optionally launches in an embedded browser window when bundled
  - Handles port selection with automatic fallback

---

## Helper Files Overview

### `run.bat`

The main Windows launcher script.

Responsibilities:

- Resolves the root application directory
- Detects or selects the appropriate R executable
- Configures environment variables used by the app
- Initializes logging
- Launches the Shiny app via `run.R`

This allows the app to be started with a **double-click**, without requiring the user to open R or a terminal.

---

### `run.R`

The R-side bootstrapper.

Responsibilities:

- Loads required packages (installing missing ones if needed)
- Initializes the application data directory
- Applies environment-based configuration
- Selects a preferred port with automatic fallback
- Launches the Shiny app

This script ensures consistent startup behavior across machines.

---

### `LaunchApp.hta` (Optional)

A lightweight Windows helper used to:

- Hide the console window
- Launch `run.bat` silently
- Provide a clean, app-like startup experience

This is optional and can be omitted if console visibility is acceptable.

---

## OneDrive / Shared Folder Syncing

The app is designed to work seamlessly with synced folders such as **OneDrive**, **SharePoint**, or other cloud-backed directories.

### How Syncing Is Used

- A single **shared root folder** is designated
- All data files are read from and written to this location
- The folder is expected to be:
  - Synced locally
  - Available offline
  - Kept consistent across users

The app itself does **not** manage syncing. Instead, it relies on the operating system’s sync client to ensure files are available locally.

### Why This Matters

- Enables shared access to input/output files
- Prevents hard-coded user paths
- Supports team-based workflows
- Allows results to be reviewed or reused by others

---

## Design Philosophy

- **Explicit over implicit**  
  Every action is surfaced and documented.

- **Transparent over silent**  
  Cleaning, validation, and schema enforcement are always reported.

- **Reviewable over automatic**  
  Users are prompted to review changes instead of having them hidden.

- **Schema correctness before convenience**  
  Structural integrity is prioritized over permissive ingestion.

---

## What This App Does NOT Do

- It does **not** guess or infer missing data  
- It does **not** silently drop meaningful columns  
- It does **not** modify real data values  
- It does **not** auto-fix unknown schema changes  

Any non-standard change is surfaced and left for the user to resolve.

---

## Intended Use

- Data ingestion checkpoints  
- Pre-validation before automation  
- Manual QA workflows  
- Schema drift detection  
- Structured file handoffs between teams  

This app is intended to sit **before** automated pipelines, not replace human review.

---

## Summary

The OLTCR Files Checker is intentionally conservative:

- It protects downstream systems
- It preserves original data intent
- It documents every decision
- It keeps the user in control

If something changes unexpectedly, the app will tell you—and then step aside.

