# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the App

```r
# From R console (working directory: gRs_ShinyApp/)
shiny::runApp("gRs_ShinyApp/app.R")

# Or open app.R in RStudio and click "Run App"
```

## Dependency Management

This project does not use `renv` or any other project-local library. Packages resolve from the user's R library (R 4.5.x) and are installed with plain `install.packages()`.

The `gRs` package is a private GitHub package — install via:
```r
devtools::install_github("jenright-git/gRs")
```

## Architecture

The entire app is a single file: `gRs_ShinyApp/app.R`. There are no modules, helper files, or separate UI/server files.

## Goal

The goal of the app is to make environmental analysis, specifically Mann-Kendall trend tests as simple and informative as possible.  The layout should be clean, objects exportable or downloadable wherever possible and the interface intuitive and easy to use.


### Reactive Data Flow

```
file_input (xlsx upload)
  └─► processed_file()       reactive — data_processor() + format normalisation
        │                     returns list(data, n_raw, n_kept)
        └─► file_data()      reactive — processed_file()$data
              ├─► mk_results()     eventReactive(mk_button) — mann_kendall_test()
              │     └─► increasing_plot_obj()  reactive — faceted trend ggplot2
              └─► plotting_data()  eventReactive(update_plot_locations) — location filter + LOR multiplier
                    ├─► ts_plot_obj()    reactive — full ggplot2 timeseries (shared by render + download)
                    ├─► hist_plot_obj()  reactive — ggplot2 histogram (shared by render + download)
                    └─► boxplot_obj()    reactive — ggplot2 boxplot (shared by render + download)
```

`plotting_data()` is behind a task button, so it does not update until the user explicitly clicks "Update Locations". Analyte filtering for Charts happens *inside* each plot reactive, not at the `plotting_data()` level.

### Key Design Patterns

**`processed_file()` normalises the export before anything else sees it**: `data_processor()` now auto-detects the report family and matches on column names, so what it returns varies by export. `processed_file()` (a) rejects a non-chemistry file with a `validate()` message instead of letting downstream reactives fail on a missing `chem_name`, (b) adds `chem_group = NA_character_` when the export has none — ESDAT `Chemistry List` exports do not carry a chemical group, and `establish_plotting_variables()` reads that column unconditionally — and (c) optionally applies `select_max_concentration()`. Anything that assumes a column exists belongs here, not in the plot reactives.

**Two sample-selection modes, and the default is the strict one**: ESDAT exports carry `Field_D` / `Interlab_D` QC rows *and* repeat analyses of the same sample by different methods; left in, they enter the Mann-Kendall test as extra independent samples. `processed_file()` handles this in one of two ways, reported back to the user via `dedupe_note`:

- **`dedupe_check` unticked (default, mode `"normal"`)** — keep `sample_type == "Normal"` only, then `distinct()` on `location_code, field_id, date, chem_name, concentration, prefix, output_unit`. Note that a bare `distinct()` over all columns removes *nothing*: an ESDAT migration row and its re-reported twin differ in `method_name`, `method_type` and the lab report columns, so the key has to name the result. `field_id` is in the key so two genuinely separate samples that return the same value — common for non-detects sharing an LOR — are not merged; it is dropped from the key via `intersect()` for exports that lack the column.
- **`dedupe_check` ticked (mode `"max"`)** — `select_max_concentration()` on the *raw* upload, which takes the highest of the primary, duplicate and triplicate results for each location/date/analyte (detects preferred over non-detects). The Normal filter is deliberately not applied here.

The Normal filter is skipped entirely (mode `"unfiltered"`) when no row is labelled `"Normal"`, so an export using a different sample-type vocabulary is not silently emptied.

**`prefix` is `NA` for detects, not `"="`**: `data_processor()` derives `prefix` from `detect_flag` for exports that have no qualifier column, so non-detects are `"<"` and detects are `NA`. Every non-detect test in the app must be written `!is.na(prefix) & prefix == "<"` (and its complement `is.na(prefix) | prefix != "<"`), never a bare `prefix == "<"`.

**`date` is POSIXct, not Date**: `data_processor()` returns `floor_date(sampled_date_time, "day")`. Charts use `scale_x_datetime()` with `as.POSIXct(input$plotting_date)` limits; `dateRangeInput` still hands back `Date`, which compares against POSIXct without coercion.

**Shared plot reactives**: Each plot type (`ts_plot_obj`, `hist_plot_obj`, `boxplot_obj`) is a standalone reactive that returns a ggplot2 object. Both the screen renderer (`renderPlot`) and the PNG download handler (`downloadHandler`) call the same reactive, so download output always matches what is on screen.

**Criteria lines use a separate aesthetic**: Horizontal criteria lines on the timeseries and boxplot are mapped to `linetype` (timeseries) or `colour` (boxplot) aesthetics — completely separate from the location `colour`/`fill` scales — to avoid scale conflicts. Do not collapse these into a single scale.

**`establish_plotting_variables()` uses `<<-`**: This `gRs` function assigns `location_colours` (a named colour vector keyed by `location_code`) to the global environment. It is called at the top of `ts_plot_obj` and `boxplot_obj` before the colour scale switch. If location colours look wrong, this is the first thing to check.

**MK table has two views**: The `mk_table_switch` checkbox toggles between a pivoted trend-summary view (wide, one column per analyte) and the raw statistics view (long, includes p-value, tau, S, mean, SD, COV). The `data` list-column is always dropped before display/export.

**Colour theme switch**: The `ts_colour_theme` input drives both `scale_colour_*` (timeseries) and `scale_fill_*` (boxplot) via a `switch()` block in each reactive. Adding a new palette requires adding a case to both switch blocks.

### gRs Package Functions

Refer to the raw gRs package scripts located at the below link when creating new features and logic.
C:\Users\Enrightj\OneDrive - AECOM\Documents\My EQuIS Work\gRs
Or on the github if not accessible
https://github.com/jenright-git/gRs

| Function | Returns |
|---|---|
| `data_processor(path, report_type = "auto", result_type = "primary")` | tibble arranged by `date`, with a `report_type` attribute. Always: date (POSIXct), sampled_date_time, site_id, location_code, chem_name, concentration, output_unit, prefix, detect_flag, fraction. Often: monitoring_zone, chem_code, sample_type, result_type, matrix_code. **Not guaranteed**: `chem_group` |
| `select_max_concentration(data)` | one row per location/date/analyte — detects preferred over non-detects, then highest concentration. Drops `Field_D` / `Interlab_D` duplicates and repeat analyses. Row order not guaranteed |
| `half_lor(data, multiplier = 1)` | concentrations multiplied by `multiplier` where `prefix == "<"`. **Default is 1 (no change)** — pass `0.5` for half-LOR. Not used by the app; the LOR multiplier is passed straight to `mann_kendall_test()` instead |
| `mann_kendall_test(data, lor_multiplier = 1, nd_threshold = NULL, min_detects = NULL)` | tibble with trend, p_value, tau_statistic, S_statistic, sample_mean, SD, COV + nested `data` list-column |
| `mann_kendall_heatmap(data, ...)` | ggplot2 object (not currently used by the app) |
| `establish_plotting_variables(data)` | side-effect: sets `location_colours`, `analytes`, `zones`, `locations_vec`, `chem_group`, `date_range` in global env. **Requires a `chem_group` column** |
| `timeseries_plot(data, ...)` | ggplot2 object (not currently used by the app) |
| `action_level_processor(path)` / `join_action_levels(...)` | guideline exports — not currently used by the app |

Mann-Kendall trend levels (in severity order): `"Increasing"`, `"Probably Increasing"`, `"Stable"`, `"No Significant Trend"`, `"Probably Decreasing"`, `"Decreasing"`. The colour map for these is defined once at the top of `server()` as `mk_trend_colors`.

### Theme

- `bslib::bs_theme()` with AECOM greens: bg=`#ffffff`, fg=`#00353E`, primary=`#00353E`, secondary=`#008745`
- `thematic::thematic_on()` called at startup to auto-propagate theme colours into ggplot2
- MK table header colour: `#008768` (stored as `mk_header_col` in server, also hard-coded in CSS)
- File upload limit: 30 MB `.xlsx` only

### Supported Upload Formats

`data_processor()` identifies the sheet by its column names, not by sheet name, so all of these upload without any extra arguments:

- ESDAT `Chemistry List` (`ChemistryList.xlsx` in the repo root is a working example)
- ESDAT `LChem1_Chemistry` / `SChem1_Chemistry` and their `dav`-prefixed variants
- EQuIS `Analytical Results II`

Gauging reports (`Water Levels II`, ESDAT gauging) and action level exports also read, but carry no chemistry — `processed_file()` rejects them with a message rather than letting the app fail. When adding a format, check it against the four things the app relies on and the export may not provide: `chem_group`, `sample_type` (with a `"Normal"` value), `field_id`, and a `prefix` of `"<"`/`NA`.




