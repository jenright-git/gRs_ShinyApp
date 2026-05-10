# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the App

```r
# From R console (working directory: gRs_ShinyApp/)
shiny::runApp("gRs_ShinyApp/app.R")

# Or open app.R in RStudio and click "Run App"
```

## Dependency Management

This project uses `renv` (R 4.5.1) with a lockfile at `gRs_ShinyApp/renv.lock`. The `.Rprofile` at the repo root auto-activates renv on startup.

```r
renv::restore()       # restore packages from lockfile
renv::snapshot()      # update lockfile after adding packages
```

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
  └─► file_data()            reactive — data_processor() [+ optional half_lor()]
        ├─► mk_results()     eventReactive(mk_button) — mann_kendall_test()
        ├─► plotting_data()  eventReactive(update_plot_locations) — location filter only
        │     ├─► ts_plot_obj()    reactive — full ggplot2 timeseries (shared by render + download)
        │     ├─► hist_plot_obj()  reactive — ggplot2 histogram (shared by render + download)
        │     └─► boxplot_obj()    reactive — ggplot2 boxplot (shared by render + download)
        └─► facet_data()     eventReactive(update_facet_locations) — location + analyte filter
```

`plotting_data()` and `facet_data()` are behind task buttons, so they do not update until the user explicitly clicks "Update Locations" / "Update Plots". Analyte filtering for Charts happens *inside* each plot reactive, not at the `plotting_data()` level.

### Key Design Patterns

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
| `data_processor(path)` | tibble: date, location_code, chem_name, concentration, output_unit, monitoring_zone, chem_group, prefix |
| `half_lor(data)` | same tibble with concentrations halved where `prefix == "<"` |
| `mann_kendall_test(data)` | tibble with trend, p_value, tau_statistic, S_statistic, sample_mean, SD, COV + nested `data` list-column |
| `mann_kendall_heatmap(data, ...)` | ggplot2 object |
| `establish_plotting_variables(data)` | side-effect: sets `location_colours` in global env |
| `timeseries_plot(data, ...)` | ggplot2 object (used only in Facet Plot panel) |

Mann-Kendall trend levels (in severity order): `"Increasing"`, `"Probably Increasing"`, `"Stable"`, `"No Significant Trend"`, `"Probably Decreasing"`, `"Decreasing"`. The colour map for these is defined once at the top of `server()` as `mk_trend_colors`.

### Theme

- `bslib::bs_theme()` with AECOM greens: bg=`#ffffff`, fg=`#00353E`, primary=`#00353E`, secondary=`#008745`
- `thematic::thematic_on()` called at startup to auto-propagate theme colours into ggplot2
- MK table header colour: `#008768` (stored as `mk_header_col` in server, also hard-coded in CSS)
- File upload limit: 30 MB `.xlsx` only



