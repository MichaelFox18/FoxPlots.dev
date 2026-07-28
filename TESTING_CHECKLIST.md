# foxplots — manual testing checklist

The automated suite (1,600+ checks) covers the helpers, the module contracts and
the launcher wiring. It cannot tell you whether the app is *pleasant to use*, or
whether a real spreadsheet from the internet lands cleanly. This checklist is
for that.

**How to use it.** Work one app at a time. Each item says what to do and **what
good looks like**, so a failure is unambiguous. You do not need to do all of it
every time — the **Smoke** items are the 10-minute pass before a release; the
rest is the deep pass before something like a CRAN submission.

Run the apps from source so you are testing the code in the repo, not an older
installed copy:

```r
pkgload::load_all(".")
run_data_explorer()   # or run_map_tool(), run_glmm_review(), ...
```

---

## 0. Cross-cutting — do these once per app that has an Import tab

### File formats

| Do | What good looks like |
|---|---|
| Upload `.csv` | Table appears, row/column counts match the file |
| Upload `.tsv` and `.txt` | Same; if a `.txt` is comma-separated, set the separator and it re-reads |
| Upload `.xlsx` with **2+ sheets** | A sheet picker appears; switching sheets re-reads the data |
| Upload `.xls` (old Excel) | Reads, or fails with a readable message — never a blank screen |
| Upload `.rds` | Reads, and list-columns survive (unlike CSV) |
| Upload a file with a **wrong extension** (e.g. a CSV named `.xlsx`) | A readable "Read error: ..." notification, app still usable |

### Messy real-world files (the ones Kaggle actually gives you)

| Do | What good looks like |
|---|---|
| A CSV with **2 title rows above the header** and a footnote row below | Auto-detected; a notice says "Auto-skipped N title line(s) ... N footnote line(s)" |
| A **completely empty** file | Clean message, no crash |
| A **one-column** file | Imports; Data Health does not mangle it |
| **Duplicate column names** (`x`, `x`, `x`) | Data Health flags it; the fix makes them `x`, `x_1`, `x_2` |
| **Blank column names** | Flagged; become `V1`, `V2`… |
| A column that is **entirely blank** | Flagged as an empty column and dropped (default on) |
| A row of **empty fields** (`,,`) in the middle | Read as an all-missing row, flagged as an empty row, removed by the default-on fix. (A *truly blank line* is skipped by the CSV reader itself and never becomes a row) |
| Numbers stored as text with `$`, `,`, `%` | "Numbers stored as text" is flagged and the fix converts them |
| A **ZIP code / phone** column with leading zeros (`02134`) | Stays **text** — must NOT become the number 2134 |
| `NA`, `N/A`, `NULL`, `#N/A` cells | Flagged as placeholder text, converted to real missing values |
| A column of `-` or `.` values | **Not** converted — these are real category values, deliberately excluded |
| An **ISO date** column (`2024-03-01`) | The "dates" fix is offered but **off by default**; ticking it converts |
| A US-style date (`03/01/2024`) | Deliberately NOT auto-converted (m/d vs d/m is unguessable) — convert by hand in Change Variable Types |

### Data Health, filters, and undo

There are **nine** issue types; two are opt-in. Check each fires and each fix works:
`names`, `trim`, `na_tokens`, `numeric`, **`dates` (off by default)**, `empty_cols`,
`empty_rows`, `dups`, **`outliers` (off by default)**.

| Do | What good looks like |
|---|---|
| Tick **outliers** and apply | Adds an `is_outlier` column (3×IQR); **deletes nothing**. Re-applying does not stack a second column |
| Apply fixes, then **Revert to original** | Data returns exactly to as-loaded |
| **Change Variable Types**: a factor → Number | Converts via the *labels*, never the hidden integer codes; unparseable values become NA and the count is reported |
| Add a **row filter**, then remove it | Downstream tabs (Summarize/Visualize/Map) follow the filtered rows, and restore when cleared |
| Press **Clear data** | Everything empties, the file input visibly resets, and you can upload again |

### Export & Report (every app has this tab now)

| Do | What good looks like |
|---|---|
| Download data as CSV / Excel | Opens cleanly; column names match the app |
| Download the charts | The image matches what is on screen |
| Generate an **HTML report** | Opens in a browser with no missing images; wide tables scroll inside their box rather than overflowing |
| Generate a **Word report** | Opens in Word and is editable (needs the optional `officer` package) |
| **Untick a section**, regenerate | That section is genuinely absent, not just blank |

---

## 1. Data Explorer — `run_data_explorer()`

**Smoke:** Import → *Florida sites (map)* → Reshape → Summarize → Visualize →
Map → Compare Groups → Regression → Export & Report, ending with one HTML report.

| Do | What good looks like |
|---|---|
| **Visualize:** try all 11 chart types | Each draws or explains what it still needs |
| Colour a density plot by a **high-cardinality column** (e.g. `site`, 120 values) | Draws **instantly**, uncoloured, with an orange note naming the column and the limit (50). *This was a 112-second freeze before 0.10.0* |
| Colour by a 5-level column | Colours and legend appear normally |
| Colour a scatter by a **continuous numeric** | A smooth gradient — continuous colours are deliberately NOT capped |
| Set 4 plots at once | All four render; the sidebar accordion keeps them separate |
| A bar chart of a column with **> 30 categories** | Only the largest 30 shown, rest as "Other", and a note says so |
| A pie chart with **> 12 slices** | Same, capped at 12 |
| **Facet** by a column with > 30 values | Faceting is dropped and explained |
| Copy the **R code** for a plot into a fresh R session | Reproduces the chart you saw — including any cap that was applied |
| **Save session** on Import, reload the app, **Restore** | Working data, filters and reshape settings all come back (analysis-tab choices are *not* saved — known limit) |

## 2. Map Tool — `run_map_tool()`

**Smoke:** Import → *Florida research sites* → Map → points draw → download HTML.

### Points
| Do | What good looks like |
|---|---|
| Load *Fiji earthquakes* (1,000 points) | Coordinates auto-detected; clustering turns on by itself above 500 points, and the footer says "clustering on (auto)" |
| **Swap lat / lon** | Points move to the mirrored position — proves the button works |
| Colour by a column, try **log** and **quantile** scales | Legend changes sensibly; falls back to linear with a note if the data can't support it |
| **Size by** a numeric column | Bubbles scale by *area*, and a graduated size legend appears |
| **Group layers by** a column with **> 12 levels** | Grouping is ignored with an explanation (12 is the cap) |
| **Density heatmap** | A smooth surface appears (needs optional `leaflet.extras`; without it you get a note, not a crash) |
| **Combine points by area** + **heatmap together** | *Both* draw — bubbles per area over a heat surface built from the **raw** points |
| Then untick **Show point markers** | Heatmap alone, no markers, no stray legends |
| Untick markers **and** turn the heatmap off | A warning says the map is only the basemap |
| Scroll with the cursor **over the settings** | The settings scroll; **the map does not move** |
| **Download HTML**, open it with no internet | Opens standalone; basemap tiles are the only thing missing |
| **Download PNG** | Framed as on screen (needs `webshot2` + `chromote` + Chrome/Edge) |
| Copy the **R code** into a fresh session | Reproduces the map, including the combine + heatmap case |

### Shaded regions (choropleth) — no coordinates needed
| Do | What good looks like |
|---|---|
| Import a table of **state names + a number**, no lat/lon | The Map tab shows a blue note pointing you to Shaded regions |
| Follow it: US states → `state` → your number | The map shades; the sidebar reports "N of 52 regions matched your data" |
| Deliberately use abbreviations (`FL`) against the `state` property | Reports 0 matched and **names the strays on both sides** — never a silent blank map |
| Open **Region name property** | Every option shows an example (`county_state — e.g. Brooks County, Georgia`) |
| US counties → **Limit to state = Florida** | Renders fast (a second at most), and your property / key / value picks **survive the change** |
| Switch to Shaded regions **before** choosing columns | The pane stays empty with "pick the region property…" — it must **not** draw a points map |
| Upload your own GeoJSON | Same flow; properties are read from the file |

## 3. Compare Groups — `run_compare_groups()`

**Smoke:** Import → *iris* → compare `Sepal.Length` across `Species` → read the result.

| Do | What good looks like |
|---|---|
| One outcome × one group, parametric | t-test/ANOVA with assumptions, effect size, and connecting letters (with SEs) |
| Switch to **non-parametric** | Wilcoxon/Kruskal with Dunn or Steel-Dwass post-hoc |
| **6 outcomes × 4 groups** (the maximum) | 24 combinations run, BH-corrected summary plus a per-combination accordion |
| Try to exceed 24 | A clear message, not a hang |
| **Split by** a third variable | One stratum per level (max 6), BH applied across the whole family |
| Set split-by back to **(none)** | It actually returns to none — *this was broken before 0.8.0* |
| Chi-square on two categorical columns | Table with the percentage options working |

## 4. Regression Tool — `run_regression_tool()`

| Do | What good looks like |
|---|---|
| Fit a linear model with a categorical predictor | Reference level is stated; coefficients have CIs |
| Add an **interaction** and a **polynomial** term | Both appear in the formula and the coefficient table |
| Check **diagnostics** | Q-Q, scale-location, Cook's distance all render; the assumption panel reads in plain English |
| Fit a **logistic** model on a 0/1 outcome | Odds ratios appear |
| **Model comparison:** fit, Save as A, change the setup, fit again | The status line walks you through it and the comparison appears |
| Press compare **without changing anything** | It tells you B is still the same fit as A |
| **Estimated means** tab | Controls are fully visible, not squeezed |

## 5. Mixed Model Review — `run_lmer_tool()`

| Do | What good looks like |
|---|---|
| Load the **RCBD example**, fit `yield_kg` with Variety × Nitrogen and Block random | ANOVA, fit stats, variance components |
| Check the **variance table** | Variances and SDs are positive; correlation rows are labelled separately — never a "negative variance" |
| **EMMeans + letters** | Plot and connecting letters agree |
| **Model comparison** | Refits with ML and says so |
| Generate the **report** | Fit statistics appear as ~10 labelled rows, not 2 garbled ones (*this was broken before 0.9.0*) |

## 6. GLMM Review — `run_glmm_review()`

| Do | What good looks like |
|---|---|
| **General tab:** `insect_count` with nbinom2 | Fits; Wald ANOVA appears (needs optional `car`, otherwise an instructive note) |
| Pick a family whose domain the data violates (e.g. beta on counts) | A live warning **before** you fit |
| `seedling_count` with a **zero-inflation** formula | Fits and reports the zi part |
| `cover_prop` with **beta** | Fits |
| **DHARMa** residual panel | Axis labels are readable numbers, not `0.0943396226415094` |
| **Binary tab:** `present` with logit/probit/cloglog | Fits; EMMeans come back on the response scale |
| Report with **both** tabs fitted | Both models appear side by side |

## 7. Reshape Tool — `run_reshape_tool()`

| Do | What good looks like |
|---|---|
| **Stack** several columns (`relig_income`) | Long form with the label/value columns you named |
| **Split** back to wide (`fish_encounters`) | Round-trips |
| Split with **duplicate keys** | Produces list-columns; CSV export flattens them rather than erroring |
| **Transpose**, **Sort**, **Subset** | Each does what its name says and the preview updates |

## 8. Combine Tool — `run_combine_tool()`

| Do | What good looks like |
|---|---|
| Load `band_members` + `band_instruments`, **join** on `name` | Left/inner/full/anti all behave as labelled |
| **Concatenate** the two mtcars halves | 32 rows; the optional source column marks which table each row came from |
| **Update** and **Compare** | Compare names the differing columns/rows |
| Join on a column present in only one table | Clear message, not a crash |
| Note there are **two** Data Health panels (one per table) | Both work independently |

---

## Sharp edges worth probing on purpose

These are deliberate limits. Each should **explain itself**, never fail silently:

| Limit | Value | Where |
|---|---|---|
| Colour / group levels | **50** | Visualize — past this, colouring is dropped |
| Bars before "Other" | 30 | Bar charts (slider-adjustable) |
| Pie slices before "Other" | 12 | Pie charts |
| Facet panels | 30 | Visualize |
| Compare Groups combinations | 24 | 6 outcomes × 4 groups |
| Split-by strata | 6 | Compare Groups |
| Mixed-model fixed effects | 3 | lmer / GLMM tools |
| Map layer groups | 12 | Map — group-by |
| "Combine by area" areas | 100 | Map |
| Auto-clustering threshold | 500 points | Map |
| Choropleth features drawn | 4,000 | Map |

**Optional packages** — each should degrade to a note, never an error:
`hexbin` (hexbin chart), `leaflet.extras` (heatmap), `car` (GLMM Wald ANOVA),
`officer` (Word report), `webshot2`/`chromote` + Chrome (map PNG),
`pbkrtest`/`performance`/`MuMIn`/`influence.ME` (mixed-model extras).

**Known gap, not yet fixed:** there is **no row-count cap or warning on import**.
An 11k-row file is comfortable; a multi-million-row file will be slow throughout.

---

## Where to get test data

**Built-in, and byte-identical every run** (so they work as golden values):

| Generator | Shape | Good for |
|---|---|---|
| `make_example_data()` | 108 rows, RCBD: Block/Variety/Nitrogen/Irrigation + 5 response types | Mixed models, Compare Groups, Regression |
| `make_map_example_data()` | 120 rows, 12 Florida counties, `lat`/`lon`/`yield`/`acres`/`crop` | Map (points **and** choropleth via `county`) |
| `make_glmm_example_data()` | 144 rows: overdispersed counts, zero-heavy counts, a proportion, a 0/1 | GLMM Review |
| `datasets::quakes` | 1,000 points | Map clustering, heatmap, combine-by-area |

**From Kaggle (or anywhere) — what shape to look for:**

| App | Look for | Example |
|---|---|---|
| Reshape | A **wide** survey/year table (one column per year or per question) | Population by country by year |
| Combine | **Two** tables sharing a key column | Orders + customers |
| Map (points) | Any table with `latitude`/`longitude` | Earthquakes, airports, crime incidents |
| Map (shaded) | A table keyed by **state / county / country name**, no coordinates | Poverty or unemployment by state |
| Compare Groups | A numeric outcome plus 1–2 grouping columns | Test scores by school and gender |
| Regression | Several numeric predictors and one outcome | Housing prices |
| Mixed models | **Repeated measures** — the same subject/site/block measured more than once | Field trials, longitudinal health data |
| Data Health | Anything **messy** — that is the point. Government CSVs with title rows and footnotes are ideal | Census / agency exports |

When a Kaggle file breaks something, keep it. A file that broke the app once is
the best regression test there is.
