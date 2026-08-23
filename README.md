# Time-Series

Forecasting coursework worked twice — **once in R and once in Python** — across
the full sequence from decomposition to ARIMA, with published forecasting
papers alongside for context.

Course sequence BAN 673. The duplication is the point: the same model, the same
data, two ecosystems, so the method stays separable from the syntax.

## The sequence

### 1 · Seeing the series
| file | topic |
|---|---|
| `1_1_components_autocorrelation` | Level, trend, seasonality; autocorrelation |
| `1_2_plots` | The plots you look at before fitting anything |
| `1_3_partition_naive_accuracy` | Train/validation partitioning, the naive benchmark, accuracy measures |

The naive forecast comes third for a reason — it is the number every later
model has to beat, and a model that does not beat it has not earned its
complexity.

### 2 · Smoothing
`2_1_MA_centered_trailing` — centred vs trailing moving averages, and why only
one of them can forecast.
`2_2_MA_de-trend_de-season_WMA` · `2_3_SES_original_optimal` (simple
exponential smoothing, hand-set vs optimised) · `2_4_HW_trend_seasonality`
(Holt-Winters).

### 3 · Regression-based forecasting
`3_1_regression_trend_season` · `3_2_regression_external_vars` — trend and
seasonal dummies, then bringing in outside predictors.

### 4 · Autoregressive models
`4_1_ACF_AR_models` · `4_2_predictability_AR_models` (is this series even
predictable?) · `4_3_ARIMA_models` · `4_2_prophet_model` ·
`4_4_best_models_comp_amtrak2019` — the comparison that closes the sequence.

## Case studies

`BAN673CS1.R` · `RN7945TSCS2.R` — applied case work.

## Data

Amtrak ridership (`amtrak_comp.csv`, `Amtrak_naive_comp_upd.xlsx`,
`Amtrak_regression_comp.xlsx`) is the spine of the sequence.
S&P 500 prices across several windows (`S&P500prices_23/24/20_24_5years.csv`),
plus `Charges-1`, `Cust_Part1-1`, `ToyotaCorolla`, `utilities`, `train`, and a
cleaned Bitcointalk forum corpus with its cleaning report.

## Reading

Papers kept alongside the code, on where these methods are actually used:

- `01_Radovilsky_Jawahar_M7_revenue_forecast_IJRM_2025.pdf` — revenue forecasting
- `01_health_supply_chain_forecasting.pdf` · `02_supply_chain_demand_frecasting.pdf`
- `00_time_series_seasonal_items_sales_ML.pdf` — seasonal retail demand
- `time_series_analytics_process_summary_2025.pdf`

## Running

```r
install.packages(c("forecast", "zoo", "prophet"))
```
```bash
pip install pandas numpy statsmodels matplotlib prophet jupyter
```

R scripts run standalone; the `.ipynb` files are the Python equivalents.

## Libraries & Methods

Scanned every `.R`/`.ipynb` file (43 files, 13,576 lines) — the R and Python
implementations use different libraries for the same methods, which is the
whole design of this repo.

**R side** — the `forecast` package supplies `auto.arima` (77 calls),
`Arima` (40), `ets` (49), `tslm` (96), `stl` — plus base `lm`/`glm` (96
calls). `dmba::regressionSummary` scores the fits, `zoo` handles the
irregular time index, `TTR` supplies additional smoothing functions.

**Python side** — `statsmodels.tsa` covers the same ground:
`SARIMAX`, `ExponentialSmoothing`, `Holt`, `SimpleExpSmoothing`,
`seasonal_decompose`, plus `tsaplots`/`stattools`/`acf` for diagnostics.
`prophet.Prophet` and `pmdarima` (auto-ARIMA for Python) appear alongside
the hand-fit statsmodels models — three approaches to the same forecasting
problem, not one.

**Clustering** (`03-Clustering.ipynb`) — `sklearn.cluster
.AgglomerativeClustering`, `scipy.cluster.hierarchy.dendrogram`, evaluated
with `silhouette_score`.

**Other regression/ML** — `sklearn.pipeline.Pipeline`,
`PolynomialFeatures`, `KMeans`, `StandardScaler`, `OneHotEncoder`,
`train_test_split`, `KFold`.

**Visualization** — matplotlib, seaborn, `plotly` for the interactive
charts, `mpl_toolkits.mplot3d.Axes3D` for the 3D cluster plots.
