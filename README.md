# ggfun

`ggfun` provides a collection of extensions and utilities for working with
[`ggplot2`](https://ggplot2.tidyverse.org/) objects. It includes additional
geometric layers, theme elements, legend helpers, plot-data utilities, and tools
for modifying plot objects.

## Installation

Install the released version from CRAN:

```r
install.packages("ggfun")
```

You can install the development version from GitHub with:

```r
## install.packages("remotes")
remotes::install_github("YuLab-SMU/ggfun")
```

## Features

- Additional geoms, including `geom_xspline()`, `geom_hist()`,
  `geom_cake()`, `geom_triangle()`, `geom_segment_c()`, and `geom_volpoint()`.
- Theme helpers such as `theme_noaxis()`, `theme_noxaxis()`,
  `theme_noyaxis()`, `theme_transparent()`, `theme_nothing()`, and
  `theme_blinds()`.
- Custom theme elements, including `element_roundrect()` and
  `element_blinds()`.
- Legend and font utilities such as `get_legend()`, `gglegend()`,
  `set_font()`, and `set_point_legend_shape()`.
- Plot manipulation helpers, including `facet_set()`, `get_plot_data()`,
  `get_aes_var()`, `xrange()`, `yrange()`, and `ggrange()`.
- Utilities for tree-data style workflows, including `%<+%`, `td_filter()`,
  `td_mutate()`, and `td_unnest()`.

## Links

- GitHub: <https://github.com/YuLab-SMU/ggfun>
- Bug reports: <https://github.com/YuLab-SMU/ggfun/issues>
