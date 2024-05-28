<!-- 
# TODO

+ mv facet utilities from `ggtree`

-->

# ggfun 0.1.5

+ implement the `%<+%` operator as a S3 method (2024-05-26, Sun)
    - mv the `%<+%` operator from 'ggtree' as the `%<+%.ggtree` method
    - implement `%<+%.ggsc` method for `ggsc` object.

# ggfun 0.1.4

+ deprecate `keybox()` as it is not compatible with ggplot2 v3.5.0 and we have better solution by using `element_roundrect()` (2024-01-18, Thu)
+ `theme_noaxis()` to remove both x and y axes (2023-12-22, Fri)

# ggfun 0.1.3

+ `set_point_legend_shape()` to override point shape legend (2023-09-15, Fri)
+ `get_plot_data()` to extract data from a 'gg' plot (2023-09-12, Tue)

# ggfun 0.1.2

+ add R version dependency in DESCRIPTION (2023-08-04, Fri)
+ `get_legend()` function to extract legend of a plot (2023-07-10, Mon)

# ggfun 0.1.1

+ mv `theme_no_margin()` from the 'aplot' package (2023-06-24, Sat)
+ mv `theme_fp()` from the 'ggbreak' package (2023-06-24, Sat)
+ be compatible with R 4.1 (2023-06-21, Wed, #10)

# ggfun 0.1.0

+ remove `theme_stamp()` and implement a better version `theme_blinds()` which internally use `element_blinds()` to draw the strip background (2023-06-20, Tue, #9)
+ `geom_cake()`, `geom_triangle()` and `geom_segment_c()` functions from 'GuangchuangYu/gglayer' (2023-02-10, Fri)
+ `volplot()` function to visualize volcano plot for DEGs (2022-11-29, Tue)
+ `geom_volpoint()` for volcano plot (2022-11-28, Mon)

# ggfun 0.0.9

+ `theme_noxaxis()` (2022-11-21, Mon)

# ggfun 0.0.8

+ compatible with ggplot2 v3.4.0 (2022-11-07, Mon)

# ggfun 0.0.7

+ add `theme_stamp` (2022-08-31, Wed, #6) (remove since v=0.1.0, use `theme_blinds` instead)

# ggfun 0.0.6

+ mv `identify.gg()` from 'ggtree' (2022-04-01, Fri)
+ mv `ggrange()`, `xrange()` and `yrange()` from 'aplot'

# ggfun 0.0.5

+ mv `theme_transparent()` and `theme_nothing()` from the ggimage package (2022-01-20, Thu)

# ggfun 0.0.4

+ mv `ggbreak2ggplot`, `is.ggbreak` and `is.ggtree` from the aplot package (2021-09-16, Thu)
+ `facet_set`: a better implementation of manually setting facet label, which combines `add_facet`, `ggtree::facet_labeller`  and more (2021-09-15, Wed)
+ `add_facet` to add facet label to a ggplot object (2021-09-03, Fri)

# ggfun 0.0.3

+ `element_roundrect` to add round rect background to ggplot legend. Now we can use `theme()` to enable this effect (2021-08-10)

# ggfun 0.0.2

+ mv `gglegend` and `set_font` functions from `yyplot` package (2021-06-30)
+ mv `get_aes_var` from `rvcheck` package

# ggfun 0.0.1

+ `keybox` to add round rect background to ggplot legend (2021-06-29)

