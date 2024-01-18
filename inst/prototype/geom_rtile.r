
geom_rtile <- function(mapping = NULL, data = NULL, r = 0.1, ...) {
    layer(
        data = data,
        mapping = mapping,
        geom = GeomRtile,
        stat = "identity",
        position = "identity",
        params = rlang::list2(r=r, ...),
        check.aes = FALSE
    )
}


GeomRtile <- ggproto("GeomRtile", Geom,

                  draw_panel = function(data, panel_scales, coord, width=NULL, height=NULL, r=.1) {
                      data <- coord$transform(data, panel_scales) 
                      data$width <- data$width   %||% width  %||% resolution(data$x, FALSE)
                      data$height <- data$height  %||% height  %||% resolution(data$y, FALSE)  
                      data$r <- data$r %||% r
                      grobs <- lapply(1:nrow(data), function(i) {
                          vp <- viewport(x=data$x[i], y=data$y[i],
                                         width=data$width[i], height=data$height[i],
                                         just = c("center", "center"),
                                         default.units = "npc")
                          roundrectGrob(x = data$x[i], y = data$y[i], 
                                    r = unit(data$r[i], 'snpc'), 
                                    gp = gpar(col = data$colour[i],
                                            #fill_alpha(data$fill[i], data$alpha[i])
                                            fill = data$fill[i]
                                        ), 
                                    vp=vp, name=i)
                      })
                      class(grobs) <- "gList"
                      ggplot2:::ggname("geom_rtilee",
                                       gTree(children = grobs))
                  },
                  non_missing_aes = c("x", "y", "width", "height"),
                  default_aes = aes(colour =NA),
                  draw_key = draw_key_blank
                  )



ggplot(d, aes(row, col)) + geom_rtile(aes(fill=value)) + ggexpand(.2) + ggexpand(.2, -1)

