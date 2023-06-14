hj_dag <- function(x,
                   y,
                   names,
                   arcs = cbind(0,0),
                   add_points = FALSE,
                   solids = rep(1, length(x)),
                   title = "",
                   contraction = .1,
                   add_functions = 0,
                   add_functions_text = NULL,
                   text_shift = .2*add_points,
                   padding = .5,
                   length = 0.2,
                   cex = 1,
                   box = TRUE) {
  if(add_points)  plot(x, y, pch=ifelse(solids == 1, 19, 1), cex = 2, axes = FALSE, xlab = "", ylab = "",
                       xlim = c(min(x)-padding, max(x)+padding),
                       ylim = c(min(y)-padding-add_functions, max(y)+padding),
                       main = title)
  if(!add_points)  plot(x, y, type = "n", cex = 2, axes = FALSE, xlab = "", ylab = "",
                        xlim = c(min(x)-padding, max(x)+padding),
                        ylim = c(min(y)-padding-add_functions, max(y)+padding),
                        main = title)

  arrows(x[arcs[,1]]*(1-contraction) + x[arcs[,2]]*contraction,
         y[arcs[,1]]*(1-contraction) + y[arcs[,2]]*contraction,
         x[arcs[,2]]*(1-contraction) + x[arcs[,1]]*contraction,
         y[arcs[,2]]*(1-contraction) + y[arcs[,1]]*contraction, length = length)
  text(x, y + text_shift, names, cex = cex)
  if(!is.null(add_functions_text)) text(((min(x)+max(x))/2), min(y)-1, add_functions_text)
  if(box) box()
}

