vp = function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, heights = unit(c(0.5, 5, 5, 5), "null"))))   
plot1 = ggplot(points, aes(GE, IBM)) + geom_point(size = 1) + scatterTheme
print(plot1, vp = vp(1,1))
plot2 = ggplot(points, aes(GE, KO)) + geom_point(size = 1) + scatterTheme
print(plot2, vp = vp(1,2))