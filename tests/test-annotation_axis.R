library(ComplexHeatmap)


gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
    side = "left", facing = "outside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "left", facing = "outside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
    side = "left", facing = "inside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "left", facing = "inside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
    side = "right", facing = "outside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "right", facing = "outside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
    side = "right", facing = "inside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "right", facing = "inside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:3, labels = month.name[1:3], labels_rot = 0, 
    side = "top", facing = "outside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "top", facing = "outside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:3, labels = month.name[1:3], labels_rot = 90, 
    side = "top", facing = "outside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "top", facing = "outside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:3, labels = month.name[1:3], labels_rot = 45, 
    side = "top", facing = "outside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "top", facing = "outside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:3, labels = month.name[1:3], labels_rot = 0, 
    side = "top", facing = "inside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "top", facing = "inside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:3, labels = month.name[1:3], labels_rot = 0, 
    side = "bottom", facing = "outside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "bottom", facing = "outside"')
grid.draw(gb)
popViewport()

gb = annotation_axis_grob(at = 1:3, labels = month.name[1:3], labels_rot = 0, 
    side = "bottom", facing = "inside")
grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
grid.rect()
grid.text('side = "bottom", facing = "inside"')
grid.draw(gb)
popViewport()

grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
gb = annotation_axis_grob(labels_rot = 0, side = "left", facing = "outside")
grid.rect()
grid.text('side = "left", facing = "outside"')
grid.draw(gb)
popViewport()

grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
gb = annotation_axis_grob(side = "left", direction = "reverse")
grid.rect()
grid.text('side = "left", direction = "reverse')
grid.draw(gb)
popViewport()

grid.newpage()
pushViewport(viewport(xscale = c(0, 4), yscale = c(0, 6), width = 0.6, height = 0.6))
gb = annotation_axis_grob(side = "bottom", direction = "reverse")
grid.rect()
grid.text('side = "bottom", direction = "reverse"')
grid.draw(gb)
popViewport()


