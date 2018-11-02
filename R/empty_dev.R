 
graphicsDevice = function(name, dim = c(1024, 768), col = "black", fill = "transparent", ps = 10, ..., 
	funcs = list(...), ipr = rep(1/72.27, 2)) {

	
    dim = as.integer(dim)
    
    col = as(col, "RGBInt")
    fill = as(fill, "RGBInt")

	dev = .Call("R_createGraphicsDevice",  funcs, as.character(name), dim, col, fill, as.integer(ps),
	                 list(funcs@initDevice, funcs@GEInitDevice), ipr)
	invisible(dev)
})

empty_dev = function() {
	funs = dummyDevice()
	graphicsDevice(name = "empty_dev", funcs = funs)
}


dummy_dev = 

> funs
An object of class "RDevDescMethods"
Slot "activate":
NULL

Slot "circle":
NULL

Slot "clip":
NULL

Slot "close":
NULL

Slot "deactivate":
NULL

Slot "locator":
NULL

Slot "line":
NULL

Slot "metricInfo":
function (char, gc, ascent, descent, width, dev) 
{
    width[1] = gc$ps * gc$cex
    ascent[1] = 1
    descent[1] = 0.25
}
<environment: 0x2351d48>

Slot "mode":
NULL

Slot "newPage":
NULL

Slot "polygon":
NULL

Slot "polyline":
NULL

Slot "rect":
NULL

Slot "size":
function (left, right, bottom, top, dev) 
{
    right[1] = dev$right
    bottom[1] = dev$bottom
}
<environment: 0x2266248>

Slot "strWidth":
function (str, gc, dev) 
{
    gc$cex * gc$ps * nchar(str)
}
<environment: 0x2351d48>

Slot "text":
NULL

Slot "onExit":
NULL

Slot "getEvent":
NULL

Slot "newFrameConfirm":
NULL

Slot "textUTF8":
NULL

Slot "strWidthUTF8":
NULL

Slot "initDevice":
NULL

Slot "GEInitDevice":
NULL

Slot "state":
NULL

