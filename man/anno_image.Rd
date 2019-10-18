\name{anno_image}
\alias{anno_image}
\title{
Image Annotation
}
\description{
Image Annotation
}
\usage{
anno_image(image, which = c("column", "row"), border = TRUE,
    gp = gpar(fill = NA, col = NA), space = unit(1, "mm"),
    width = NULL, height = NULL)
}
\arguments{

  \item{image}{A vector of file paths of images. The format of the image is inferred from the suffix name of the image file. NA values or empty strings in the vector means no image to drawn.}
  \item{which}{Whether it is a column annotation or a row annotation?}
  \item{border}{Wether draw borders of the annotation region?}
  \item{gp}{Graphic parameters for annotation grids. If the image has transparent background, the \code{fill} parameter  can be used to control the background color in the annotation grids.}
  \item{space}{The space around the image to the annotation grid borders. The value should be a \code{\link[grid]{unit}} object.}
  \item{width}{Width of the annotation. The value should be an absolute unit. Width is not allowed to be set for column annotation.}
  \item{height}{Height of the annotation. The value should be an absolute unit. Height is not allowed to be set for row annotation.}

}
\details{
This function supports image formats in \code{png}, \code{svg}, \code{pdf}, \code{eps}, \code{jpeg/jpg}, \code{tiff}. 
\code{png}, \code{jpeg/jpg} and \code{tiff} images are imported by \code{\link[png]{readPNG}}, \code{\link[jpeg]{readJPEG}} and 
\code{\link[tiff]{readTIFF}}, and drawn by \code{\link[grid]{grid.raster}}. \code{svg} images are firstly reformatted by \code{rsvg::rsvg_svg}
and then imported by \code{\link[grImport2]{readPicture}} and drawn by \code{\link[grImport2]{grid.picture}}. \code{pdf} and \code{eps}
images are imported by \code{\link[grImport]{PostScriptTrace}} and \code{\link[grImport]{readPicture}}, later drawn by \code{\link[grImport]{grid.picture}}.

Different image formats can be mixed in the \code{image} vector.
}
\value{
An annotation function which can be used in \code{\link{HeatmapAnnotation}}.
}
\seealso{
\url{https://jokergoo.github.io/ComplexHeatmap-reference/book/heatmap-annotations.html#image-annotation}
}
\examples{
# download the free icons from https://github.com/Keyamoon/IcoMoon-Free
\dontrun{
image = sample(dir("~/Downloads/IcoMoon-Free-master/PNG/64px", full.names = TRUE), 10)
anno = anno_image(image)
draw(anno, test = "png")
image[1:5] = ""
anno = anno_image(image)
draw(anno, test = "some of png")
}
}
