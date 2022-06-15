\name{textbox_grob}
\alias{textbox_grob}
\title{
A simple grob for the word cloud
}
\description{
A simple grob for the word cloud
}
\usage{
textbox_grob(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "centre",
    gp = gpar(), background_gp = gpar(col = "black", fill = "transparent"), round_corners = FALSE, r = unit(0.1, "snpc"),
    line_space = unit(4, "pt"), text_space = unit(4, "pt"), max_width = unit(100, "mm"),
    padding = unit(4, "pt"), first_text_from = "top", add_new_line = FALSE, word_wrap = FALSE)
}
\arguments{

  \item{text}{A vector of texts. The value can be single words or phrases/sentenses.}
  \item{x}{X position.}
  \item{y}{Y position.}
  \item{just}{Justification of the box in the viewport.}
  \item{gp}{Graphics parameters of texts.}
  \item{background_gp}{Graphics parameters for the box.}
  \item{round_corners}{Whether to draw round corners for the box.}
  \item{r}{Radius of the round corners.}
  \item{line_space}{Space between lines. The value can be a \code{\link[grid]{unit}} object or a numeric scalar which is measured in mm.}
  \item{text_space}{Space between texts The value can be a \code{\link[grid]{unit}} object or a numeric scalar which is measured in mm.}
  \item{max_width}{The maximal width of the viewport to put the word cloud. The value can be a \code{\link[grid]{unit}} object or a numeric scalar which is measured in mm. Note this might be larger than the final width of the returned grob object.}
  \item{padding}{Padding of the box, i.e. space between text and the four box borders. The value should be a \code{\link[grid]{unit}} object with length 1, 2 or 4. If  length of the input unit is 2, the first value is the padding both to the top and to the bottom, and the second value is the padding to the left and right. If length of the input unit is 4, the four values correspond to paddings to the bottom, left, top and right of the box.}
  \item{first_text_from}{Should the texts be added from the top of the box or from the bottom? Value should be either "top" or "bottom".}
  \item{add_new_line}{Whether to add new line after every text? If \code{TRUE}, each text will be in a separated line.}
  \item{word_wrap}{Whether to apply word wrap for phrases/sentenses.}

}
\value{
A \code{\link[grid:grid.grob]{grob}} object. The width and height of the grob can be get by \code{\link[grid]{grobWidth}} and \code{\link[grid:grobWidth]{grobHeight}}.
}
\examples{
words = sapply(1:30, function(x) strrep(sample(letters, 1), sample(3:10, 1)))
grid.newpage()
grid.textbox(words, gp = gpar(fontsize = runif(30, min = 5, max = 30)))

sentenses = c("This is sentense 1", "This is a long long long long long long long sentense.")
grid.newpage()
grid.textbox(sentenses)
grid.textbox(sentenses, word_wrap = TRUE)
grid.textbox(sentenses, word_wrap = TRUE, add_new_line = TRUE)
}
