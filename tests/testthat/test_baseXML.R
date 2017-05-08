library(graphics)
library(gridSVG)
library(XML)

expect_xml_equal <- function(a, b) {
  doc <- XML::xmlParse(b)
  comp <- XML::compareXMLDocs(a, doc)
  expect_equal(length(comp$inA), 0)
  expect_equal(length(comp$inB), 0)
}

expect_doc_equal <- function(a, b) {
  comp <- XML::compareXMLDocs(a, b)
  expect_equal(length(comp$inA), 0)
  expect_equal(length(comp$inB), 0)
  ## print(XML::toString.XMLNode(a))
  ## print(XML::toString.XMLNode(b))
  expect_equal(XML::toString.XMLNode(a),
               XML::toString.XMLNode(b))
}

test_that('document', {
  doc <- .AddXMLDocument('root')
  expect_xml_equal(doc,
                   '<root xmlns:sre="http://www.chemaccess.org/sre-schema"/>')
  root <- xmlRoot(doc)
  child <- .AddXMLAddNode(root, 'child')
  expect_xml_equal(doc,
                   '<root xmlns:sre="http://www.chemaccess.org/sre-schema"><child/></root>')
  expect_equal(.AddXMLmakeId('box', '1.1.1'), 'graphics-plot-1-box-1.1.1')
})


## test_that('simple', {
##   doc <- XML::newXMLDoc()
##   root <- XML::newXMLNode('root', doc=doc)
##   XML::ensureNamespace(root, c(sre = "http://www.chemaccess.org/sre-schema"))
##   title <- .AddXMLAddTitle(root, title='title', longTitle='long title')
##   expect_xml_equal(doc, '<root xmlns:sre="http://www.chemaccess.org/sre-schema"><title sre:speech="title" sre:speech2="Title: long title"/></root>')
## })

test_that('boxplot', {
  xml <- XML::xmlParseDoc('../resources/boxplot.xml')
  chart <- AddXML(boxplot(Ozone~Month, data=airquality, main='Boxplots for Ozone',xlab='Months',ylab='Ozone'))
  expect_doc_equal(xml, chart)
})

test_that('boxplot_horizontal', {
  xml <- XML::xmlParseDoc('../resources/boxplot_horiz.xml')
  chart <- AddXML(boxplot(Ozone~Month, data=airquality, main='Boxplots for Ozone',ylab='Months',xlab='Ozone',horizontal=TRUE))
  expect_doc_equal(xml, chart)
})

test_that('timeseries_disc', {
  xml <- XML::xmlParseDoc('../resources/timeseries_disc.xml')
  chart <- AddXML(TimeSeriesPlot(airquality$Ozone, main="Timeseries of Ozone", ylab="Ozone"))
  expect_doc_equal(xml, chart)
})

test_that('timeseries_cont', {
  xml <- XML::xmlParseDoc('../resources/timeseries_cont.xml')
  chart <- AddXML(TimeSeriesPlot(airquality$Temp, main="Timeseries of Temperature", ylab="Temperature"))
  expect_doc_equal(xml, chart)
})

test_that('histogram', {
  xml <- XML::xmlParseDoc('../resources/histogram.xml')
  chart <- AddXML(hist(airquality$Ozone, main="Histogram of Ozone", xlab="Ozone"))
  expect_doc_equal(xml, chart)
})

dev.off()
