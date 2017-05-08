library(XML)
## library(BrailleR)

expect_xml_equal <- function(a, b) {
  comp <- compareXMLDocs(a, b)
  expect_equal(length(comp$inA) == 0) && expect_equal(length(comp$inB) == 0)
}

test_that("document", {
  expect_equal(BrailleR:::.AddXMLDocument("test"),
               '<test xmlns:sre="http://www.chemaccess.org/sre-schema"></test>"')
})

