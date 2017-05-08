
MakeAccessibleSVG = function(x, file = "test", view=interactive(), ...) {
            UseMethod("MakeAccessibleSVG")
          }

MakeAccessibleSVG.default =
    function(x, file = "test", view=interactive(), ...) {
      svgfile = SVGThis(x, paste0(file, ".svg"))
      xml = AddXML(x)
      XML::saveXML(doc=xml, file=paste0(file, ".xml"))
      if (view) {
          BrowseSVG(file=file, view=view, ...)
      }
      message("SVG and XML files created successfully")
      return(invisible(NULL))
}

MakeAccessibleSVG.histogram =
    function(x, file = "test", view=interactive(), ...) {
      svgfile = SVGThis(x, paste0(file, ".svg"))
      xml = AddXML(x)
      XML::saveXML(doc=xml, file=paste0(file, ".xml"))
      if (view) {
          BrowseSVG(file=file, view=view, ...)
      }
      message("SVG and XML files created successfully")
      return(invisible(NULL))
}

MakeAccessibleSVG.tsplot =
    function(x, file = "test", view=interactive(), ...) {
      svgfile = SVGThis(x, paste0(file, ".svg"))
      if (x$Continuous) {
        .RewriteSVG.tsplot(x, paste0(file, ".svg"))
      }
      xml = AddXML(x)
      XML::saveXML(doc=xml, file=paste0(file, ".xml"))
      if (view) {
          BrowseSVG(file=file, view=view, ...)
      }
      message("SVG and XML files created successfully")
      return(invisible(NULL))
}
