# look out for two versions of GetWriteR() second one is while the exe file is broken.
# look out for two versions of GetWxPython27(), the second is experimental and uses a Python shell command command to pull wxPython

GetWriteR =
    function(UseGitHub = TRUE) {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (UseGitHub) {
              download.file(
                  "https://raw.github.com/ajrgodfrey/WriteR/master/Python/WriteR.zip",
                  "WriteR.zip")
              unzip("WriteR.zip")
              file.remove("WriteR.zip")
              file.rename("WriteR.exe",
                          paste0(getOption("BrailleR.Folder"), "WriteR.exe"))
              message(
                  "The WriteR application has been added to your MyBrailleR folder.")
            } else {
              browseURL("https://R-Resources.massey.ac.nz/WriteR/WriteR.zip")
              message(
                  "The WriteR application has been downloaded but you need to unzip it.")
              message("Move it to your MyBrailleR folder and unzip before use.")
            }
            message(
                "It is assumed you wanted to download this file by issuing the last command.")
            message(
                "You can delete WriteR.exe at any time to remove WriteR from your system.")
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }


GetPandoc =
    function() {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.\n")
              installr::install.pandoc(
                download_dir = getOption("BrailleR.Folder"),
                keep_install_file = TRUE)
              message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }



GetRStudio =
    function() {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.\n")
              installr::install.RStudio(
                download_dir = getOption("BrailleR.Folder"),
                keep_install_file = TRUE)
              message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }



Get7zip =
    function() {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.")
              installr::install.7zip(download_dir = getOption("BrailleR.Folder"),
                                   keep_install_file = TRUE)
              message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }

GetPython27 =
    function() {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.")
            #installr::
.install.python(version_number = 2, download_dir=getOption("BrailleR.Folder"), keep_install_file = TRUE)
            message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }

GetPython27 =
    function() {
      .GetPython(2)
      return(invisible(NULL))
    }

GetPython3 =
    function() {
      .GetPython(3)
      return(invisible(NULL))
    }


.GetPython =
    function(version) {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.")
#            installr::
.install.python(version_number = version, download_dir=getOption("BrailleR.Folder"), keep_install_file = TRUE)
            message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }

GetWxPython27 =
    function() {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.")
              installr::install.URL(
                "http://downloads.sourceforge.net/wxpython/wxPython3.0-win32-3.0.2.0-py27.exe",
                download_dir = getOption("BrailleR.Folder"),
                keep_install_file = TRUE)
              message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }

GetWriteR = function(UseGitHub = TRUE) {
              message("This command is temporarily unavailable.")
            }


.PullWxUsingPip = function(){
    if(reticulate::py_module_available("wx")){
        system("pip install -U wxPython")
        }
    else{
        system("pip install wxPython")
        }
        return(invisible(TRUE))
    }


GetWxPython27 =
    function() {
      Success = FALSE
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if(reticulate::py_available(TRUE)){
            if(reticulate::py_config()$version == "2.7"){
              Success = .PullWxUsingPip()
            } else {
              warning("There is no installation of Python 2.7.\n")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(Success))
    }


GetCygwin =
    function(x64=TRUE) {
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if (requireNamespace("BrailleR")) {
            if (requireNamespace("installr")) {
              message(
                "This command will download a file and save it to your hard drive.\n")
bit=32
if(x64){
              if(installr::is.x64()) bit=64
              }
      installr::install.cygwin(bit=bit,
                download_dir = getOption("BrailleR.Folder"),
                keep_install_file = TRUE)
              message(
                "The installer file has been added to your MyBrailleR folder.")
              message(
                "You can delete it at any time, but that will not uninstall the application.")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(NULL))
    }

GetWxPython3 =
    function() {
      Success = FALSE
      if (interactive()) {
        if (.Platform$OS.type == "windows") {
          if(reticulate::py_available(TRUE)){
            if(reticulate::py_config()$version > 3){
              Success = .PullWxUsingPip()
            } else {
              warning("There is no installation of Python 3.\n")
            }
          }
        } else {
          warning(
              "This function is for users running R under the Windows operating system.\n")
        }
      } else {
        warning("This function is meant for use in interactive mode only.\n")
      }
      return(invisible(Success))
    }
