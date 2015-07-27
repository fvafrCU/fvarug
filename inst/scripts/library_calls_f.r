#!/usr/bin/Rscript --vanilla
# ROXYGEN_START
#' library_calls_f.r
#'
#' Summarize installed libraries and libraries actually used.
#'
#' This is the a modified version of library_calls.r giving the same output
#' using functions.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version $Id: df44ece5ae86fc31458bb3f1b93d030f8a7a3e76 $
#' @docType data
#' @name A Header for
NULL
# ROXYGEN_STOP
clean_names <- function(v, string) {
    in_invocation <- gsub(paste0(".*", string, "\\((.*)\\).*"), "\\1", v)
    cleaned_combine <- gsub("c\\(", "", gsub("\\)", "",  in_invocation))
    cleaned_quotes <- gsub("\"", "",  gsub("'", "", cleaned_combine))
    cleaned_csv <- unlist(strsplit(cleaned_quotes, ","))
    cleaned_options <- cleaned_csv[grep("=", cleaned_csv, invert = TRUE)]
    cleaned_leading_blanks <- sub(" *", "", cleaned_options)
    return(cleaned_leading_blanks)
}

# somehow, dir.exists() seems gone, this is a workaround.
exists_directory <- function(path) {
    if (existsFunction("dir.exists")) {
        status <- dir.exists(path)
    } else {
        status <- FALSE
        if (file.access(path) == 0 && getElement(file.info(path), "isdir"))
            status <- TRUE
    }
    return(status)
}

get_path <- function(path) {
    # reset path for known hosts
    path <- switch(Sys.info()["nodename"],
               "f060" = ,
               "h5" = "/home/nik/git_cyclops",
               "fvafrdebianCU" = "/home/nik/git/cs",
               "FVAFR-PC82053" = "p:/git/deploy/hochrechnungen",
               path
               )
    # prompt user
    while (is.null(path)) {
        if (is.null(path)) 
            path <- readline("Please give a path (for example c:/temp): ")
        if (! exists_directory(path)) {
            warning(paste0("Can't find ", path, "!"), immediate. = TRUE)
            path <- NULL
        }
    }
    return(path)
}

get_package_calls <- function(files) {
    ##% grep library() and require() calls form candidate files
    libraries  <- requirements <- NULL
    for (file_name in files) {
        opened_file  <- file(file_name)
        libraries <- c(libraries, grep("library(", fixed = TRUE, 
                                       readLines(con = opened_file), 
                                       value = TRUE))
        requirements <- c(requirements, grep("require(", fixed = TRUE, 
                                             readLines(con = opened_file),
                                             value = TRUE))
        close(opened_file)
    }

    packages_loaded <- factor(c(clean_names(requirements, "require"), 
                          clean_names(libraries, "library")))
    return(packages_loaded)
}

# ROXYGEN_START
#' List packages 
#' 
#' List packages installed on your system and called from source files under a
#' given path.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9bbb752b06d887f2115e37c3e9dadd89e40c49c7 $
#' @param path The path to look for source files. If NULL, the user is queried
#' to give a path.
#' @param pattern A pattern defining source files.
#' @return a named list containing
#' \itemize{
#' \item "path", a character vector giving the path, 
#' \item "pattern", a character vector giving  the pattern, 
#' \item "packages_installed", a character vector giving the packages installed,
#' \item "package_calls", a factor giving the package calls.   
#' }
# @examples
# get_package_list() 
get_package_list <- function(path = NULL, 
                            pattern = "*\\.[rR]$") {
    path <- get_path(path)
    value <- list(path = path, pattern = pattern)
    class(value) <- c("package_list", class(value))
    value[["packages_installed"]] <- row.names(installed.packages())
    ##% find candidate files
    candidate_files <- list.files(path = path, pattern = pattern, 
                                  full.names = TRUE, recursive = TRUE)
    if (length(candidate_files) > 0) {
        value[["package_calls"]] <- get_package_calls(candidate_files)
    } else {
        value[["package_calls"]] <- NULL
        warning("found no candidate R code files under ", path, ".")
    }
    return(value)
}

#' Summary function for objects of class "package_list"
#' 
#' Summarize the findings of \code{\link{get_package_list}}.
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9bbb752b06d887f2115e37c3e9dadd89e40c49c7 $
#' @param object an object of class object.
#' @param ... argument passing for generic \code{\link{summary}} compablility.
#' @return invisibly NULL
# @examples
# summary(get_object())
summary.object <- function(object, ...) {
    hostname <- getElement(Sys.info(), "nodename")
    message("Packages installed on ", hostname, ":")
    print(object[["packages_installed"]])
    message("Packages loaded by library() or require() in '",
            object[["pattern"]], "'-files under ", 
            object[["path"]], ":")
    print(packages_loaded  <- levels(object[["package_calls"]]))
    message("Packages loaded by library() or require()  in '",
            object[["pattern"]], "'-files under ", 
            object[["path"]], " but not installed ",
            "on ", hostname, ":")
    print(packages_not_installed <- setdiff(packages_loaded,
                                      object[["packages_installed"]]))
    message("Packages installed on ", hostname, 
            " but never loaded by library() or require() in '",
            object[["pattern"]], "'-files under ", 
            object[["path"]], ":")
    print(packages_never_loaded <- setdiff(object[["packages_installed"]],
                                     packages_loaded))
    message("Frequencies of packages loaded by library() or require() in '",
            object[["pattern"]], "'-files under ", 
            object[["path"]], ":")
    print(summary(object[["package_calls"]]))
    return(invisible(NULL))
}

#' Plot function for objects of class "package_list"
#' 
#' Plot the frequencies of packages called. 
#' These frequencies are given by the
#' \code{\link{get_package_list}}()[["package_calls"]].
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version: $Id: 9bbb752b06d887f2115e37c3e9dadd89e40c49c7 $
#' @param x an object of class package_list.
#' @param ... argument passing for generic \code{\link{plot}} compablility.
#' @return invisibly NULL
# @examples
# plot(get_package_list())
plot.package_list <- function(x, ...) {
    op <- par(mar = c(10,4,4,2) + 0.1)
    plot(x[["package_calls"]],
         col=rainbow(length(levels(x[["package_calls"]]))),
         las=2,
         main = paste0("'", x[["pattern"]], "'-files under ", 
            x[["path"]], " on ", 
            getElement(Sys.info(), "nodename")),
         ylab = "Frequencies of packages loaded by library() or require()")
    par(op)
}
# ROXYGEN_STOP

#% body
summary(get_package_list())
plot(get_package_list())
