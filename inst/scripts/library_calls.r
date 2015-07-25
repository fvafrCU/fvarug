#!/usr/bin/Rscript --vanilla
#' library_calls.r
#'
#' summarize installed libraries and libraries actually used.
#'
#' This is the original version of a script I used, I have 
#' 
#' @author Dominik Cullmann, <dominik.cullmann@@forst.bwl.de>
#' @section Version $Id: df44ece5ae86fc31458bb3f1b93d030f8a7a3e76 $
#' @docType data
#' @name A Header for
NULL
# ROXYGEN_STOP
#% set user's specifications
path <- NULL
pattern = "*\\.[rR]$"

#% define local functions
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

clean_names <- function(v, string) {
    in_invocation <- gsub(paste0(".*", string, "\\((.*)\\).*"), "\\1", v)
    cleaned_combine <- gsub("c\\(", "", gsub("\\)", "",  in_invocation))
    cleaned_quotes <- gsub("\"", "",  gsub("'", "", cleaned_combine))
    cleaned_csv <- unlist(strsplit(cleaned_quotes, ","))
    cleaned_options <- cleaned_csv[grep("=", cleaned_csv, invert = TRUE)]
    cleaned_leading_blanks <- sub(" *", "", cleaned_options)
    return(cleaned_leading_blanks)
}

#% body
##% reset path for known hosts
path <- switch(Sys.info()["nodename"],
               "f060" = ,
               "fvafrdebianCU" = "/home/nik/git/cs",
               "h5" = "/home/nik/git_cyclops",
               "FVAFR-PC82053" = "p:/git/deploy/hochrechnungen",
               path
               )
##% prompt user
while (is.null(path)) {
    if (is.null(path)) 
        path <- readline("Please give a path (for example c:/temp): ")
    if (! exists_directory(path)) {
        warning(paste0("Can't find ", path, "!"), immediate. = TRUE)
        path <- NULL
    }
}

##% initialize resulting list
package_list <- list(path = path, pattern = pattern)
package_list[["packages_installed"]] <- row.names(installed.packages())

##% find candidate files
candidate_files <- list.files(path = path, pattern = pattern, 
                              full.names = TRUE, recursive = TRUE)



##% grep library() and require() calls form candidate files
if (length(candidate_files) > 0) {
    libraries  <- requirements <- NULL
    for (file_name in candidate_files) {
        opened_file  <- file(file_name)
        libraries <- c(libraries, grep("library(", fixed = TRUE, 
                                       readLines(con = opened_file), 
                                       value = TRUE))
        requirements <- c(requirements, grep("require(", fixed = TRUE, 
                                             readLines(con = opened_file),
                                             value = TRUE))
        close(opened_file)
    }

    package_list[["package_calls"]]<- factor(c(clean_names(requirements, "require"), 
                                               clean_names(libraries, "library")))
} else {
    package_list[["package_calls"]] <- NULL
    warning("found no candidate R code files under ", path, ".")
}

##% summarize findings
hostname <- getElement(Sys.info(), "nodename")
message("Packages installed on ", hostname, ":")
print(package_list[["packages_installed"]])
message("Packages loaded by library() or require() in '",
        package_list[["pattern"]], "'-files under ", 
        package_list[["path"]], ":")
print(packages_loaded  <- levels(package_list[["package_calls"]]))
message("Packages loaded by library() or require()  in '",
        package_list[["pattern"]], "'-files under ", 
        package_list[["path"]], " but not installed ",
        "on ", hostname, ":")
print(packages_not_installed <- setdiff(packages_loaded,
                                        package_list[["packages_installed"]]))
message("Packages installed on ", hostname, 
        " but never loaded by library() or require() in '",
        package_list[["pattern"]], "'-files under ", 
        package_list[["path"]], ":")
print(packages_never_loaded <- setdiff(package_list[["packages_installed"]],
                                       packages_loaded))
message("Frequencies of packages loaded by library() or require() in '",
        package_list[["pattern"]], "'-files under ", 
        package_list[["path"]], ":")
print(summary(package_list[["package_calls"]]))

##% plot package frequencies
op <- par(mar = c(10,4,4,2) + 0.1)
plot(package_list[["package_calls"]],
     col=rainbow(length(levels(package_list[["package_calls"]]))),
     las=2,
     main = paste0("'", package_list[["pattern"]], "'-files under ", 
                   package_list[["path"]], " on ", 
                   getElement(Sys.info(), "nodename")),
     ylab = "Frequencies of packages loaded by library() or require()")
par(op)
