localCRAN <- path.expand("/var/www/lran")
dir.create(localCRAN)
contribDir <- file.path(localCRAN, "src", "contrib")
dir.create(contribDir, recursive = TRUE)
rVersion <- paste(unlist(getRversion())[1:2], collapse = ".")

binPaths <- list(
                 win.binary = file.path("bin/windows/contrib", rVersion),
                 mac.binary = file.path("bin/macosx/contrib", rVersion),
                 mac.binary.mavericks = file.path("bin/macosx/mavericks/contrib", rVersion),
                 mac.binary.leopard = file.path("bin/macosx/leopard/contrib", rVersion)
                 )

binPaths <- lapply(binPaths, function(x) file.path(localCRAN, x))
lapply(binPaths, function(path) {
       dir.create(path, recursive = TRUE)
                 })

tar_ball <- "fvarug_0.0-3.tar.gz"
file.copy(tar_ball, file.path(contribDir, tar_ball))

tools::write_PACKAGES(contribDir, type = "source")
lapply(binPaths, function(path) {
       tools::write_PACKAGES(path)
                 })

oldRepos <- getOption("repos")
if (FALSE) {
    if (TRUE) {
        message("we need a web servers' config for that") 
        cranURI <- "http://10.33.50.43/lran/"
    } else {

        cranURI <- paste("file://", normalizePath(localCRAN, winslash = "/"), sep = "")
    }
    options(repos = c( fvafrdebianCU = cranURI))
    install.packages("fvarug", type = "source")
} else {
    message("we need a web servers' config for that") 
    install.packages("fvarug", repos = "http://10.33.50.43/lran/", type = "source")
}
