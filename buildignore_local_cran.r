#!/usr/bin/Rscript --vanilla
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
this_directory <- basename(getwd())
tar_balls <- list.files(pattern = paste0(this_directory, ".*\\.tar\\.gz"))
current_tar_ball <- tar_balls[length(tar_balls)]
destination <- file.path(contribDir, current_tar_ball)
file.copy(current_tar_ball, destination ,overwrite = TRUE)
system(paste("chmod 644", destination))

tools::write_PACKAGES(contribDir, type = "source")
lapply(binPaths, function(path) {
       tools::write_PACKAGES(path)
                 })

if (FALSE) {
    message("we need a web servers' config for that") 
    install.packages("fvarug", repos = "http://10.33.50.43/lran/", type = "source")
}
