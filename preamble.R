# Source all the things!
for (p in c("function", "class", "lib")) {
    path = paste(".", p, sep = "//")
    files = paste(path, list.files(path = path, pattern = "*.R"), sep = "//")
    lapply(files, source)
}
