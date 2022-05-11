# download the data from OSF
library(here)
library(fs)

fs::dir_create(here("inst", "extdata"))
fs::dir_create(here("figures"))


# Download the QC data
download.file("https://osf.io/c5pfj/download", here("inst", "extdata","motion_benchmark.rds"))
download.file("https://osf.io/9jz76/download", here("inst", "extdata", "qc_benchmark.rds"))
