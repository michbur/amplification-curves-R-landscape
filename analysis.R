library(deepdep)
library(pbapply)
library(dplyr)
library(reshape2)

package_names <- readLines("./data/pcr-packages.txt")

# package_deps <- pblapply(package_names, function(ith_package) {
#   res <- try(deepdep(ith_package, bioc = TRUE, depth = 5), silent = TRUE)
#   
#   res
# })

package_deps <- readRDS("./results.RDS")

plot_dependencies(package_deps[[3]])

package_mtr <- lapply(package_deps[!(sapply(package_deps, inherits, what = "try-error"))], function(ith_list) {
  (package_names %in% unique(ith_list[["name"]])) %>% 
    setNames(package_names)
}) %>% 
  do.call(rbind, .)

rownames(package_mtr) <- paste0("origin_", package_names[!(sapply(package_deps, inherits, what = "try-error"))])
package_mtr

data.frame(package_name = package_names, not_on_cran = sapply(package_deps, inherits, what = "try-error"),
           bioc = sapply(package_deps, function(ith_package) {
             if(!inherits(ith_package, what = "try-error")) {
               any("Biobase" == unique(ith_package[["origin"]])) 
             } else {
               FALSE
             }
           })) %>% 
  mutate(location = ifelse(not_on_cran, "elsewhere", ifelse(bioc, "Bioconductor", "CRAN"))) %>% 
  select(package_name, location)



