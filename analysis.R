library(deepdep)
library(pbapply)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
library(grid)
library(gridExtra)

get_legend <- function(gg_plot) {
  grob_table <- ggplotGrob(gg_plot)
  grob_table[["grobs"]][[which(sapply(grob_table[["grobs"]], function(x) x[["name"]]) == "guide-box")]]
}

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
edge_df <- package_mtr %>% 
  melt %>% 
  filter(value) %>% 
  select(from = Var2, to = Var1) %>% 
  mutate(to = gsub("origin_", "", x = to, fixed = TRUE))
  

node_df <- data.frame(package_name = package_names, not_on_cran = sapply(package_deps, inherits, what = "try-error"),
           bioc = sapply(package_deps, function(ith_package) {
             if(!inherits(ith_package, what = "try-error")) {
               any("Biobase" == unique(ith_package[["origin"]])) 
             } else {
               FALSE
             }
           })) %>% 
  mutate(location = ifelse(not_on_cran, "elsewhere", ifelse(bioc, "Bioconductor", "CRAN"))) %>% 
  select(package_name, location)

pkg_graph <- graph_from_data_frame(edge_df, directed = TRUE, vertices = node_df)
plot(pkg_graph)

pkg_colors <- c("chartreuse3", "cornflowerblue", "orange")
color_legend <- get_legend(ggplot(node_df, aes(x = 1, y = 1, fill = location)) +
  geom_tile() +
  scale_fill_manual("", values = pkg_colors) +
  theme_bw() +
  theme(legend.position = "bottom"))

graph_plot <- ggraph(pkg_graph, layout = "kk") + 
  geom_edge_link() + 
  geom_node_label(aes(label = name, color = location), size = 3.2) +
  scale_color_manual("", values = pkg_colors, guide = FALSE) +
  theme_void()

pdf("package_network.pdf", width = 9, height = 8)
grid.arrange(graph_plot,
             color_legend,
             heights = c(0.9, 0.1))
dev.off()
