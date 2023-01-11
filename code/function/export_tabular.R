export_tabular <- function(territory_i){
  
  writeLines(c("\\begin{tabular}{>{\\bfseries}>{\\color{color1}}rl}",
               "Maritime area & 120,000 km\\textsuperscript{2} \\\\",
               "Land area & 12,000 km\\textsuperscript{2} \\\\",
               "Reef area & 500 km\\textsuperscript{2} \\\\",
               "\\end{tabular}"),
             paste0("figs/02_geographic-inf_", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}