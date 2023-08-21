taxonomic_levels <- function(data){
  
  data %>% 
    mutate(tax_lvl = case_when(!(is.na(scientificName)) ~ "Species",
                             !(is.na(genus)) ~ "Genus",
                             !(is.na(family)) ~ "Family",
                             !(is.na(order)) ~ "Order",
                             !(is.na(class)) ~ "Class",
                             !(is.na(phylum)) ~ "Phylum",
                             !(is.na(subcategory)) ~ "Subcategory",
                             !(is.na(category)) ~ "Category")) %>% 
    group_by(tax_lvl) %>% 
    summarise(abs = n()) %>% 
    ungroup() %>% 
    mutate(rel = (abs*100)/sum(abs),
           tax_lvl = as_factor(tax_lvl),
           tax_lvl = fct_expand(tax_lvl, "Category", "Subcategory", "Phylum", "Class",
                                "Order", "Family", "Genus", "Species"),
           tax_lvl = fct_relevel(tax_lvl, "Species", "Genus", "Family", "Order", "Class",
                                 "Phylum", "Subcategory", "Category")) %>% 
    ggplot(data = ., aes(x = tax_lvl, y = rel)) +
    geom_bar(stat = "identity", fill = "#446cb3") +
    lims(y = c(0, 100)) +
    scale_x_discrete(drop = FALSE) +
    coord_flip() +
    labs(x = NULL, y = "Percentage of rows")
  
}