draft_annotation <- function(include = TRUE){
  
  require(ggpmisc)
  
  if(include == TRUE){
    
    draft_annotation <- geom_text_npc(aes(npcx = 0.5, npcy = 0.5, label = "DRAFT 2023 GCRMN PACIFIC REPORT"),
                                      size = 6, angle = 45, color = "#ecf0f1", family = "Open Sans Semibold")
    
  }else{
    
    draft_annotation <- geom_text_npc(aes(npcx = 0.5, npcy = 0.5, label = ""))    
  }
  
}