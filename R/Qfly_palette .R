# Manually define colour palette to be used in this project for each population


scale_fill_Qfly_2 <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#A6CEE3", "#2A7FB7", "#99CD91", "#52AF43", 
                        "#B89B74", "#ED4F50", "#F06C45", "#FDA440", 
                        "#ED8F47", "#B294C7", "#825D99", "#F8F18F"), 
                      c("Alice Springs", "Batemans Bay", "Bega Valley", "Brisbane", 
                        "Canberra", "Cape Tribulation", "Darwin", "Griffith",
                        "Mareeba", "Narrabri", "Sydney", "Utchee Creek")), 
    ...
  )
}


scale_fill_Qfly <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#e6194B", "#3cb44b", "#9A6324", "#4363d8", 
                        "#f58231", "#911eb4", "#808000", "#800000",
                        "#000075", "#a9a9a9", "#469990", "#e6beff"), 
                      c("Alice Springs", "Batemans Bay", "Bega Valley", "Brisbane", 
                        "Canberra", "Cape Tribulation", "Darwin", "Griffith",
                        "Mareeba", "Narrabri", "Sydney", "Utchee Creek")), 
    ...
  )
}


scale_colour_Qfly_2 <- function(...){
  ggplot2:::manual_scale(
    'colour', 
    values = setNames(c("#A6CEE3", "#2A7FB7", "#99CD91", "#52AF43", 
                        "#B89B74", "#ED4F50", "#F06C45", "#FDA440", 
                        "#ED8F47", "#B294C7", "#825D99", "#F8F18F"), 
                      c("Alice Springs", "Batemans Bay", "Bega Valley", "Brisbane", 
                        "Canberra", "Cape Tribulation", "Darwin", "Griffith",
                        "Mareeba", "Narrabri", "Sydney", "Utchee Creek")), 
    ...
  )
}

scale_colour_Qfly <- function(...){
  ggplot2:::manual_scale(
    'colour', 
    values = setNames(c("#e6194B", "#3cb44b", "#9A6324", "#4363d8", 
                        "#f58231", "#911eb4", "#808000", "#800000",
                        "#000075", "#a9a9a9", "#469990", "#e6beff"), 
                      c("Alice Springs", "Batemans Bay", "Bega Valley", "Brisbane", 
                        "Canberra", "Cape Tribulation", "Darwin", "Griffith",
                        "Mareeba", "Narrabri", "Sydney", "Utchee Creek")), 
    ...
  )
}