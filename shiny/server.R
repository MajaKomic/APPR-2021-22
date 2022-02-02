library(shiny)

shinyServer(function(input, output) {
  
  output$graf.shiny <- renderPlot({
    narisigraf(input$leto)
  })
})

preurejena.4 <- tabela4 %>%
  dplyr::select(2, 4, 7) %>%
  left_join(pretvori.leta2, by = "solsko.leto") %>%
  dplyr::select(-solsko.leto) %>%
  group_by(vrsta.izobrazevanja, leto) %>%
  summarise(delez.diplomantov = ceiling(sum(delez)))

tabela.shiny <- tabela3 %>%
  dplyr::select(1, 5, 8) %>%
  filter(solsko.leto != "2020/21", solsko.leto != "2009/10", solsko.leto != "2010/11") %>%
  left_join(pretvori.leta2, by = "solsko.leto") %>%
  dplyr::select(-solsko.leto) %>%
  group_by(vrsta.izobrazevanja, leto) %>%
  summarise(delez.vpisanih = sum(delez)) %>%
  left_join(preurejena.4, by = c("vrsta.izobrazevanja" = "vrsta.izobrazevanja", 
                                 "leto" = "leto"))

narisigraf <- function(leto){
  grafshiny <- ggplot(tabela.shiny %>% filter(leto == leto)) +
    aes(
      x = vrsta.izobrazevanja, 
      y = delez.vpisanih, 
      size = delez.diplomantov) + 
    geom_point(color = "lightblue") + 
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 60, vjust = 0.5),
      axis.title.x = element_text(vjust =  0),
      plot.title = element_text(hjust = 0.5),
      legend.position="none"
    ) +
    geom_label_repel(
      aes(label = delez.diplomantov),
      box.padding   = 0.35, 
      point.padding = 0.5) +
    labs(
      x = "Vrsta izobraževanja",
      y = "Število vpisanih v terciarno izobraževanje na 1000 prebivalcev",
      title = "ŠTEVILO VPISANIH V TERCIARNO IZOBRAŽEVANJE\nIN ŠTEVILO DIPLOMANTOV NA 1000 PREBIVALCEV"
    )
  
  print(grafshiny)
  
}

