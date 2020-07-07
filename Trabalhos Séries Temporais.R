# Bibliotecas

library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
#library(highcharter)
library(dygraphs)
library(tidyverse)
library(shinyWidgets)
library(highcharter)
library(xts)

library(ggplot2)
library(ggpmisc)
library(lubridate)
library(zoo)
library(fpp2)
library(randtests)
library(dygraphs)

# Dados
link = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
#link = "Global_Mobility_Report.csv"

dados = readr::read_csv(link) %>% 
  filter(country_region_code == "BR") %>% 
  select(-country_region, -sub_region_2, -iso_3166_2_code,-census_fips_code )  %>% 
  mutate(Estado = sub_region_1 %>% str_replace_all("State of ", "")) %>% 
  filter(Estado == "Goiás") #%>% 
  #select(date, workplaces_percent_change_from_baseline) %>% rename("Workplaces" = "workplaces_percent_change_from_baseline")

names(dados)[4:9] = c("retail_recreation", "grocey_pharmacy", "parks", 
                      "transit_station", "workplaces", "residential")

dadoxts = xts(dados, order.by = dados$date) 
dadosWo = dados %>% select(date, workplaces)
dadosWo_Xts = xts(dadosWo$workplaces ,order.by =dadosWo$date)

# Dado das Quarentena

dadosQua = dadosWo %>% filter(date >= as.Date("2020-03-15")) 
dadosQua_xts = xts(dadosQua$workplaces, order.by = dadosQua$date)

# Sidebar

Sidebar = argonDashSidebar(
    vertical = TRUE,
  skin = "light",
  background = "white",
  size = "md",
  side = "left",
  id = "my_sidebar",
  brand_url = "https://www.ufg.br/",
  brand_logo = "https://files.cercomp.ufg.br/weby/up/1/o/Marca_UFG_cor_completa_horizontal.png",
  argonSidebarHeader(title = "Trabalhos"),
   argonSidebarMenu(
    argonSidebarItem(
      tabName = "tendsaz", icon = argonIcon(name = "planet", color = "info"), "Tend/Saz"
    )

    )
)

# navBar

Navbar = argonDashNavbar(
    argonDropNav(
    title = "Séries Temporais", 
    src = "https://marcas-logos.net/wp-content/uploads/2020/03/GITHUB-LOGO.png", 
    orientation = "right",
    argonDropNavTitle(title = "Bem-vindo(a)"),
    argonDropNavItem(
      title = "Código aqui!", 
      src = "https://www.google.com", 
      icon = argonIcon("folder-17")
    )
))

# Header

Header = argonDashHeader(
    gradient = TRUE,
  color = "primary",
  separator = TRUE,
  separator_color = "secondary"
)

# Footer
 
Footer = argonDashFooter(
  copyrights = "Érika S. e Danilo E., 2020"
)

# Body

Body = argonDashBody(
  argonTabItems(
    argonTabItem(
      tabName = "tendsaz"
      
      , argonRow(
        argonColumn(width = 6, 
        argonImage(
        src = "https://exame.com/wp-content/uploads/2020/03/coronavc3adrus-9.jpg",
        url = "https://www.google.com/covid19/mobility/", floating =  T)
        
      )
      ,argonCard(width = 6,
                 title = "Dados utilizados",
                 #src = "http://www.google.com",
                 hover_lift = TRUE,
                 shadow = TRUE,
                 shadow_size = NULL,
                 hover_shadow = FALSE,
                 border_level = 0,
                 icon = argonIcon("atom"),
                 status = "primary",
                 background_color = NULL,
                 gradient = FALSE,
                 floating = FALSE,
                 "Este conjunto de dado mostra como as visitas e o tempo de permanência em locais 
                 diferentes mudam em comparação com um valor base( mediana do dia da semana correspondente, 
                 durante o período de cinco semanas de 3 de janeiro a 6 de fevereiro de 2020).
                 Para nosso trabalho consideramos apenas os dados referente as mudanças em relação
                 as visitas e tempo de permanência nos locais de trabalhos no estado de Goiás."       )
      )
      ,argonRow(
          argonCard( title = "Primeira olhada", width = 6,
                     plotlyOutput(outputId = "graficoInicial") )
          ,argonCard(title = "Regressão Linear", width = 6,
                     plotlyOutput(outputId = 'regLinear'))
          )
      ,argonH1("Suavização", display = 4)
      ,argonRow(
        argonCard(title = "Médias Móveis", width = 6, sliderTextInput(
          inputId = "ordem",
          label = "Número de termos:", 
          choices = 1:30,
          selected = 3
        ),
        plotlyOutput("mediasmoveis") )
      ,argonCard(title = "Regressão Não Paramétrica 'lowess'", width = 6 
                 ,sliderTextInput(
                   inputId = "span",
                   label = "Proporção de pontos:", 
                   choices = (1:10)*0.1,
                   selected = 0.3
                 )
                 ,plotlyOutput("rlowess")) )
      ,argonH1("Tendência", display = 4)
      ,argonRow(
        argonCard(title = "Testes de tendência", width = 4,
                  argonBadge("Teste Wald - Wolfowitz", status = "success")
                  ,verbatimTextOutput("walwol"),
                  argonBadge("Teste Cox - Stuart", status = "success"),
                  verbatimTextOutput("coxstu")),
        argonCard(title = "Série livre de Tendência", width = 8,
                  plotlyOutput("livreten"))
      )
    
    ) #argonTabItem
  ) #argonTabItems
) #argonDashBody

# UI

UI = argonDashPage(
  title = "Séries Temporais",
  author = "Érika e Danilo",
  description = "Trabalho Tendêndia e Sazonalidade",
  sidebar = Sidebar,
  navbar = Navbar, 
  header = Header,
  body = Body,
  footer = Footer
)

# Server

SERVER = function(input, output){ 
  
#### Gráfico inicial
  
  dadosWo = dados %>% select(date, workplaces)
  output$graficoInicial <- renderPlotly({
    
    dadosWo %>% 
      plot_ly(x = ~date, y = ~workplaces, type = "scatter", mode = "lines") %>% 
      config(displayModeBar = F)
    
  })
  
  
  dadosQua = dados %>% filter(date >= as.Date("2020-03-15"))
  #dadosQua_xts = xts(dadosQua, order.by = dadosQua$date)
  

#### Regressão linear
  
  modelo = lm(workplaces ~ date, data = dadosQua)
 
  
  dadosQuaP = dadosQua %>% mutate(Predita = predict(modelo))
  #dadosQuaP_xts = xts(dadosQuaP$Predita, order.by = dadosQuaP$date)
  
  output$regLinear <- renderPlotly({

    dadosQuaP %>% 
      plot_ly(x = ~date, y = ~workplaces, type = "scatter", mode = "lines", name = "Dado original") %>% 
      add_lines( y = ~Predita, x ~date , name = "Regressão Linear") %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = F)
      
    
    })
  
#### médias móveis
  
  dadosQuaMM = reactive({dadosQua %>% 
    mutate(MediaMovel = ma(workplaces, order = as.numeric(input$ordem), centre=TRUE) )})
 
  
  output$mediasmoveis <- renderPlotly({
    
    dadosQuaMM() %>% 
      plot_ly(x = ~date, y = ~workplaces, type = "scatter", mode = "lines", name = "Dado original") %>% 
      add_lines( y = ~MediaMovel, x ~date , name = "Média Móvel") %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = F)
    
  }) 
  
### Lowees
  

  output$rlowess <- renderPlotly({

    dadosQua %>% 
      mutate(Lowess = lowess(dadosQua$date,dadosQua$workplaces,  f= as.numeric(input$span) )$y) %>% 
      plot_ly(x = ~date, y = ~workplaces, type = "scatter", mode = "lines", name = "Dado original") %>% 
      add_lines( y = ~Lowess, x ~date , name = "Regressão Lowess") %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = F)
    
  })
  
####### Teste Tendência Wald - Wolfowitz
  output$walwol <- renderPrint({
    runs.test(dadosQua$workplaces)
  })
  
  
##### Teste Tendência Cox - Stuart
  output$coxstu <- renderPrint({
    cox.stuart.test(dadosQua$workplaces)
  })
  
##### Séries livre de tendência
  
  
  #dadosQuaSMM_xts = xts(dadosQuaSMM$Livre, order.by = dadosQuaSMM$date)
  
  output$livreten <- renderPlotly({
    dadosQuaMM() %>% 
      mutate(Livre = workplaces - MediaMovel)    %>% 
      plot_ly(x = ~date, y = ~workplaces, type = "scatter", mode = "lines", name = "Dado original") %>% 
      add_lines( y = ~Livre, x ~date , name = "Livre de Tendência") %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = F)
    
  })
  
  }

# App

{
  shinyApp(UI, SERVER)
}



