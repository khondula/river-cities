
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

# 040 = State
# 050 = County
# 061 = Minor Civil Division
# 071 = Minor Civil Division place part
# 157 = County place part
# 162 = Incorporated place
# 170 = Consolidated city
# 172 = Consolidated city -- place within consolidated city

citytown_long <- readr::read_csv("Subcounty_Intercensal_Pop_2000-2018.csv")

get_subcounty_data <- function(){
  
  states <- unique(citytown_long$STNAME)
  placenames <- unique(citytown_long$NAME)
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "Get data from"),
    miniUI::miniContentPanel(
    shiny::fillCol(flex = c(1, 4),
      fillRow(
      shiny::selectizeInput(inputId = "my_state",
                        label = "State:",
                        choices = states,
                        multiple = TRUE),
    shiny::uiOutput('selectorUI_placenames')),
    shiny::plotOutput('myplot')
  )))

server <- function(input, output, session){
  
  state_placenames <- reactive({
    ct_filter <- citytown_long %>%
      dplyr::filter(STNAME %in% input[['my_state']])
    unique(ct_filter[['NAME']])
           })
  
  output$selectorUI_placenames <- shiny::renderUI({
    req(input$my_state)
    shiny::selectizeInput(inputId = "my_placename",
                          label = "Name:",
                          choices = state_placenames(),
                          multiple = TRUE)
  })
  
 return_data <- reactive({
   req(input$my_placename)
   citytown_long %>%
     filter(STNAME %in% input[['my_state']]) %>%
     filter(NAME %in% input[['my_placename']]) %>%
     mutate(year = as.integer(year))
 })
 
 output$myplot <- shiny::renderPlot({
   req(return_data())
   
   return_data() %>%
     ggplot(aes(x = year, y = population, group = STATE)) +
     geom_point() + 
     geom_line() +
     expand_limits(y = 0) +
     facet_wrap(vars(NAME), scales = "free_y")
   
   })
  
  # return table
  shiny::observeEvent(input$done,{
    returnValue <- return_data()
    shiny::stopApp(returnValue)
  })
  
}

shiny::runGadget(ui, server)

}

my_data <- get_subcounty_data()
