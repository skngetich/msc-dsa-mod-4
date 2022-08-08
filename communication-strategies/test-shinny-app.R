# Load packages
library(renv)
library(shiny)
library(vroom)
library(tidyverse)
library(devtools)
library(packcircles)
library(ggplot2)
library(viridis)

# install.packages("devtools")
#devtools::install_github("hadley/neiss")

#setwd("communication-strategies/")

#dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
#download <- function(name) {
 # url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  #download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
#}
#download("injuries.tsv.gz")
#download("population.tsv")
#download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")

#Load Accident dataset
#library(neiss)

#injuries = vroom::vroom(neiss::injuries)
#products = neiss::products
#population = neiss::population


products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")
prod_codes <- setNames(products$prod_code, products$title)


ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", choices = prod_codes)
    )
  ),
  fluidRow(
    column(6, plotOutput("diagnosis")),
    column(6, plotOutput("body_part"))
  )
,
  fluidRow(
    column(6,  plotOutput("location")),
    column(6, plotOutput("age_sex"))

  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))

  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )

  output$diagnosis <- renderPlot({
    selected() %>% count(diag, wt = weight, sort = TRUE) %>%
      ggplot(aes(x=diag, y=n)) +
      geom_bar(stat='identity') +
      labs(x = "ER Diagnosis") +
      coord_flip()
  }, res = 96)

  output$body_part <- renderPlot({
    selected() %>% count(body_part, wt = weight, sort = TRUE) %>%
      ggplot(aes(x=body_part, y=n)) +
      geom_bar(stat='identity') +
      labs(y = "Count") +
      coord_flip()
  }, res = 96)




  output$location <- renderPlot({
    data = selected() %>% count(location, wt = weight, sort = TRUE)

    # Generate the layout
    packing <- circleProgressiveLayout(data$n, sizetype='area')
    packing$radius <- 0.85*packing$radius
    data <- cbind(data, packing)
    dat.gg <- circleLayoutVertices(packing, npoints=50)

    # Plot
    ggplot() +
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
      scale_fill_viridis() +
      geom_text(data = data, aes(x, y, size=n, label = location), color="black") +
      theme_void() +
      theme(legend.position="none")+
      coord_equal()+
      ggtitle("Where did the injuries happen ?")
  },res=100,height = 500)

  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}



shinyApp(ui, server)




