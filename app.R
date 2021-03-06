
# Load packages -----------------------------------------------------------
library("shiny")
library("babynames")
library("ggthemes")
library("scales")
library("ggplot2")


# Define UI ---------------------------------------------------------------
default_name <- "Daniel"
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio1", label = h4("Which input?"),
                   choices = list("Text Box" = 1,
                                  "Uploaded File" = 2), 
                   selected = 1),
      textInput("text1", label = h5("Enter name (or multiple names separated by a comma)"), 
                value = default_name),
      fileInput("file1", label = h5("Or, upload a file with a list of names to get the group prediction."),
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    ),
    mainPanel(
      plotOutput("AgeSexPlot")
    )
  )
)


# Define server -----------------------------------------------------------
server <- function(input, output) {
  inRadio <- reactive({input$radio1})
  inFile <- reactive({input$file1})
  inText <- reactive({input$text1})
  sdf <- reactive({
    if(inRadio() == 1) {
      d1 <- gsub(", ",",", inText())
      d1 <- data.frame(strsplit(d1, ","))
    } else if(inRadio() == 2 & !is.null(inFile()$datapath)){
      d1 <- read.table(inFile()$datapath, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    } else (
      d1 <- default_name
    )
    
    names(d1) <- "firstname"
    d1$firstname <- tolower(d1$firstname)
    
    bn <- babynames
    bn_start <- 1934
    bn <- bn[bn$year >= bn_start,]
    colnames(bn)[colnames(bn) %in% "name"] <- "firstname"
    bn$firstname <- tolower(bn$firstname)
    bn$sex[bn$sex == "F"] <- "Female"
    bn$sex[bn$sex == "M"] <- "Male"
    
    s1 <- merge(x = d1, y = bn, by = "firstname")
    if(nrow(s1) == 0) s1 <- rbind(s1, data.frame(firstname = d1$firstname, 
                                                 year = bn_start, sex = "Female", n = 0, prop = 0, 
                                                 stringsAsFactors = FALSE))
    s1 <- s1[!is.na(s1$n),]
    s1 <- aggregate(formula = n ~ year + sex, data = s1, FUN = sum, na.rm = TRUE)
    s1$age <- as.numeric(format(Sys.Date(), "%Y")) - s1$year
    s1$sex <- ordered(s1$sex, levels = c("Female", "Male"))
    s1
  })
  output$AgeSexPlot <- renderPlot({
    ggplot(data = sdf(), aes(x = age, y = n, group = sex, fill = sex)) + 
      theme_wsj(color = "gray") + 
      scale_fill_manual(values = c("pink", "blue"), drop = FALSE) + 
      geom_bar(stat = "identity", width = 1.0) + 
      theme(legend.position = "none") + 
      theme(axis.title = element_text(size = 12)) + 
      facet_wrap(~sex, drop = FALSE) + 
      coord_cartesian(xlim = c(5, 79)) + 
      scale_x_continuous(name = "Age", breaks = seq(from = 5, to = 85, by = 10)) + 
      scale_y_continuous(name = "# of People", labels = comma) + 
      labs(title = "Age & Sex by First Name")
  })
}


# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)


