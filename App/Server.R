library(shiny)
source("BodyMassIndex.R")
source("my_who_plot.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$text1 <- renderText({
                paste("Your weight is", input$wt, input$wtUnits)
        })
        
        # =====================================================================
        output$text2 <- renderText({
                paste("Your height is", input$ht, input$htUnits)
        })
        
        # =====================================================================
        wt <- reactive({
                aux <- as.numeric(input$wt)
                if(input$wtUnits == "lb"){aux <- aux * 0.453592}
                aux
        })
        
        # =====================================================================
        ht <- reactive({
                aux <- as.numeric(input$ht)
                if(input$htUnits == "in"){aux <- aux * 2.54}
                aux <- aux/100
                aux
        })
        
        # =====================================================================
        result <- reactive({BodyMassIndex(wt(), ht())})
        
        # =====================================================================
        output$text3 <- renderText({
                aux <- result()
                paste("Your BMI is", aux$BMI, "kg/m2. This value belongs to
                      the category", aux$category, ".")
        })
        
        # =====================================================================
        output$plot <- renderPlot({
                aux <- result()
                my_info <- data.frame(my_ht = ht(), my_wt = wt(), Category = aux$category)
                source("myPlot.R")
                g
                g <- g + geom_point(data = my_info, aes(x = my_ht, y = my_wt, size = 2))
                g <- g + geom_text(data = my_info, 
                                   aes(x = my_ht, y = my_wt*1.05, label = "YOU"))
                g
        })
        
        # =====================================================================
        output$text4 <- renderText({
                paste("Your are from", input$nationality, ".")
        })
        
        # =====================================================================
        who <- reactive({
                        data <- read.csv("./data/WHO.csv")
                        for(i in 2:ncol(data)){
                                data[ , i] <- as.character(data[ , i])
                                data[ , i] <- gsub(",", "", data[ , i], fixed = TRUE)
                                data[ , i] <- as.numeric(data[ , i])
                        }
                        data
        })
        
        # =====================================================================
        cols <- reactive({
                        mycol <- names(who())
                        gsub(".", " ", mycol, fixed = TRUE)
        })
        
        # =====================================================================
        output$table1 <- renderTable({
                aux <- grep(input$nationality, cols(), ignore.case = TRUE)
                if(length(aux) > 0){
                        my_table <- who()[ , c(1, aux)]
                        rows <- which(complete.cases(who()[ , aux]))
                        my_table[rows, ]
                } else { data.frame( Info = "Not Available")}
        })
        
        # =====================================================================
        output$whoPlot <- renderPlot({
                my_who_plot(who(), cols(), input$nationality)
        })
})