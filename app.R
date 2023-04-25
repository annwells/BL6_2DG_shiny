#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(here)
library(tidyverse)

# load data
BL6_2DG <- readRDS(here("B6_2DG_shiny","Data","log.tdata.FPKM.sample.info.subset.with.interactions.and.gene.names.RData"))
BL6_2DG <- BL6_2DG[,-c(17333, 17336:17347)]
BL6_2DG$Treatment <- as.character(BL6_2DG$Treatment)
BL6_2DG$Tissue <- as.character(BL6_2DG$Tissue)
BL6_2DG[BL6_2DG == "Pre-frontal Cortex"] <- "Prefrontal Cortex"
BL6_2DG[BL6_2DG == "Hypothanamus"] <- "Hypothalamus"
BL6_2DG[BL6_2DG == "None"] <- "Control"
BL6_2DG$Treatment <- as.factor(BL6_2DG$Treatment)
BL6_2DG$Tissue <- as.factor(BL6_2DG$Tissue)

# BL6_2DG <- readRDS(here("Chang_2DG","Data","log.tdata.FPKM.sample.info.subset.RData"))
# BL6_2DG$Time <- as.factor(BL6_2DG$Time)
# BL6_2DG$Tissue <- as.factor(BL6_2DG$Tissue)
# BL6_2DG$Treatment <- as.factor(BL6_2DG$Treatment)

tissue.opts <- c("Heart","Hippocampus","Hypothalamus","Kidney","Liver", 
                 "Prefrontal Cortex","Skeletal Muscle","Small Intestine","Spleen")
treatment.opts <- c("Control","2DG")
var.opts <- colnames(BL6_2DG[,-c(17334:17335)])

#plotting theme for ggplot2
.theme <- theme(
    axis.line = element_line(colour = 'gray', size = .75),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.title.y = element_text(size = 16), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
)

# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("BL6 2DG"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("variable","Gene:", choices = var.opts),
            checkboxGroupInput("tissue_select","Tissue:", tissue.opts, selected = c("Heart","Hypothalamus","Liver",
                                                                                    "Prefrontal Cortex","Skeletal Muscle",
                                                                                    "Small Intestine") # This specify the initial condition
            ),
            checkboxGroupInput("treatment_select","Treatment:", treatment.opts, selected = c("Control","2DG")),
            selectInput("plot.type","Plot Type:", list(boxplot = "boxplot", density = "density", bar = "bar")),
            h6("At least one box must be selected for Tissue and Treatment"),
            checkboxInput("show.points", "show points", TRUE),
            checkboxInput("show.errorbars", "show errorbars", TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("caption")),
            #h3(htmlOutput("caption")),
            uiOutput("plot") # depends on input
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #variables based on the data
    # observe({
    #     
    #     #tissue.opts <- c("Heart","Hippocampus","Hypothalamus","Kidney","Liver",
    #     #                 "Prefrontal Cortex","Skeletal Muscle","Small Intestine","Spleen")
    #     #treatment.opts <- c("Control","2DG")
    #     #updateSelectInput(session, "variable", choices = var.opts)
    #     updateSelectInput(session, "tissue_select", choices = tissue.opts)
    #     updateSelectInput(session, "treatment_select", choices = treatment.opts)
    # })
    # 
    tmp_data <- reactiveVal()
    tmp_data({data})
    error_bars <- reactiveVal()
    error_bars({data})
    
    #observe({updateCheckboxGroupInput(session, "tissue_select", choices = tmp_data(), selected = tmp_data())
    #         updateCheckboxGroupInput(session, "treatment_select", tmp_data())})
    
    observeEvent( 
        c(input$tissue_select, input$treatment_select, input$variable),
        {
            tmp_data({ BL6_2DG %>% 
                    filter(
                        { if(!is.null(input$tissue_select)) Tissue %in% input$tissue_select }
                        ) %>%
                        filter (
                            {if(!is.null(input$treatment_select)) Treatment %in% input$treatment_select}
                             ) %>%
                     mutate("Tissue_by_Treatment" = paste0(Tissue, ":", Treatment)) %>%
                      group_by(Tissue_by_Treatment) %>%
                          mutate(mean = mean(eval(parse(text = input$variable)), na.rm = TRUE),
                                    sd = sd(eval(parse(text = input$variable)), na.rm = TRUE),
                                 lower = mean - sd,
                                 upper = mean + sd)
                                                  
                
        })
            }
        
, ignoreInit = F, ignoreNULL = F )
    
    # error_bars <- tmp_data() %>%
    #     group_by(Tissue_by_Treatment) %>%
    #     summarise(mean = mean(eval(parse(text = input$variable)), na.rm = TRUE),
    #               sd = sd(eval(parse(text = input$variable)), na.rm = TRUE))

    # observeEvent(
    #     c(input$tissue_select,input$treatment_select),
    #         {
    #             error_bars({ tmp_data() %>%
    #                  group_by("Tissue_by_Treatment") %>%
    #              summarize(mean = mean(.data[["Tissue_by_Treatment"]], na.rm = TRUE),
    #                        sd = sd(.data[["Tissue_by_Treatment"]], na.rm = TRUE)
    #                        ) %>%
    #                     ungroup()
    #         })
    #     }
    # 
    #     , ignoreInit = F, ignoreNULL = F )
    
    output$caption<-renderText({
        switch(input$plot.type,
               "boxplot" 	= 	"Boxplot",
               "density" 	=	"Density plot",
               "bar" 		=	"Bar graph")
    })
    
    output$plot <- renderUI({
        plotOutput("p")
    })
    
    #plotting function using ggplot2
    output$p <- renderPlot({
        
        
        
        #plot types
        plot.type<-switch(input$plot.type,
                          "boxplot" 	=   geom_boxplot(),
                          "density" 	=	geom_density(alpha=.75),
                          "bar" 		=	geom_bar(stat = "summary", position = position_identity())
        )
        
        
        if(input$plot.type== "boxplot")	{		#control for 1D or 2D graphs
            p<-ggplot(tmp_data(),
                      aes(
                          x 		= Tissue_by_Treatment,
                          y 		= eval(parse(text = input$variable)),
                          fill 	= Tissue_by_Treatment # let type determine plotting
                      )
            ) + plot.type
            
            if(input$show.points==TRUE)
            {
                p<-p+ geom_jitter(color='black',alpha=0.5, position = position_jitter(0.2))
            }
            
        } else if(input$plot.type== "bar")	{		#control for 1D or 2D graphs
            p <- ggplot(tmp_data(),
                        aes(
                            x 		= Tissue_by_Treatment,
                            y 		= eval(parse(text = input$variable)),
                            fill 	= Tissue_by_Treatment, # let type determine plotting
                            ymin = lower, 
                            ymax = upper
                        )
            ) + plot.type 
            p <- p
            
            if(input$show.points == TRUE & input$show.errorbars == FALSE)
            {
                p <- p + geom_point(color='black',alpha=0.5, position = "jitter")
                
            } else if(input$show.points == TRUE & input$show.errorbars == TRUE){
                #sd <- rep(std.error(plot.obj$variable),length(plot.obj$variable))
                
                p <- p + geom_point(color='black',alpha=0.5, position = "jitter") +
                    geom_errorbar(width=.4, position = position_identity())
                
                
            } else if(input$show.points == FALSE & input$show.errorbars == TRUE){
                #sd <- rep(std.error(plot.obj$variable),length(plot.obj$variable))
                
                p <- p + geom_errorbar(width=.4, position = position_identity())
            }
            
        } else {
            
            p<-ggplot(tmp_data(),
                      aes_string(
                          x 		= input$variable,
                          fill 	= "Tissue_by_Treatment",
                          group 	= "Tissue_by_Treatment"
                          #color 	= as.factor(plot.obj$group)
                      )
            ) + plot.type
        }
        
        p<-p+labs(
            fill 	= input$variable,
            x 		= "",
            y 		= input$variable
        )  +
            .theme
        print(p)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
