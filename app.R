library(shiny)
library(ggplot2)
source(file = "dataframe.R", local = T)
minAge <- min(patients_t0$eta[!is.na(patients_t0$eta)])
maxAge <- max(patients_t0$eta[!is.na(patients_t0$eta)])
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("CAPABLE integrated analysis tool"),
  tabsetPanel(
    tabPanel("UX Study (T0)", 
          #   fileInput("file", "Data", buttonLabel = "Upload..."),
          #   textInput("delim", "Delimiter (leave blank to guess)", ""),
          #   numericInput("skip", "Rows to skip", 0, min = 0),
          #   numericInput("rows", "Rows to preview", 10, min = 1),
          sliderInput(inputId="edad", "Age filter",
                      min = minAge, max = maxAge,
                      value = c(minAge, maxAge)),
          titlePanel("CAPABLE Questionnaire UX enrollment (Italy)"),
             plotOutput("gender_age"),
           plotOutput("expectations")
    ),
    tabPanel("UX Study (T1)",
             sliderInput(inputId="edad2", "Age filter",
                         min = minAge, max = maxAge,
                         value = c(minAge, maxAge)),
             plotOutput("gender_age2"),
             plotOutput("overall_satisfaction_t1"),
             plotOutput("sus_t1"),
             plotOutput("easiness_t1"),
             plotOutput("usefulness_t1"),
             plotOutput("perceived_impact_t1"),
             plotOutput("trust_t1")
             ),
    tabPanel("QoL study"),
    tabPanel("Other clinical outcomes")
  ),

    # Application title
   
)

server <- function(input, output) {

  output$gender_age <- renderPlot({
    temp=filter(patients_t0,patients_t0$eta>=input$edad[1] & patients_t0$eta<=input$edad[2])
    
      ggplot(temp, aes(x=Sesso, y=eta))  +
        geom_point(size=2)
    })
    output$expectations <- renderPlot({
      source(file = "filter.R", local = T)
      tema<-theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold.italic"),
        axis.title.y = element_text(color="black", size=14, face="bold.italic")
      )
      
      ggplot(expectations, aes(q, y = freq, fill = x)) +
        geom_bar(position='stack', stat='identity')  +
        scale_fill_manual(values=c( 'red','orange','grey','lightgreen','green','white'))  +tema + ggtitle("Expectation questionnaire @ Enrollment") +
        ylab("Number of patients_t0") + xlab("Dimensions")+  coord_flip() 
     
    })
  ##UX Study T1
    output$gender_age2 <- renderPlot({
      temp=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      
      ggplot(temp, aes(x=Sesso, y=eta))  +
        geom_point(size=2)
    })
    
    output$overall_satisfaction_t1 <- renderPlot({
      #create data frame
      patients_t1=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      
      temp<-count(patients_t1$Quale.è.il.suo.livello.di.soddisfazione.globale.in.CAPABLE.)
     # data <- data.frame("category" = c('Inadequate', 'Poor', 'Acceptable', 'Good','Excellent'),
      #                  "amount" = c(25, 40, 27, 8))
      
      #create pie chart
      ggplot(temp, aes(x="", y=freq, fill=x)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)
    })
    output$sus_t1 <- renderPlot({
      #source(file = "filter.R", local = T)
      patients_t1=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      v1<-count(patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente)
      v1$q<-"Use frequently"
      v2<-count(patients_t1$Ho.trovato.il.sistema.inutilmente.complesso)
      v2$q<-"uselessly complexity"
      v3<-count(patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare)
      v3$q<-"Easy to use"
      v4<-count(patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema)
      v4$q<-"Technician support"
      v5<-count(patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate)
      v5$q<-"Functions well integrated"
      v6<-count(patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema)
      v6$q<-"Too many inconsistencies"
      v7<-count(patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente)
      v7$q<-"Speedy learning"
      v8<-count(patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare)
      v8$q<-"Very complex"
      v9<-count(patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema)
      v9$q<-"Feel capable to use"
      v10<-count(patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema)
      v10$q<-"Complex learning"
      sus <- rbind(v1, v2,v3,v4,v5,v6,v7,v8,v9,v10)
      View(sus)
      tema<-theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold.italic"),
        axis.title.y = element_text(color="black", size=14, face="bold.italic")
      )
      
      ggplot(sus, aes(q, y = freq, fill = x)) +
        geom_bar(position='stack', stat='identity')  +
        scale_fill_manual(values=c( 'red','orange','grey','lightgreen','green','white'))  +tema + ggtitle("System Usability Scale") +
        ylab("Number of patients_t1") + xlab("Dimensions")+  coord_flip() 
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
