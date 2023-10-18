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
      patients_t0=filter(patients_t0,patients_t0$eta>=input$edad[1] & patients_t0$eta<=input$edad[2])
      v1<-count(patients_t0$Il.sistema.CAPABLE.probabilmente.si.adatterà.facilmente.alla.mia.vita.di.tutti.i.giorni)
      v1$q<-"Adaptable to patient life"
      v2<-count(patients_t0$CAPABLE.aiuterà.i.medici.a.seguirmi.meglio.durante.il.trattamento)
      v2$q<-"Help doctors to treatment followup"
      v3<-count(patients_t0$CAPABLE.mi.aiuterà.a.gestire.gli.effetti.collaterali.del.trattamento)
      v3$q<-"Help to manage side effects"
      v4<-count(patients_t0$CAPABLE.mi.aiuterà.a.migliorare.il.mio.stile.di.vita)
      v4$q<-"Help to improve lifestyle"
      v5<-count(patients_t0$CAPABLE.mi.darà.supporto.per.affrontare.i.problemi.della.vita.quotidiana)
      v5$q<-"Help daily life problems"
      v6<-count(patients_t0$CAPABLE.mi.aiuterà.a.controllare.le.mie.emozioni.negative..ansia..stress.)
      v6$q<-"Help manage negative emotions"
      v7<-count(patients_t0$Vorrei.avere.l.applicazione.CAPABLE.nel.mio.cellulare.personale)
      v7$q<-"Install in  personal phone"
      v8<-count(patients_t0$CAPABLE.migliorerà.la.qualità.dell.assistenza.sanitaria)
      v8$q<-"Improve quality of healthcare service"
      v9<-count(patients_t0$CAPABLE.migliorerà.la.comunicazione.dei.pazienti.con.il.loro.team.di.professionisti.sanitari)
      v9$q<-"Improve patient - doctor communication"
      v10<-count(patients_t0$CAPABLE.mi.aiuterà.a.migliorare.la.qualità.della.mia.vita)
      v10$q<-"Help to improve QoL"
      expectations <- rbind(v1, v2,v3,v4,v5,v6,v7,v8,v9,v10)
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
   
      patients_t1=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      v1<-(data.frame(table(patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente)))
      v1$q<-"Use frequently"
      v2<-(data.frame(table(patients_t1$Ho.trovato.il.sistema.inutilmente.complesso)))
      v2$q<-"uselessly complexity"
      v3<-(data.frame(table(patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare)))
      v3$q<-"Easy to use"
      v4<-(data.frame(table(patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema)))
      v4$q<-"Technician support"
      v5<-(data.frame(table(patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate)))
      v5$q<-"Functions well integrated"
      v6<-(data.frame(table(patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema)))
      v6$q<-"Too many inconsistencies"
      v7<-(data.frame(table(patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente)))
      v7$q<-"Speedy learning"
      v8<-(data.frame(table(patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare)))
      v8$q<-"Very complex"
      v9<-(data.frame(table(patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema)))
      v9$q<-"Feel capable to use"
      v10<-(data.frame(table(patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema)))
      v10$q<-"Complex learning"
      sus <- rbind(v1, v2,v3,v4,v5,v6,v7,v8,v9,v10)
      View(sus)
      tema<-theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold.italic"),
        axis.title.y = element_text(color="black", size=14, face="bold.italic")
      )
      
      ggplot(sus, aes(q, y = Freq, fill = Var1)) +
        geom_bar(position='stack', stat='identity')  +
        scale_fill_manual(values=c( 'red','orange','grey','lightgreen','green','white'))  +tema + ggtitle("System Usability Scale") +
        ylab("Number of patients_t1") + xlab("Dimensions")+  coord_flip() 
      
    })
    
    output$easiness_t1<- renderPlot({
      
      patients_t1=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      v1<-(data.frame(table(patients_t1$Seguire.le.schermate.introduttive)))
      v1$q<-"Follow indtroduction instruction"
      v2<-(data.frame(table(patients_t1$Usare.la.pagina.iniziale.dell.app)))
      v2$q<-"Use home page"
      v3<-(data.frame(table(patients_t1$Seguire.le.istruzioni.dei.messaggi.di.posta.in.arrivo)))
      v3$q<-"Instruction of messages in the inbox"
      v4<-(data.frame(table(patients_t1$riportare.un.sintomo)))
      v4$q<-"Report a symptom"
      v5<-(data.frame(table(patients_t1$Eseguire.le.attività.proposte.nella.sezione.Obiettivo..Pillole.di.benessere.)))
      v5$q<-"Perform capsule"
      v6<-(data.frame(table(patients_t1$Consultare.i.contenuti.educazionali )))
      v6$q<-"Check educational content"
      v7<-(data.frame(table(patients_t1$Usare.lo.smartwatch)))
      v7$q<-"Use Smartwatch"
      v8<-(data.frame(table(patients_t1$Sincronizzazione.dello.smartwatch)))
      v8$q<-"Sync Smartwatch"
      easiness <- rbind(v1, v2,v3,v4,v5,v6,v7,v8)
 
      tema<-theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold.italic"),
        axis.title.y = element_text(color="black", size=14, face="bold.italic")
      )
      
      ggplot(easiness, aes(q, y = Freq, fill = Var1)) +
        geom_bar(position='stack', stat='identity')  +
        scale_fill_manual(values=c( 'red','orange','grey','lightgreen','green','white'))  +tema + ggtitle("Easiness of CAPABLE patient tasks") +
        ylab("Number of patients_t1") + xlab("Dimensions")+  coord_flip() 
      
    })
    
    output$usefulness_t1<- renderPlot({
      
      patients_t1=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      v1<-(data.frame(table(patients_t1$Le.pagine.di.introduzione.dell.app )))
      v1$q<-"Intro pages of the app"
      v2<-(data.frame(table(patients_t1$La.pagina.iniziale.della.app)))
      v2$q<-"The home page"
      v3<-(data.frame(table(patients_t1$I.messaggi.ricevuti)))
      v3$q<-"Messages in the inbox"
      v4<-(data.frame(table(patients_t1$Riportare.i.sintomi)))
      v4$q<-"Report a symptom"
      v5<-(data.frame(table(patients_t1$Le.attività.proposte.nella.sezione.Obiettivi)))
      v5$q<-"Proposed capsules"
      v6<-(data.frame(table(patients_t1$I.contenuti.educazionali)))
      v6$q<-"Educational content"
      v7<-(data.frame(table(patients_t1$Lo.smartwatch)))
      v7$q<-"Use Smartwatch"

      usefulness <- rbind(v1, v2,v3,v4,v5,v6,v7)
      
      tema<-theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold.italic"),
        axis.title.y = element_text(color="black", size=14, face="bold.italic")
      )
      
      ggplot(usefulness, aes(q, y = Freq, fill = Var1)) +
        geom_bar(position='stack', stat='identity')  +
        scale_fill_manual(values=c( 'red','orange','grey','lightgreen','green','white'))  +tema + ggtitle("Usefulness of CAPABLE patient tasks") +
        ylab("Number of patients_t1") + xlab("Dimensions")+  coord_flip() 
      
    })
    
    
    output$perceived_impact_t1<- renderPlot({
      
      patients_t1=filter(patients_t1,patients_t1$eta>=input$edad2[1] & patients_t1$eta<=input$edad2[2])
      v1<-(data.frame(table(patients_t1$CAPABLE.mi.aiuta.ad.affrontare.il.trattamento.del.cancro)))
      v1$q<-"system helps to cope cancer treatment"
      v2<-(data.frame(table(patients_t1$Il.concetto.CAPABLE.si.adatta.facilmente.alla.mia.vita.di.tutti.i.giorni)))
      v2$q<-"Fit in everiday life"
      v3<-(data.frame(table(patients_t1$CAPABLE.aiuta.i.medici.a.controllarmi.meglio.durante.il.trattamento)))
      v3$q<-"Help doctors to follow up"
      v4<-(data.frame(table(patients_t1$CAPABLE.mi.aiuta.a.gestire.gli.effetti.collaterali.del.trattamento )))
      v4$q<-"Help patient to manage side effect"
      v5<-(data.frame(table(patients_t1$CAPABLE.mi.aiuta.a.migliorare.il.mio.stile.di.vita )))
      v5$q<-"Help to improve lifestyle"
      v6<-(data.frame(table(patients_t1$CAPABLE.mi.supporta.nell.affrontare.i.problemi.della.vita.quotidiana )))
      v6$q<-"Help to cope daily problems"
      v7<-(data.frame(table(patients_t1$CAPABLE.mi.aiuta.a.controllare.le.mie.emozioni.negative..ansia..stress. )))
      v7$q<-"Help manage negative emotions (stress , anxiety)"
      v8<-(data.frame(table(patients_t1$Vorrei.avere.l.applicazione.CAPABLE.nel.mio.cellulare.personale)))
      v8$q<-"CAPABLE installed in patient's mobile"
      v9<-(data.frame(table(patients_t1$CAPABLE.migliora.la.qualità.dell.assistenza.sanitaria )))
      v9$q<-"Improve quality of health service"
      v10<-(data.frame(table(patients_t1$CAPABLE.migliora.la.comunicazione.dei.pazienti.con.il.personale.medico)))
      v10$q<-"Improve patient -doctor communication "
      v11<-(data.frame(table(patients_t1$CAPABLE.mi.aiuta.a.migliorare.la.qualità.della.mia.vita )))
      v11$q<-"Help to improve QoL"

      
      perceived_impact <- rbind(v1, v2,v3,v4,v5,v6,v7,v8,v9,v10,v11)
      
      tema<-theme(
        plot.title = element_text(color="black", size=20, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold.italic"),
        axis.title.y = element_text(color="black", size=14, face="bold.italic")
      )
      
      ggplot(perceived_impact, aes(q, y = Freq, fill = Var1)) +
        geom_bar(position='stack', stat='identity')  +
        scale_fill_manual(values=c( 'red','orange','grey','lightgreen','green','white'))  +tema + ggtitle("Perceived impact") +
        ylab("Number of patients_t1") + xlab("Dimensions")+  coord_flip() 
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
