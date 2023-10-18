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
View(expectations)