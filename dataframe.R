library(xlsx)
library(dplyr) 
library(reshape2)
library(lubridate)
library(plyr)
## first version of excel (July 2023)
#excel_file <- "data/CAPABLE_PAVIA_PazientiArruo-UXT0_DATA_LABELS_2023-07-21_0912.csv"
#pavia<-read.csv(excel_file, header=TRUE, sep=";")
## september version
pavia<-read.xlsx(UXT0_Pavia, 1 )
pavia$Record.ID<-paste("PAVIA",pavia$Record.ID)
pavia$Data.di.nascita<-as.Date(pavia$Data.di.nascita)
pavia$Data.di.arruolamento<-as.Date(pavia$Data.di.arruolamento)
pavia$eta<-as.numeric(round((pavia$Data.di.arruolamento-pavia$Data.di.nascita)/365,digits=0))
## first version of excel (July 2023)
#excel_file <- "data/CAPABLE_BARI_PazientiArruo-UXT0_DATA_LABELS_2023-07-21_0913.csv"
#bari<-read.csv(excel_file, header=TRUE, sep=";")
bari<-read.xlsx(UXT0_Bari, 1 )
bari$Record.ID<-paste("BARI",bari$Record.ID)
bari$Data.di.nascita<-as.Date(bari$Data.di.nascita)
bari$Data.di.arruolamento<-as.Date(bari$Data.di.arruolamento)
bari$eta<-as.numeric(round((bari$Data.di.arruolamento-bari$Data.di.nascita)/365,digits=0))

patients_t0 <- rbind(pavia, bari)
patients_t0$Sesso<-as.factor(patients_t0$Sesso)
patients_t0$Tipo.di.tumore<-as.factor(patients_t0$Tipo.di.tumore)
lickert<-c("Molto in disaccordo", "In disaccordo", "Neutrale", "D'accordo","Molto d'accordo")
#aggre<-ordered(patients_t0$Il.sistema.CAPABLE.mi.aiuterà.ad.affrontare.il.trattamento.del.cancro,levels = lickert)

patients_t0$Il.sistema.CAPABLE.probabilmente.si.adatterà.facilmente.alla.mia.vita.di.tutti.i.giorni<-ordered(patients_t0$Il.sistema.CAPABLE.probabilmente.si.adatterà.facilmente.alla.mia.vita.di.tutti.i.giorni, levels = lickert)
patients_t0$CAPABLE.aiuterà.i.medici.a.seguirmi.meglio.durante.il.trattamento<-ordered(patients_t0$CAPABLE.aiuterà.i.medici.a.seguirmi.meglio.durante.il.trattamento, levels = lickert)
patients_t0$CAPABLE.mi.aiuterà.a.gestire.gli.effetti.collaterali.del.trattamento<-ordered(patients_t0$CAPABLE.mi.aiuterà.a.gestire.gli.effetti.collaterali.del.trattamento, levels = lickert)
patients_t0$CAPABLE.mi.aiuterà.a.migliorare.il.mio.stile.di.vita<-ordered(patients_t0$CAPABLE.mi.aiuterà.a.migliorare.il.mio.stile.di.vita, levels = lickert)
patients_t0$CAPABLE.mi.darà.supporto.per.affrontare.i.problemi.della.vita.quotidiana<-ordered(patients_t0$CAPABLE.mi.darà.supporto.per.affrontare.i.problemi.della.vita.quotidiana, levels = lickert)
patients_t0$CAPABLE.mi.aiuterà.a.controllare.le.mie.emozioni.negative..ansia..stress.<-ordered(patients_t0$CAPABLE.mi.aiuterà.a.controllare.le.mie.emozioni.negative..ansia..stress., levels = lickert)
patients_t0$Vorrei.avere.l.applicazione.CAPABLE.nel.mio.cellulare.personale<-ordered(patients_t0$Vorrei.avere.l.applicazione.CAPABLE.nel.mio.cellulare.personale,levels = lickert)
patients_t0$CAPABLE.migliorerà.la.qualità.dell.assistenza.sanitaria<-ordered(patients_t0$CAPABLE.migliorerà.la.qualità.dell.assistenza.sanitaria, levels = lickert)
patients_t0$CAPABLE.migliorerà.la.comunicazione.dei.pazienti.con.il.loro.team.di.professionisti.sanitari<-ordered(patients_t0$CAPABLE.migliorerà.la.comunicazione.dei.pazienti.con.il.loro.team.di.professionisti.sanitari,levels = lickert)
patients_t0$CAPABLE.mi.aiuterà.a.migliorare.la.qualità.della.mia.vita<-ordered(patients_t0$CAPABLE.mi.aiuterà.a.migliorare.la.qualità.della.mia.vita, levels = lickert)
#View (patients_t0)


pavia<-read.xlsx(UXT1_Pavia, 1 )
pavia$Record.ID<-paste("PAVIA",pavia$Record.ID)

bari<-read.xlsx(UXT1_Bari, 1 )
#View(bari)
bari$Record.ID<-paste("BARI",bari$Record.ID)
names(bari)<-names(pavia)
patients_t1 <- rbind(pavia, bari)

#View(patients_t1)
patient_info<-data.frame(patients_t0$Record.ID, patients_t0$Sesso, patients_t0$eta)
colnames(patient_info)<-c('Record.ID','Sesso','eta')
#View(patient_info)
patients_t1<-merge(patient_info,patients_t1, by.x = "Record.ID", all.x=FALSE)
#View(patients_t1)
lickert2<-c("Completamente in disaccordo", "Non d'accordo", "Neutrale","D'accordo","Completamente d'accordo")
patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente<-ordered(patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente,levels =lickert2)
patients_t1$Ho.trovato.il.sistema.inutilmente.complesso<-ordered(patients_t1$Ho.trovato.il.sistema.inutilmente.complesso,levels =lickert2)
patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare<-ordered(patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare,levels =lickert2)
patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema<-ordered(patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema,levels =lickert2)
patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate<-ordered(patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate,levels =lickert2)
patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema<-ordered(patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema,levels =lickert2)
patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente<-ordered(patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente,levels =lickert2)
patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare<-ordered(patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare,levels =lickert2)
patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema<-ordered(patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema,levels =lickert2)
patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema<-ordered(patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema,levels =lickert2)

patients_t1 <- transform(patients_t1,
                         
                         Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente = as.numeric(Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente) ,   
                         Ho.trovato.il.sistema.inutilmente.complesso = as.numeric(Ho.trovato.il.sistema.inutilmente.complesso),
                         Pensavo.che.il.sistema.fosse.facile.da.usare = as.numeric(Pensavo.che.il.sistema.fosse.facile.da.usare),
                         Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema = as.numeric(Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema),
                         Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate = as.numeric(Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate),
                         Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema=as.numeric(Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema),
                         Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente =as.numeric(Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente),
                         Ho.trovato.il.sistema.molto.complicato.da.usare = as.numeric(Ho.trovato.il.sistema.molto.complicato.da.usare),
                         Mi.sentivo.molto.fiducioso.nell.usare.il.sistema =as.numeric(Mi.sentivo.molto.fiducioso.nell.usare.il.sistema),
                         Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema=as.numeric(Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema)
)
patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente<-patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente-1
patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare<-patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare-1
patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate<-patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate-1
patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente<-patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente-1
patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema<-patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema-1

patients_t1$Ho.trovato.il.sistema.inutilmente.complesso<-5-patients_t1$Ho.trovato.il.sistema.inutilmente.complesso
patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema<-5-patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema
patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema<-5-patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema
patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare<-5-patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare
patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema<-5-patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema

patients_t1$SUS<-(patients_t1$Penso.che.mi.piacerebbe.usare.questo.sistema.frequentemente
                  +patients_t1$Ho.trovato.il.sistema.inutilmente.complesso
                  +patients_t1$Pensavo.che.il.sistema.fosse.facile.da.usare
                  +patients_t1$Penso.che.avrei.bisogno.del.supporto.di.una.persona.tecnica.per.poter.utilizzare.questo.sistema
                
                  +patients_t1$Ho.trovato.che.le.varie.funzioni.di.questo.sistema.erano.ben.integrate
                  +patients_t1$Ho.pensato.che.ci.fossero.troppe.incongruenze.in.questo.sistema
                  +patients_t1$Immagino.che.la.maggior.parte.delle.persone.imparerebbe.a.usare.questo.sistema.molto.velocemente
                  +patients_t1$Ho.trovato.il.sistema.molto.complicato.da.usare
                  +patients_t1$Mi.sentivo.molto.fiducioso.nell.usare.il.sistema
                  +patients_t1$Ho.dovuto.imparare.molte.cose.prima.di.poter.usare.questo.sistema
)*2.5
lickert3<-c("Molto difficile", "Difficile", "Neutrale","Facile","Molto facile")
patients_t1$Seguire.le.schermate.introduttive <-ordered(patients_t1$Seguire.le.schermate.introduttive ,levels =lickert3)
patients_t1$Usare.la.pagina.iniziale.dell.app <-ordered(patients_t1$Usare.la.pagina.iniziale.dell.app ,levels =lickert3)
patients_t1$Seguire.le.istruzioni.dei.messaggi.di.posta.in.arrivo <-ordered(patients_t1$Seguire.le.istruzioni.dei.messaggi.di.posta.in.arrivo ,levels =lickert3)
patients_t1$riportare.un.sintomo <-ordered(patients_t1$riportare.un.sintomo ,levels =lickert3)
patients_t1$Eseguire.le.attività.proposte.nella.sezione.Obiettivo..Pillole.di.benessere. <-ordered(patients_t1$Eseguire.le.attività.proposte.nella.sezione.Obiettivo..Pillole.di.benessere. ,levels =lickert3)
patients_t1$Consultare.i.contenuti.educazionali <-ordered(patients_t1$Consultare.i.contenuti.educazionali ,levels =lickert3)
patients_t1$Usare.lo.smartwatch <-ordered(patients_t1$Usare.lo.smartwatch ,levels =lickert3)
patients_t1$Sincronizzazione.dello.smartwatch <-ordered(patients_t1$Sincronizzazione.dello.smartwatch ,levels =lickert3)


lickert4<-c("Per niente utile", "Inutile", "Neutrale","Utile","Molto utile")
patients_t1$Le.pagine.di.introduzione.dell.app <-ordered(patients_t1$Le.pagine.di.introduzione.dell.app ,levels =lickert4)
patients_t1$La.pagina.iniziale.della.app <-ordered(patients_t1$La.pagina.iniziale.della.app ,levels =lickert4)
patients_t1$I.messaggi.ricevuti <-ordered(patients_t1$I.messaggi.ricevuti ,levels =lickert4)
patients_t1$Riportare.i.sintomi <-ordered(patients_t1$Riportare.i.sintomi ,levels =lickert4)
patients_t1$Le.attività.proposte.nella.sezione.Obiettivi <-ordered(patients_t1$Le.attività.proposte.nella.sezione.Obiettivi ,levels =lickert4)
patients_t1$I.contenuti.educazionali <-ordered(patients_t1$I.contenuti.educazionali ,levels =lickert4)
patients_t1$Lo.smartwatch <-ordered(patients_t1$Lo.smartwatch ,levels =lickert4)


patients_t1$CAPABLE.mi.aiuta.ad.affrontare.il.trattamento.del.cancro <-ordered(patients_t1$CAPABLE.mi.aiuta.ad.affrontare.il.trattamento.del.cancro ,levels =lickert2)
patients_t1$Il.concetto.CAPABLE.si.adatta.facilmente.alla.mia.vita.di.tutti.i.giorni <-ordered(patients_t1$Il.concetto.CAPABLE.si.adatta.facilmente.alla.mia.vita.di.tutti.i.giorni ,levels =lickert2)
patients_t1$CAPABLE.aiuta.i.medici.a.controllarmi.meglio.durante.il.trattamento <-ordered(patients_t1$CAPABLE.aiuta.i.medici.a.controllarmi.meglio.durante.il.trattamento ,levels =lickert2)
patients_t1$CAPABLE.mi.aiuta.a.gestire.gli.effetti.collaterali.del.trattamento <-ordered(patients_t1$CAPABLE.mi.aiuta.a.gestire.gli.effetti.collaterali.del.trattamento ,levels =lickert2)
patients_t1$CAPABLE.mi.aiuta.a.migliorare.il.mio.stile.di.vita <-ordered(patients_t1$CAPABLE.mi.aiuta.a.migliorare.il.mio.stile.di.vita ,levels =lickert2)
patients_t1$CAPABLE.mi.supporta.nell.affrontare.i.problemi.della.vita.quotidiana <-ordered(patients_t1$CAPABLE.mi.supporta.nell.affrontare.i.problemi.della.vita.quotidiana ,levels =lickert2)
patients_t1$CAPABLE.mi.aiuta.a.controllare.le.mie.emozioni.negative..ansia..stress. <-ordered(patients_t1$CAPABLE.mi.aiuta.a.controllare.le.mie.emozioni.negative..ansia..stress. ,levels =lickert2)
patients_t1$Vorrei.avere.l.applicazione.CAPABLE.nel.mio.cellulare.personale <-ordered(patients_t1$Vorrei.avere.l.applicazione.CAPABLE.nel.mio.cellulare.personale ,levels =lickert2)
patients_t1$CAPABLE.migliora.la.qualità.dell.assistenza.sanitaria <-ordered(patients_t1$CAPABLE.migliora.la.qualità.dell.assistenza.sanitaria ,levels =lickert2)
patients_t1$CAPABLE.migliora.la.comunicazione.dei.pazienti.con.il.personale.medico <-ordered(patients_t1$CAPABLE.migliora.la.comunicazione.dei.pazienti.con.il.personale.medico ,levels =lickert2)
patients_t1$CAPABLE.mi.aiuta.a.migliorare.la.qualità.della.mia.vita <-ordered(patients_t1$CAPABLE.mi.aiuta.a.migliorare.la.qualità.della.mia.vita ,levels =lickert2)

#patients_t1$ <-ordered(patients_t1$ ,levels =lickert2)

#View(patients_t1)


## Normalization of SUS






