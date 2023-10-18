library(xlsx)
library(dplyr) 
library(reshape2)
library(lubridate)
library(plyr)
## first version of excel (July 2023)
#excel_file <- "data/CAPABLE_PAVIA_PazientiArruo-UXT0_DATA_LABELS_2023-07-21_0912.csv"
#pavia<-read.csv(excel_file, header=TRUE, sep=";")
## september version
excel_file <- "data/UXT0_Pavia.xlsx"
pavia<-read.xlsx(excel_file, 1 )
pavia$Record.ID<-paste("PAVIA",pavia$Record.ID)
pavia$Data.di.nascita<-as.Date(pavia$Data.di.nascita)
pavia$Data.di.arruolamento<-as.Date(pavia$Data.di.arruolamento)
pavia$eta<-as.numeric(round((pavia$Data.di.arruolamento-pavia$Data.di.nascita)/365,digits=0))
## first version of excel (July 2023)
#excel_file <- "data/CAPABLE_BARI_PazientiArruo-UXT0_DATA_LABELS_2023-07-21_0913.csv"
#bari<-read.csv(excel_file, header=TRUE, sep=";")
excel_file <- "data/UXT0_Bari.xlsx"
bari<-read.xlsx(excel_file, 1 )
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

excel_file <- "data/UXT1_Pavia.xlsx"
pavia<-read.xlsx(excel_file, 1 )
pavia$Record.ID<-paste("PAVIA",pavia$Record.ID)
excel_file <- "data/UXT1_Bari.xlsx"
bari<-read.xlsx(excel_file, 1 )
View(bari)
bari$Record.ID<-paste("BARI",bari$Record.ID)
patients_t1 <- rbind(pavia, bari)

#View(patients_t1)
patient_info<-data.frame(patients_t0$Record.ID, patients_t0$Sesso, patients_t0$eta)
colnames(patient_info)<-c('Record.ID','Sesso','eta')
#View(patient_info)
patients_t1<-merge(patient_info,patients_t1, by.x = "Record.ID", all.x=FALSE)
View(patients_t1)
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




lickert3<-c("Molto difficile", "Difficile", "Neutrale","Facile","Molto facile")
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)
#patients_t1$ <-ordered(patients_t1$ ,levels =lickert3)




