## 1:fake_data 2: real data : not available on github.
type_data<-1
## fake_data is the folder with  fake data, available on github.
## data is the folder with the REAL data, NOT available on github.
folder<-c("fake_data/","data/")
## Link to Excel files - this info is used by dataframe.R to prepare the R data.
UXT0_Pavia <- paste(folder[type_data],"UXT0_Pavia.xlsx",sep="")
UXT0_Bari <- paste(folder[type_data],"UXT0_Bari.xlsx",sep="")
UXT1_Pavia <-paste(folder[type_data],"UXT1_Pavia.xlsx",sep="")
UXT1_Bari <- paste(folder[type_data],"UXT1_Bari.xlsx",sep="")

