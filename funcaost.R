library(readxl)
library(shiny)
library(imputeTS)
library(dplyr)
library(plyr)
library(editData)


Dados<-read_excel("Tempo05junho.xlsx", sheet=1, col_names=TRUE)

attach(Dados)

SdmInc <- (SdmNewCases/1854581)*100000

SdmMort <- SdmNewDeaths/1854581*100000

SdmLet <- SdmMort/SdmInc*100

stSdmInc <- ts(SdmInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stSdmMort <- ts(SdmMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stSdmLet <- ts(SdmLet,
               start = 14+4/7,
               end = 23+6/7,
               frequency = 7)

MGInc <- (MGNewCases/21168791)*100000


MGMort <- MGNewDeaths/21168791*100000

MGLet <- MGMort/MGInc*100

stMGInc <- ts(MGInc,
              start = 9+3/7,
              end = 23+6/7,
              frequency = 7)

stMGMort <- ts(MGMort,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stMGLet <- ts(MGLet,
              start = 12+2/7,
              end = 23+6/7,
              frequency = 7)


AlfInc <- AlfNewCases/79996*100000

AlfMort <- AlfNewDeaths/79996*100000

AlfLet <- AlfMort/ AlfInc*100

stAlfInc <- ts(AlfInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stAlfMort <- ts(AlfMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stAlfLet <- ts(AlfLet,
               start = 21+4/7,
               end = 23+6/7,
               frequency = 7)

############################################################################################

##  municipio po?os de caldas 

PdcInc <- PdcNewCases/167397*100000

PdcMort <- PdcNewDeaths/167397*100000

PdcLet <- PdcMort/ PdcInc*100

stPdcInc <- ts(PdcInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stPdcMort <- ts(PdcMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stPdcLet <- ts(PdcLet,
               start = 21+5/7,
               end = 23+6/7,
               frequency = 7)
##############################################################################################

##  municipio Varginha 

VarInc <- VarNewCases/ 135558*100000
VarMort <- VarNewDeaths/135558*100000
VarLet <- VarMort/ VarInc*100

stVarInc <- ts(VarInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stVarMort <- ts(VarMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stVarLet <- ts(VarLet,
               start = 21+4/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  UF S?o Paulo 

SPInc <- SPNewCases/ 45919049*100000

SPMort <- SPNewDeaths/45919049*100000

SPLet <- SPMort/ SPInc*100

stSPInc <- ts(SPInc,
              start = 9+3/7,
              end = 23+6/7,
              frequency = 7)

stSPMort <- ts(SPMort,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stSPLet <- ts(SPLet,
              start = 11+4/7,
              end = 23+6/7,
              frequency = 7)

##############################################################################################

##  Oeste de Minas 

OdmInc <- OdmNewCases/ 382291*100000

OdmMort <- OdmNewDeaths/382291*100000

OdmLet <- OdmMort/ OdmInc*100

stOdmInc <- ts(OdmInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stOdmMort <- ts(OdmMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stOdmLet <- ts(OdmLet,
               start = 14+1/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  Metropolitana de Belo Horizonte Mbh

MbhInc <- MbhNewCases/ 3769456*100000

MbhMort <- MbhNewDeaths/3769456*100000

MbhLet <- MbhMort/MbhInc*100


stMbhInc <- ts(MbhInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stMbhMort <- ts(MbhMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stMbhLet <- ts(MbhLet,
               start = 12+2/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

## Campinas (Cam)

CamInc <- CamNewCases/ 2664483*100000

CamMort <- CamNewDeaths/2664483*100000

CamLet <- CamMort/ CamInc*100

stCamInc <- ts(CamInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stCamMort <- ts(CamMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stCamLet <- ts(CamLet,
               start = 14+2/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

## Ribeir?o Preto (Rbp)

RbpInc <- RbpNewCases/ 1056480*100000

RbpMort <- RbpNewDeaths/ 1056480*100000

RbpLet <- RbpMort/ RbpInc*100

stRbpInc <- ts(RbpInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stRbpMort <- ts(RbpMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stRbpLet <- ts(RbpLet,
               start = 18+2/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  Macro Metropolitana Paulista (Mmp)

MmpInc <- MmpNewCases/ 851249*100000

MmpMort <- MmpNewDeaths/ 851249*100000

MmpLet <- MmpMort/ MmpInc*100

stMmpInc <- ts(MmpInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stMmpMort <- ts(MmpMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stMmpLet <- ts(MmpLet,
               start = 13+4/7,
               end = 23+6/7,
               frequency = 7)

plot.ts(stMmpMort)

plot(decompose(stMmpMort)$trend,main="Tendência da Taxa de Letalidade Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado")



##############################################################################################

##  Metropolitana de S?o Paulo (Msp)

MspInc <- MspNewCases/ 15887332*100000

MspMort <- MspNewDeaths/ 15887332*100000

MspLet <- MspMort/ MspInc*100

stMspInc <- ts(MspInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stMspMort <- ts(MspMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stMspLet <- ts(MspLet,
               start = 14+2/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  Vale do Para?ba Paulista (Vpp)

VppInc <- VppNewCases/ 1731600*100000

VppMort <- VppNewDeaths/ 1731600*100000

VppLet <- VppMort/ VppInc*100

stVppInc <- ts(VppInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stVppMort <- ts(VppMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stVppLet <- ts(VppLet,
               start = 17+3/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  Mesorregi?o Araraquara (Ara)

AraInc <- AraNewCases/ 488055*100000

AraMort <- AraNewDeaths/ 488055*100000

AraLet <- AraMort/ AraInc*100

stAraInc <- ts(AraInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stAraMort <- ts(AraMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stAraLet <- ts(AraLet,
               start = 19+1/7,
               end = 23+6/7,
               frequency = 7)


##############################################################################################

##  Piracicaba (Pir)

PirInc <- PirNewCases/ 710256*100000

PirMort <- PirNewDeaths/ 710256*100000

PirLet <- PirMort/ PirInc*100

stPirInc <- ts(PirInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stPirMort <- ts(PirMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stPirLet <- ts(PirLet,
               start = 17+5/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  S?o Jos? do Rio Preto (Sjr)

SjrInc <- SjrNewCases/ 460671*100000

SjrMort <- SjrNewDeaths/ 460671*100000

SjrLet <- SjrMort/ SjrInc*100

stSjrInc <- ts(SjrInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stSjrMort <- ts(SjrMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stSjrLet <- ts(SjrLet,
               start = 14+7/7,
               end = 23+6/7,
               frequency = 7)

##############################################################################################

##  Campo das Vertentes (Cdv)

CdvInc <- CdvNewCases/ 357937*100000

CdvMort <- CdvNewDeaths/ 357937*100000

CdvLet <- CdvMort/ CdvInc*100


stCdvInc <- ts(CdvInc,
               start = 9+3/7,
               end = 23+6/7,
               frequency = 7)

stCdvMort <- ts(CdvMort,
                start = 9+3/7,
                end = 23+6/7,
                frequency = 7)

stCdvLet <- ts(CdvLet,
               start = 21+3/7,
               end = 23+6/7,
               frequency = 7)


regioes <- c("Sul e Sudoeste - MG","MG","SP","Alfenas","Poços de Caldas", "Varginha",
             "Campo das Vertentes", "Oeste de Minas","Metropolitana de BH",
             "Campinas", "Ribeirão Preto", "Macro Metropolitana Paulista","Metropolitana de SP","Vale do Paraíba Paulista","Araraquara","Piracicaba", "São José do Rio Preto")

tipo<-c("Incidência","Mortalidade","Letalidade")

Decomposicao<-c("Observado","Tendência")

stMGInc[sapply(stMGInc, is.infinite)] <- NA

stMGMort[sapply(stMGMort, is.infinite)] <- NA

stMGLet[sapply(stMGLet, is.infinite)] <- NA

stSPInc[sapply(stSPInc, is.infinite)] <- NA

stSPMort[sapply(stSPMort, is.infinite)] <- NA

stSPLet[sapply(stSPLet, is.infinite)] <- NA

stAlfInc[sapply(stAlfInc, is.infinite)] <- NA

stAlfMort[sapply(stAlfMort, is.infinite)] <- NA

stAlfLet[sapply(stAlfLet, is.infinite)] <- NA

stPdcInc[sapply(stPdcInc, is.infinite)] <- NA

stPdcMort[sapply(stPdcMort, is.infinite)] <- NA

stPdcLet[sapply(stPdcLet, is.infinite)] <- NA

stVarInc[sapply(stVarInc, is.infinite)] <- NA

stVarMort[sapply(stVarMort, is.infinite)] <- NA

stVarLet[sapply(stVarLet, is.infinite)] <- NA

stSdmInc[sapply(stSdmInc, is.infinite)] <- NA

stSdmMort[sapply(stSdmMort, is.infinite)] <- NA

stSdmLet[sapply(stSdmLet, is.infinite)] <- NA

stCdvInc[sapply(stCdvInc, is.infinite)] <- NA

stCdvMort[sapply(stCdvMort, is.infinite)] <- NA

stCdvLet[sapply(stCdvLet, is.infinite)] <- NA

stOdmInc[sapply(stOdmInc, is.infinite)] <- NA

stOdmMort[sapply(stOdmMort, is.infinite)] <- NA

stOdmLet[sapply(stOdmLet, is.infinite)] <- NA

stMbhInc[sapply(stMbhInc, is.infinite)] <- NA

stMbhMort[sapply(stMbhMort, is.infinite)] <- NA

stMbhLet[sapply(stMbhLet, is.infinite)] <- NA

stCamInc[sapply(stCamInc, is.infinite)] <- NA

stCamMort[sapply(stCamMort, is.infinite)] <- NA

stCamLet[sapply(stCamLet, is.infinite)] <- NA

stRbpInc[sapply(stRbpInc, is.infinite)] <- NA

stRbpMort[sapply(stRbpMort, is.infinite)] <- NA

stRbpLet[sapply(stRbpLet, is.infinite)] <- NA

stMmpInc[sapply(stMmpInc, is.infinite)] <- NA

stMmpMort[sapply(stMmpMort, is.infinite)] <- NA

stMmpLet[sapply(stMmpLet, is.infinite)] <- NA

stMspInc[sapply(stMspInc, is.infinite)] <- NA

stMspMort[sapply(stMspMort, is.infinite)] <- NA

stMspLet[sapply(stMspLet, is.infinite)] <- NA

stVppInc[sapply(stVppInc, is.infinite)] <- NA

stVppMort[sapply(stVppMort, is.infinite)] <- NA

stVppLet[sapply(stVppLet, is.infinite)] <- NA

stAraInc[sapply(stAraInc, is.infinite)] <- NA

stAraMort[sapply(stAraMort, is.infinite)] <- NA

stAraLet[sapply(stAraLet, is.infinite)] <- NA

stPirInc[sapply(stPirInc, is.infinite)] <- NA

stPirMort[sapply(stPirMort, is.infinite)] <- NA

stPirLet[sapply(stPirLet, is.infinite)] <- NA

stSjrInc[sapply(stSjrInc, is.infinite)] <- NA

stSjrMort[sapply(stSjrMort, is.infinite)] <- NA

stSjrLet[sapply(stSjrLet, is.infinite)] <- NA


funct1<-function(a,b,c)
{`if`(a=="MG" & b=="Incidência"  & c=="Observado", stMGInc%>%plot.ts(main="Taxa de Incidência Diária - Minas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="MG" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stMGInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Minas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="MG" & b=="Mortalidade" & c=="Observado", stMGMort%>%plot.ts(main="Taxa de Mortalidade Diária - Minas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="MG" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stMGMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Minas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="MG" & b=="Letalidade"  & c=="Observado", stMGLet%>%plot.ts(main="Taxa de Letalidade Diária - Minas",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="MG" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stMGLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Minas",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),

`if`(a=="Sul e Sudoeste - MG" & b=="Incidência"  & c=="Observado", stSdmInc%>%plot.ts(main="Taxa de Incidência Diária - Sul e Sudoeste de MG",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Sul e Sudoeste - MG" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stSdmInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Sul e Sudoeste de MG",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Sul e Sudoeste - MG" & b=="Mortalidade" & c=="Observado", stSdmMort%>%plot.ts(main="Taxa de Mortalidade Diária - Sul e Sudoeste de MG",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Sul e Sudoeste - MG" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stSdmMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Sul e Sudoeste de MG",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Sul e Sudoeste - MG" & b=="Letalidade"  & c=="Observado", stSdmLet%>%plot.ts(main="Taxa de Letalidade Diária - Sul e Sudoeste de MG",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Sul e Sudoeste - MG" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stSdmLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Sul e Sudoeste de MG",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),

`if`(a=="SP" & b=="Incidência"  & c=="Observado", stSPInc%>%plot.ts(main="Taxa de Incidência Diária - São Paulo",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="SP" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stSPInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - São Paulo",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="SP" & b=="Mortalidade" & c=="Observado", stSPMort%>%plot.ts(main="Taxa de Mortalidade Diária - São Paulo",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="SP" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stSPMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - São Paulo",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="SP" & b=="Letalidade"  & c=="Observado", stSPLet%>%plot.ts(main="Taxa de Letalidade Diária - São Paulo",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="SP" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stSPLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - São Paulo",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Oeste de Minas" & b=="Incidência"  & c=="Observado", stOdmInc%>%plot.ts(main="Taxa de Incidência Diária - Oeste de Minas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Oeste de Minas" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stOdmInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Oeste de Minas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Oeste de Minas" & b=="Mortalidade" & c=="Observado", stOdmMort%>%plot.ts(main="Taxa de Mortalidade Diária - Oeste de Minas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Oeste de Minas" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stOdmMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Oeste de Minas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Oeste de Minas" & b=="Letalidade"  & c=="Observado", stOdmLet%>%plot.ts(main="Taxa de Letalidade Diária - Oeste de Minas",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Oeste de Minas" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stOdmLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Oeste de Minas",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Metropolitana de BH" & b=="Incidência"  & c=="Observado", stMbhInc%>%plot.ts(main="Taxa de Incidência Diária - Metropolitana de BH",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Metropolitana de BH" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stMbhInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Metropolitana de BH",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Metropolitana de BH" & b=="Mortalidade" & c=="Observado", stMbhMort%>%plot.ts(main="Taxa de Mortalidade Diária - Metropolitana de BH",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Metropolitana de BH" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stMbhMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Metropolitana de BH",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Metropolitana de BH" & b=="Letalidade"  & c=="Observado", stMbhLet%>%plot.ts(main="Taxa de Letalidade Diária - Metropolitana de BH",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Metropolitana de BH" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stMbhLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Metropolitana de BH",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Campinas" & b=="Incidência"  & c=="Observado", stCamInc%>%plot.ts(main="Taxa de Incidência Diária - Campinas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Campinas" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stCamInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Campinas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Campinas" & b=="Mortalidade" & c=="Observado", stCamMort%>%plot.ts(main="Taxa de Mortalidade Diária - Campinas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Campinas" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stCamMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Campinas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Campinas" & b=="Letalidade"  & c=="Observado", stCamLet%>%plot.ts(main="Taxa de Letalidade Diária - Campinas",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Campinas" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stCamLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Campinas",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Ribeirão Preto" & b=="Incidência"  & c=="Observado", stRbpInc%>%plot.ts(main="Taxa de Incidência Diária - Ribeirão Preto",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Ribeirão Preto" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stRbpInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Ribeirão Preto",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Ribeirão Preto" & b=="Mortalidade" & c=="Observado", stRbpMort%>%plot.ts(main="Taxa de Mortalidade Diária - Ribeirão Preto",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Ribeirão Preto" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stRbpMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Ribeirão Preto",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Ribeirão Preto" & b=="Letalidade"  & c=="Observado", stRbpLet%>%plot.ts(main="Taxa de Letalidade Diária - Ribeirão Preto",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Ribeirão Preto" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stRbpLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Ribeirão Preto",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado")
))))))))))))))))))))))))))))))))))))))))))
}

funct2<-function(a,b,c)
{
`if`(a=="Macro Metropolitana Paulista" & b=="Incidência"  & c=="Observado", stMmpInc%>%plot.ts(main="Taxa de Incidência Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Macro Metropolitana Paulista" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stMmpInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Macro Metropolitana Paulista" & b=="Mortalidade" & c=="Observado", stMmpMort%>%plot.ts(main="Taxa de Mortalidade Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Macro Metropolitana Paulista" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stMmpMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Macro Metropolitana Paulista" & b=="Letalidade"  & c=="Observado", stMmpLet%>%plot.ts(main="Taxa de Letalidade Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Macro Metropolitana Paulista" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stMmpLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Macro Metropolitana Paulista",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Metropolitana de SP" & b=="Incidência"  & c=="Observado", stMspInc%>%plot.ts(main="Taxa de Incidência Diária - Metropolitana de SP",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Metropolitana de SP" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stMspInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Metropolitana de SP",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Metropolitana de SP" & b=="Mortalidade" & c=="Observado", stMspMort%>%plot.ts(main="Taxa de Mortalidade Diária - Metropolitana de SP",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Metropolitana de SP" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stMspMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Metropolitana de SP",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Metropolitana de SP" & b=="Letalidade"  & c=="Observado", stMspLet%>%plot.ts(main="Taxa de Letalidade Diária - Metropolitana de SP",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Metropolitana de SP" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stMspLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Metropolitana de SP",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Vale do Paraíba Paulista" & b=="Incidência"  & c=="Observado", stVppInc%>%plot.ts(main="Taxa de Incidência Diária - Vale do Paraíba Paulista",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Vale do Paraíba Paulista" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stVppInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Vale do Paraíba Paulista",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Vale do Paraíba Paulista" & b=="Mortalidade" & c=="Observado", stVppMort%>%plot.ts(main="Taxa de Mortalidade Diária - Vale do Paraíba Paulista",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Vale do Paraíba Paulista" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stVppMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Vale do Paraíba Paulista",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Vale do Paraíba Paulista" & b=="Letalidade"  & c=="Observado", stVppLet%>%plot.ts(main="Taxa de Letalidade Diária - Vale do Paraíba Paulista",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Vale do Paraíba Paulista" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stVppLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Vale do Paraíba Paulista",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="Piracicaba" & b=="Incidência"  & c=="Observado", stPirInc%>%plot.ts(main="Taxa de Incidência Diária - Piracicaba",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Piracicaba" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stPirInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Piracicaba",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Piracicaba" & b=="Mortalidade" & c=="Observado", stPirMort%>%plot.ts(main="Taxa de Mortalidade Diária - Piracicaba",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Piracicaba" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stPirMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Piracicaba",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Piracicaba" & b=="Letalidade"  & c=="Observado", stPirLet%>%plot.ts(main="Taxa de Letalidade Diária - Piracicaba",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="Piracicaba" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stPirLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - Piracicaba",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),       

`if`(a=="São José do Rio Preto" & b=="Incidência"  & c=="Observado", stSjrInc%>%plot.ts(main="Taxa de Incidência Diária - São José do Rio Preto",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="São José do Rio Preto" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stSjrInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - São José do Rio Preto",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="São José do Rio Preto" & b=="Mortalidade" & c=="Observado", stSjrMort%>%plot.ts(main="Taxa de Mortalidade Diária - São José do Rio Preto",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="São José do Rio Preto" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stSjrMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - São José do Rio Preto",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="São José do Rio Preto" & b=="Letalidade"  & c=="Observado", stSjrLet%>%plot.ts(main="Taxa de Letalidade Diária - São José do Rio Preto",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado"),
`if`(a=="São José do Rio Preto" & b=="Letalidade"  & c=="Tendência", decompose(na_kalman(stSjrLet))$trend%>%plot(main="Tendência da Taxa de Letalidade Diária - São José do Rio Preto",xlab="Semana Epidemiológica",ylab="Percentual de mortes por caso confirmado")       
                
))))))))))))))))))))))))))))))
}


funct3<-function(a,b,c)
{
`if`(a=="Alfenas" & b=="Incidência"  & c=="Observado", stAlfInc%>%plot.ts(main="Taxa de Incidência Diária - Alfenas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Alfenas" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stAlfInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Alfenas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Alfenas" & b=="Mortalidade" & c=="Observado", stAlfMort%>%plot.ts(main="Taxa de Mortalidade Diária - Alfenas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Alfenas" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stAlfMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Alfenas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Alfenas" & b=="Letalidade"  & c=="Observado", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),
`if`(a=="Alfenas" & b=="Letalidade"  & c=="Tendência", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),       

`if`(a=="Poços de Caldas" & b=="Incidência"  & c=="Observado", stPdcInc%>%plot.ts(main="Taxa de Incidência Diária - Poços de Caldas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Poços de Caldas" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stPdcInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Poços de Caldas",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Poços de Caldas" & b=="Mortalidade" & c=="Observado", stPdcMort%>%plot.ts(main="Taxa de Mortalidade Diária - Poços de Caldas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Poços de Caldas" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stPdcMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Poços de Caldas",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Poços de Caldas" & b=="Letalidade"  & c=="Observado", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),
`if`(a=="Poços de Caldas" & b=="Letalidade"  & c=="Tendência", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""), 

`if`(a=="Varginha" & b=="Incidência"  & c=="Observado", stVarInc%>%plot.ts(main="Taxa de Incidência Diária - Varginha",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Varginha" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stVarInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Varginha",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Varginha" & b=="Mortalidade" & c=="Observado", stVarMort%>%plot.ts(main="Taxa de Mortalidade Diária - Varginha",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Varginha" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stVarMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Varginha",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Varginha" & b=="Letalidade"  & c=="Observado", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),
`if`(a=="Varginha" & b=="Letalidade"  & c=="Tendência", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),       

`if`(a=="Campo das Vertentes" & b=="Incidência"  & c=="Observado", stCdvInc%>%plot.ts(main="Taxa de Incidência Diária - Campo das Vertentes",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Campo das Vertentes" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stCdvInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Campo das Vertentes",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Campo das Vertentes" & b=="Mortalidade" & c=="Observado", stCdvMort%>%plot.ts(main="Taxa de Mortalidade Diária - Campo das Vertentes",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Campo das Vertentes" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stCdvMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Campo das Vertentes",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Campo das Vertentes" & b=="Letalidade"  & c=="Observado", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),
`if`(a=="Campo das Vertentes" & b=="Letalidade"  & c=="Tendência", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),       

`if`(a=="Araraquara" & b=="Incidência"  & c=="Observado", stVarInc%>%plot.ts(main="Taxa de Incidência Diária - Araraquara",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Araraquara" & b=="Incidência"  & c=="Tendência", decompose(na_kalman(stVarInc))$trend%>%plot(main="Tendência da Taxa de Incidência Diária - Araraquara",xlab="Semana Epidemiológica",ylab="Nº de casos por 100 mil hab."),
`if`(a=="Araraquara" & b=="Mortalidade" & c=="Observado", stVarMort%>%plot.ts(main="Taxa de Mortalidade Diária - Araraquara",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."),
`if`(a=="Araraquara" & b=="Mortalidade" & c=="Tendência", decompose(na_kalman(stVarMort))$trend%>%plot(main="Tendência da Taxa de Mortalidade Diária - Araraquara",xlab="Semana Epidemiológica",ylab="Nº de mortes por 100 mil hab."), 
`if`(a=="Araraquara" & b=="Letalidade"  & c=="Observado", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab=""),
`if`(a=="Araraquara" & b=="Letalidade"  & c=="Tendência", plot(0,0,main="não é possível exibir o gráfico",axes = F,xlab = "",ylab="")       
                
))))))))))))))))))))))))))))))  
}

