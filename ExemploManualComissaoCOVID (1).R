


## Carregando o banco de dados

setwd("C:\\Users\\sandr\\Downloads")
Dados <- read.table("Tempo22maio.csv",
                    sep = ",",
                    header = T)
attach(Dados)
head(Dados)

## Taxas e series para o espaço geográfico mesorregião Sul e Sudoeste de Minas

# Criando o vetor de taxa de incidência diária

SdmInc <- SdmNewCases/135558*100000

# Criando o vetor de taxa de mortalidade por causa diária

SdmMort <- SdmNewDeaths/135558*100000

# criando o vetor de taxa de letalidade

SdmLet <- SdmMort/SdmInc*100

# criando objetos da classe séries temporais

stSdmInc <- ts(SdmInc,
               start = 9+3/7,
               end = 21+5/7,
               frequency = 7)

stSdmMort <- ts(SdmMort,
                start = 9+3/7,
                end = 21+5/7,
                frequency = 7)

stSdmLet <- ts(SdmLet,
               start = 14+4/7,
               end = 21+5/7,
               frequency = 7)

# observar o ajuste mo parâmetro 'start' da letalidade para a quarta-feira da 14a
# semana epidemiológica, pois é quando se iniciam a existir casos confirmados continuamente
# evitando denominadores iguais a zero

par(mfrow=c(3,1))
plot.ts(stSdmInc,xlim=c(0,100), ylim=c(0,100),main="Taxa de Incid�ncia di�ria")
plot.ts(stSdmMort,xlim=c(0,100), ylim=c(0,100) , main="Taxa de Mortalidade di�ria")
plot.ts(stSdmLet,xlim=c(0,100), ylim=c(0,100) , main="taxa de letalidade di�ria")
dev.off()

