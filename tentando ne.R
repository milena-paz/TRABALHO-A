source("tempsAnual.txt")
library(gganimate)
library(dplyr)
library(RColorBrewer)
tempsAnual[,1]<- as.factor(tempsAnual[,1])
temps<- unlist(tempsAnual[,2:13])
Tab<-data.frame(cbind(temps))
meses<-c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago",
         "Set","Out","Nov","Dez")
Tab$Mes <-factor(rep(meses,each=146),labels=meses)
Tab$Ano <- factor(rep(tempsAnual[,1],times=12))

grafico <-Tab|> ggplot(aes(x = Mes, y = temps, group = Ano, color = Ano)) +
    geom_line(show.legend = FALSE)+
    coord_radial(r_axis_inside = T)+
    xlab("Mês") +
    ylab("Temperatura (°C)")+
    theme(
      axis.text.r = element_text(face="bold")
    )
#PROBLEMA: tem um espaço separando dezembro e janeiro, queremos conectá-los para dar ideia de continuidade.
#POSSIVEL GAMBIARRA: o argumento de coord_radial expand permite que junte o primeiro e o ultimo pedaço
#da escala theta (y antes da transformacao), gerando:
grafico + coord_radial(expand=F)
#POREM nao queremos juntar janeiro com dezembro... queremos juntar janeiro com o PROXIMO janeiro
temps2<-append(temps,c(tempsAnual$Jan[2:146],NA))
Tab2<-data.frame(cbind(temps2))
Tab2$Mes <-factor(rep(c(meses,"pJan"),each=146),labels=c(meses,"pJan"))
Tab2$Ano <- factor(rep(tempsAnual[,1],times=13))

Tab2[Tab2$Ano==2014:2015,] |> ggplot(aes(x = Mes, y = temps2, group = Ano, color = Ano)) +
  geom_line(show.legend = FALSE)+
  coord_radial(r_axis_inside = T,expand=F)+
  xlab("Mês") +
  ylab("Temperatura (°C)")+
  theme(axis.text.r = element_text(face="bold"))
