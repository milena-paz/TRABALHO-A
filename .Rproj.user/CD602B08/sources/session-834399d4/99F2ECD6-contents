##LENDO E ORGANIZANDO OS DADOS
#GLB.Ts+dSST.csv foi baixado na URL abaixo
# 
library(dplyr)
raws <- read.csv("GLB.Ts+dSST.csv", na="***",skip=1) |>
        subset(select=1:13)
#vamos usar os nomes dos meses em português
meses<-c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago",
         "Set","Out","Nov","Dez")
#para poder criar o grafico precisamos agrupar por ano e categorizar por mes:
temps<- data.frame(Ano=rep(raws$Year, times=12),
              Mes=rep(meses, each=146),
              temp=unlist(raws[-1]))
rownames(temps) <- NULL


#começaremos criando um grafico estático 
library(gganimate)

#### V1 ####
temps|> ggplot(aes(x=Mes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F) +
  coord_radial()
#temos alguns problemas com essa versao inicial:
# 1. os meses estao desorganizados (ordem alfabetica, que nao é o certo)
# 2. a posicao dos meses deve começar no meio-dia
# 3. as linhas nao se conectam

#PROBLEMA 1:
#para resolver isso teremos que organizar os meses na ordem certa
#um método seria transformar a variavel Mes em FATOR:
temps$Mes<- factor(temps$Mes, levels=meses)
str(temps$Mes)
#repetindo o mesmo codigo
ggplot(temps, aes(x=Mes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F) +
  coord_radial()
#resolvido!

#PROBLEMA 2:
#Para resolver isso é só usar um argumento de coord_radial(start)
ggplot(temps,aes(x=Mes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F) +
  coord_radial(start=pi/6)
#ta meio ruim de ler porque ficou sobreposto, depois mexemos nisso

#PROBLEMA 3:
# aqui vai precisar de uma gambiarra; porque queremos que o valor de dezembro
#conecte com o janeiro do próximo ano, para que seja uma linha contínua
proxJan <- filter(temps,Mes == "Jan")
proxJan$Ano <- proxJan$Ano - 1
proxJan$Ano[1]<-NA
proxJan$Mes <- "proxJan"
t.dados <-rbind(temps,proxJan)
t.dados$numMes <- as.numeric(t.dados$Mes)
#ai a gente vai ter q fazer manualmente os rotulos e a escala x_x
t.dados |> ggplot(aes(x=numMes,y=temp,group=Ano,color=Ano)) + #mudança no aes(x=)
  geom_line(show.legend = F) +
  scale_x_continuous(breaks=1:12,labels=meses,expand = c(0,0))+
  scale_y_continuous(limits = c(-2, 2.7),expand = c(0,0)) + #aproveitando pra deixar um espaço no centro
  coord_radial(start=pi/6)
#ok, agora ta tudo conectado! fonte da gambiarra: https://youtu.be/NYF9ySYSvwQ


#daqui em diante é só mudar a estética do plot para que fique melhor de visualizar:
bleb<-t.dados|> ggplot(aes(x=numMes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F,size=.8) +
  scale_x_continuous(breaks=1:12,labels=meses,expand = c(0,0))+
  scale_y_continuous(limits = c(-2, 2.7),expand = c(0,0)) +
  coord_radial(start=pi/6) +
  scale_color_distiller(palette="RdYlBu") +
  geom_hline(yintercept = c(2,1.5), color="white")+
  annotate("segment", x = c(11.7,11.7), xend = c(12.3,12.3), y=c(1.5,2), yend =c(1.5,2),colour = "grey5",size=2) +
  annotate("text",x=12,y=c(1.5,2),label=c("1.5°C","2°C"),color="white")+
  geom_label(aes(x = 12, y=-2, label = Ano),label.size=0, label.padding=unit(10,"pt"),
             size=6,color="white",fill="grey5")+
  theme(
    panel.background = element_rect(fill="grey5", size=1),
    plot.background = element_rect(fill = "grey50",color="grey50"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x = element_text(color="white",size=20),
    axis.text.y = element_blank(),
    axis.title.x= element_blank(),
    axis.title.y = element_blank()
  )+
  transition_manual(frames = Ano, cumulative = T)

anim_save("espiral.gif",animation=animate(bleb,height = 5, width = 5, units = "in", res = 250))
