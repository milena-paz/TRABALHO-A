##LENDO E ORGANIZANDO OS DADOS
#GLB.Ts+dSST.csv foi baixado na URL abaixo
#
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
library(gganimate) #carrega o ggplot2 automaticamente

#### Versão inicial ####
temps|> ggplot(aes(x=Mes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F) +
  coord_radial()
#temos alguns problemas com essa versao inicial:
# 1. os meses estao desorganizados (ordem alfabetica, que nao é o certo)
# 2. a posicao dos meses deve começar no meio-dia
# 3. as linhas nao se conectam

# PROBLEMA 1: ----
#para resolver isso teremos que organizar os meses na ordem certa
#um método seria transformar a variavel Mes em FATOR:
temps$Mes<- factor(temps$Mes, levels=meses)
str(temps$Mes)
#repetindo o mesmo codigo
ggplot(temps, aes(x=Mes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F) +
  coord_radial()
#resolvido!

# PROBLEMA 2: ----
#Para resolver isso é só usar um argumento de coord_radial(start)
ggplot(temps,aes(x=Mes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F) +
  coord_radial(start=pi/6)
#ta meio ruim de ler porque ficou sobreposto, depois mexemos nisso

# PROBLEMA 3: ----
# aqui vai precisar de uma gambiarra; porque queremos que o valor de dezembro
#conecte com o janeiro do próximo ano, para que seja uma linha contínua
proxJan <- dplyr::filter(temps,Mes == "Jan")
proxJan$Ano <- proxJan$Ano - 1
proxJan$Ano[1]<-NA
proxJan$Mes <- "proxJan"
t.dados <-rbind(temps,proxJan)
t.dados$numMes <- as.numeric(t.dados$Mes)
#ai a gente vai ter q fazer manualmente os rotulos e a escala
t.dados |> ggplot(aes(x=numMes,y=temp,group=Ano,color=Ano)) + #mudança no aes(x=)
  geom_line(show.legend = F) +
  scale_x_continuous(breaks=1:12,labels=meses,expand = c(0,0))+
  scale_y_continuous(limits = c(-2, 2.7),expand = c(0,0)) + #aproveitando pra deixar um espaço no centro
  coord_radial()
#com essa mudança, o argumento start nao e necessario para que o plot fique centrado na posicao meio-dia
#ok, agora ta tudo conectado! fonte da gambiarra: https://youtu.be/NYF9ySYSvwQ

#### Versão melhorada ####

# Escala
escala<- data.frame(x=c(1.55,1.37,1.35),xend=c(12.45,12.63,12.65),y=c(0,1.5,2),yend=c(0,1.5,2))
##
t.dados|> ggplot(aes(x=numMes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F,linewidth=0.8) + #linhas mais grossas
  scale_x_continuous(breaks=1:12,labels=meses,expand = c(0,0))+ 
  scale_y_continuous(limits = c(-1.5, 2.7),expand = c(0,0)) +
  coord_radial() +
  #mudando as cores para uma paleta divergente e colorblind-friendly
  #do pacote rColorBrewer
  scale_color_distiller(palette="RdYlBu") + 
  #desenha linhas indicando a escala (0, 1.5 e 2 graus celsius)
  geom_segment(data=escala,aes(x=x,xend=xend,y=y,yend=yend),
               color=c("slateblue4","aliceblue","aliceblue"),inherit.aes = F)+
  #texto para enumerar a escala
  annotate("text",x=1,y=c(1.5,2,0),label=c("1.5°C","2°C","0°C"),
           color=c("aliceblue","aliceblue","slateblue4"), size=4.5)+
  #remove os nomes dos eixos (nao faz sentido ter) e aqueles tracinhos da escala
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="#6C708F",color="#6C708F"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="white",size=18),
    axis.text.y = element_blank(),
    axis.title.x= element_blank(),
    axis.title.y = element_blank()
  )
#salva a forma estatica
ggsave("espiral.png",width=5,height=5,units="in",dpi=250)

#daqui em diante é só animar:
#### Versão final e animada ####
bleb<-t.dados|> ggplot(aes(x=numMes,y=temp,group=Ano,color=Ano)) +
  geom_line(show.legend = F,linewidth=.8) +
  scale_x_continuous(breaks=1:12,labels=meses,expand = c(0,0))+
  scale_y_continuous(limits = c(-2, 2.7),expand = c(0,0)) +
  coord_radial() +
  scale_color_distiller(palette="RdYlBu") +
  geom_segment(data=escala,aes(x=x,xend=xend,y=y,yend=yend),
               color=c("slateblue4","aliceblue","aliceblue"),inherit.aes = F)+
  annotate("text",x=1,y=c(1.5,2,0),label=c("1.5°C","2°C","0°C"),
           color=c("aliceblue","aliceblue","slateblue4"), size=4.5)+
  #escreve o ano no centro do vão que deixei no centro lá no PROBLEMA 3
  geom_label(aes(x = 12, y=-2, label = Ano),label.size=0, label.padding=unit(10,"pt"),
             size=6,color="white",fill="grey5")+
  theme(
    panel.background = element_rect(fill="grey5"),
    plot.background = element_rect(fill = "#6C708F",color="#6C708F"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x = element_text(color="white",size=18),
    axis.text.y = element_blank(),
    axis.title.x= element_blank(),
    axis.title.y = element_blank()
  )+
  transition_manual(frames = Ano, cumulative = T)

anim_save("espiral2.gif",animation=animate(bleb,height = 5, width = 5, units = "in",
                                           res = 250,bg = 'transparent',nframes=146,
                                           end_pause=20))

## Como a animação demora MUITO para renderizar, fazer isso em tempo real se torna inviável com a tecnologia usada
## Até onde vi, isso parece ser causado principalmente pelo geom_label() indicando o ano do frame atual
## Uma possível solução seria simplesmente não mostrar o ano atual no gráfico, mas isso retira uma importante
## informação da animação...