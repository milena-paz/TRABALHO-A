library(shiny)
library(gganimate)
source("pedacos.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cupinto"),

    sidebarLayout(
        sidebarPanel(
          markdown("#### Espiral climática
          
          A animação ao lado representa a mudança de temperatura global, entre
          os anos de 1880 e 2025. Fica evidente que, a partir da terceira revolução industrial,
          a temperatura global tem aumentado cada vez mais rapido; com grande destaque aos
          ultimos 4 anos que 
          
          Os dados foram obtidos
          [nesta página](https://science.nasa.gov/resource/video-climate-spiral-1880-2022/).
          ")
        ),

        mainPanel(
            img(src="espiral.gif", align = "left",height='500px',width='500px'),
        )
    ),
    br(),
    markdown('### Versão estática do gráfico'),
    
    plotOutput("espiral",inline=T)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
    output$espiral <- renderPlot({
      a<-t.dados|> ggplot(aes(x=numMes,y=temp,group=Ano,color=Ano)) +
        geom_line(show.legend = F,size=.8) +
        scale_x_continuous(breaks=1:12,labels=meses,expand = c(0,0))+
        scale_y_continuous(limits = c(-2, 2.7),expand = c(0,0)) +
        coord_radial(start=pi/6) +
        scale_color_distiller(palette="RdYlBu") +
        geom_hline(yintercept = c(2,1.5), color="white")+
        annotate("segment", x = c(11.7,11.7), xend = c(12.3,12.3), y=c(1.5,2), yend =c(1.5,2),colour = "grey5",size=2) +
        annotate("text",x=12,y=c(1.5,2),label=c("1.5°C","2°C"),color="white")+
        #geom_label(aes(x = 12, y=-2, label = Ano),label.size=0, label.padding=unit(10,"pt"),
        #           size=6,color="white",fill="grey5")+
        theme(
          panel.background = element_rect(fill="grey5", size=1),
          plot.background = element_rect(fill = "grey50",color="grey50"),
          panel.grid = element_blank(),
          axis.ticks = element_blank(), 
          axis.text.x = element_text(color="white",size=20),
          axis.text.y = element_blank(),
          axis.title.x= element_blank(),
          axis.title.y = element_blank()
        )
      a
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
