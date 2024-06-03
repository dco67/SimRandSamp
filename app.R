

#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(
#  shiny, 
#  shinyWidgets, 
#  shinycssloaders,
#  DT, 
#  dplyr, 
#  ggplot2, 
#  ggstatsplot, 
#  BayesFactor, 
 # ggpubr,
#  baymedr)

 library(shiny)
 library(shinyWidgets)
 library(DT)
 library(dplyr)
 library(ggplot2)
 library(ggstatsplot)
 library(BayesFactor)
 library(ggpubr)

source("support.R")

ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  titlePanel("Simulation d'une recherche quantitative"),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, 
               actionBttn(inputId = "rst",
                          label = "Réinit",
                          style = "material-flat",
                          color = "danger")
               )
        ),
      titlePanel(h3(strong("Populations"), align = "center")),
      helpText(h4("Les moyennes des deux populations, $\\mu_1$ et $\\mu_2$, sont inconnues. Dans cette recherche, ",
                  "on espère en apprendre un peu au sujet de la différence qui pourrait exister entre elles.")),
      fluidRow(
        column(6, offset = 6,
               materialSwitch(inputId = "showpop", label = "Params", status = "info", value = FALSE, right = TRUE),
               )
        ),
      fluidRow(
        titlePanel(h3(strong("Échantillons"), align = "center")),
        column(6, 
               numericInput(inputId = "n1", 
                            label = strong("Garçons $$(n_1):$$"),
                            min=5, 
                            max=200, 
                            value=25, 
                            step=1)),
        column(6,
               numericInput(inputId = "n2", 
                            label = strong("Filles $$(n_2):$$"),
                            min=5, 
                            max=200, 
                            value=25, 
                            step=1)
               )
        ),
      fluidRow(
        column(6,
               actionButton("go", 
                            "Échantillonner", 
                            icon("paper-plane"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
        column(6, 
               materialSwitch(inputId = "interpret", 
                              label = "Interprétation", 
                              status = "info", 
                              value = FALSE, 
                              right = TRUE, 
                              inline = TRUE)
               )
        ),
      p(),
      
      wellPanel(style = "background: lightblue",
                fluidRow(
                  column(4,
                         a(h4("Par Daniel Coulombe, Ph.D.")),
                         p("2022")
                  ),
                  column(4,
                         tags$a(
                           href="https://isteah.org", 
                           tags$img(src="ISTEAH_LOGO.png", 
                                    title="ISTEAH", 
                                    width="160",
                                    height="140")
                         )
                  )
                )
      )
      ),  
    
    mainPanel(
      tabsetPanel( 
        tabPanel(strong(h4("Contexte")),
                 helpText(
                   h3("Un chercheur de l'ISTEAH s'intéresse à la performance des élèves du Lycée en Sciences. ",
                      "Plus particulièrement, il tente d'examiner la différence qui pourrait exister entre la performance ",
                      "des garçons dans un examen portant sur les Sciences en général, et celle des filles au même examen. ",
                      "Au moment d'effectuer cette recherche, le chercheur ne dispose d'aucune information concernant la direction ",
                      "que pourrait prendre une telle différence.", p(),
                      p(strong("Est-ce qu'au niveau de la population, la performance des filles est différente de celle des garçons?"))
                      ) 
                 ),
                 p(),
                 helpText(
                   h3("NOTE: la question de recherche s'adresse à l'ensemble de la population des lycéens et lycéennes du pays! ",
                   "La nature de la recherche est ", strong("quantitative!")
                   )
                 ),
                 p(),
                 p(h3("Pour répondre à cette question, il forme des échantillons aléatoires de $n_1$ lycéens et de $n_2$ lycéennes, ",
                      "prenant note de la performance de chacun et chacune à l'examen du MENFP.")
                   ),
        p(),
        wellPanel(
          uiOutput("directions")
          )
        ),

      tabPanel(strong(h4("Observations")),
               fluidRow(
                 column(8, offset=2,
                        h2("Échantillons:"),
                        dataTableOutput("Dat")
                        )
                 ),
               p(),
               uiOutput("obsint"),
               p(),
               plotOutput("showpopgrf")
               ),
      
      tabPanel(strong(h4("Description")),
               plotOutput("graf1"),
#               plotOutput("graf1b"),
               wellPanel(style = "background: bisque2",
                         dataTableOutput("descript"),
                         dataTableOutput("showpop"),
                         uiOutput("descint")
                         )
               ),
      navbarMenu(
        title = strong(h4("Analyse")),
        tabPanel(strong(h4("Réplications")),
                 plotOutput("graf2")|> 
                   shinycssloaders::withSpinner(
                     type = 1, 
                     color.background = "white"
                   ), 
                 
                 uiOutput("simint2")
                 ),
        
        tabPanel(strong(h4("Centre: Différence Observée")),
                 plotOutput("graf4"),
                 uiOutput("simint4")
                 ),
        
        tabPanel(strong(h4("Centre: Différence Nulle")),
                 plotOutput("graf3"),
                 uiOutput("simint")
                 ),
        
        tabPanel(strong(h4("Test Paramétrique")),
                 uiOutput("ttest"),
                 uiOutput("ttestint"),
                 plotOutput("ttestgrf"))
                 ),
      
      tabPanel(strong(h4("Conclusion")),
               h3("Résumé des Observations"),
               tableOutput(outputId = "descript2"),
               tags$hr(style="border-color: purple;"),
               p(),
               p(),
               plotOutput("compplot"),
               p(),
               uiOutput("showt"),
               ),
      )
    )
  )
)

server <- function(input, output, session) {
  
  parms <- reactiveValues(
    k = format(100000, scientific = FALSE),
    mu1 = runif(1, 40, 60),
    mu2 = runif(1, 45, 65),
    sig1 = runif(1, 10, 20),
#    sig2 = sig1() #runif(1, 10, 20)
    )
  
  Data <- eventReactive(input$go,{
    SimData(mu1=parms$mu1,
            mu2=parms$mu2,
            sig1=parms$sig1, 
            sig2=parms$sig1, 
            input$n1, 
            input$n2)})
  
  dat1 <- reactive(Data()$Y[Data()$Groupe == 0])
  dat2 <- reactive(Data()$Y[Data()$Groupe == 1]) 
  m1 <- reactive(mean(dat1()))
  m2 <- reactive(mean(dat2()))
  s1 <- reactive(sd(dat1()))
  s2 <- reactive(sd(dat2()))
  sc <- reactive(sqrt(((input$n1 - 1) * s1()^2 + (input$n2 - 1) * s2()^2) / (input$n1 + input$n2 -2)))
  effsize <- reactive((m1() - m2()) / sc())
  mdiff <- reactive(m1() - m2())

  ttst <- reactive({t.test(dat1(), 
                          dat2(), 
                          alternative = "two.sided", 
                          mu=0, 
                          paired = FALSE, 
                          var.equal = FALSE, 
                          conf.level = 0.95)
                   })
  ttstBF <- reactive({
    extractBF(ttestBF(dat1(), dat2()))
  })

  ttstBFint <- reactive({
    if(ttstBF()$bf >= 100) {"évidence extrême pour $H_1$"}
    else if(ttstBF()$bf >= 30 & ttstBF()$bf < 100) {"évidence très élevée pour $H_1$"}
    else if(ttstBF()$bf >= 10 & ttstBF()$bf < 30) {"évidence élevée pour $H_1$"}
    else if(ttstBF()$bf >= 3 & ttstBF()$bf < 10) {"évidence modérée pour $H_1$"}
    else if(ttstBF()$bf > 1 & ttstBF()$bf < 3) {"évidence anectodique pour $H_1$"}
    else if(ttstBF()$bf == 1) {"aucune évidence, ni pour $H_1$, ni pour $H_0$"}
    else if(ttstBF()$bf >= 1/3 & ttstBF()$bf < 1) {"évidence anecdotique pour $H_0$"}
    else if(ttstBF()$bf >= 0.1 & ttstBF()$bf < 1/3) {"évidence modérée pour $H_0$"}
    else if(ttstBF()$bf >= 1/30 & ttstBF()$bf < 0.1) {"évidence élevée pour $H_0$"}
    else if(ttstBF()$bf > 0.01 & ttstBF()$bf < 1/30) {"évidence très élevée pour $H_0$"}  
    else if(ttstBF()$bf < 0.01) {"évidence extrême pour $H_0$"} 
  })
  
  moydiff <- reactive(SimDistr(k=parms$k, 
                               mu1=parms$mu1, 
                               mu2=parms$mu2, 
                               sig1=parms$sig1, 
                               sig2=parms$sig1, # parms$sig2 pour inégalité
                               n1=input$n1, 
                               n2=input$n2))
  mdiffctr <- reactive(m1() - m2() - mean(moydiff()$DiffCtr))
  mdiffctr2 <- reactive(moydiff()$DiffCtr + mdiff())
  intc <- reactive(quantile(moydiff()$Diff, c(0.025, 0.975)))
  intc2 <- reactive(quantile(moydiff()$DiffCtr, c(0.025, 0.975)))
  intc3 <- reactive(quantile(mdiffctr2(), c(0.025, 0.975)))

  inputVal_Reactive <- reactive({
    pmean1 <- parms$mu1
    pmean2 <- parms$mu2
    
    psd <- parms$sig1
    
    n1 <- input$n1
    n2 <- input$n2
    
    p <- 0.05  # user probability
    p_tail <- "both"
    
    fn_InputData(pmean1 = pmean1, pmean2 = pmean2,
                 psd = psd,
                 n1 = n1, n2 = n2,
                 p = p, p_tail = p_tail)
    
  })
  
  
  observeEvent(input$rst, {
    session$reload()
  })

  output$directions <- renderUI({
    
  txt <- c(paste0("1. Inscrivez la taille des échantillons dans les cases appropriées du tableau de bord;"),
           paste0("2. Cliquez sur le bouton libellé ", strong("Échantillonner"), ", pour simuler l'échantillonnage;"),
           paste0("3. Examinez les observations en visitant l'onglet ", strong("Observations")),
           paste0("4. Obtenez une description des données en visitant l'onglet ", strong("Description"), br(),
                  "   a. L'interrupteur libellé ", strong("Interprétation"), " fournit détails et interprétation des tableaux et graphiques qui sont produits;", br(),
                  "   b. L'interrupteur libellé ", strong("Paramètres"), " force l'affichage des paramètres des populations, pour comparaisons;"),
           paste0("5. Générez la distribution des différences de moyennes en visitant l'onglet ", strong("Analyse"), ". ", br(),
                  "a. Sélectionnez ", strong("Réplications"), " pour la distribution non-centrée", br(),
                  "b. Sélectionnez ", strong("Centre: Différence Observée"), " pour afficher la distribution centrée sur la différence observée", br(),
                  "c. Sélectionnez ", strong("Centre: Différence Nulle"), " pour afficher la distribution supposant l'égalité des moyennes", br(),
                  "d. Afficher la position du paramètre de la population, i.e. la différence entre les moyennes au niveau des populations, en activant l'interrupteur libellé ", strong("Paramètres")),
           paste0("6. Sous l'onglet libellé ", strong("Conclusion"), ", examinez la forme générale des conclusions que l'on peut inférer à partir des observations."),
           paste0("7. Pour ré-initialiser. ou recommencer à partir de nouvelles populations, cliquez sur le bouton libellé ", strong("Réinit")))
  txt <- paste(paste0(txt, "\n"), collapse = "")
  
    helpText(
      h3(strong(
        "Utilisation de l'application: ")), p(),

        HTML(markdown::renderMarkdown(text= txt)
        )
      )
  }) 
  
  output$Dat <- renderDataTable({
    DT::datatable(round(Data(), 3), 
                  colnames=c("Groupe", "Y"),
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;',
                    htmltools::em(h4("Observations effectuées auprès de deux échantillons indépendants"))),
                  class = "cell-border stripe",
                  options = list(
                    info = TRUE,
                    paging = TRUE,
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '100px', targets = "_all"))
                    )
                  ) 
    })
  
  output$obsint <- renderUI({
    if(input$interpret == 0){return(NULL)}
    
    txt <- c(paste0("  1. ", strong("Revue le la littérature:"), " que sait-on sur le sujet traité?"),
             paste0("  2. ", strong("Question de recherche"), ": quel(s) élément(s) de connaissance font défaut?"),
             paste0("  3. ", strong("Développement d'un protocole"), ": quelle procédure nous permettra d'acquérir les connaissances manquantes? Quelles conditions doit-on créer pour pouvoir détecter une différence, si elle existe bien au niveau de la population?"),
             paste0("  4. ", strong("Définition des variables à mesurer"), ": comment peut-on mesurer de manière valide et fiable les caractéristiques étudiées? "),
             paste0("  5. ", strong("Taille des échantillons"), ": combien d'observations sont nécessaires pour nous permettre des conclusions valides? "),
             paste0("  6. ", strong("Application du protocole"), ":  collecte des données, suivant le protocole. "),
             paste0("  7. ... ET ENSUITE?"))
    txt <- paste(paste0(txt, "\n"), collapse = "")
    
    withMathJax(
      helpText(
        h3(
          "Ces données proviennent d'une population dont les paramètres sont inconnus. Elle sont ",
          "sélectionnées aléatoirement et forment deux échantillons de tailles $n_1=$", input$n1, " et $n_2=$", input$n2, ", respectivement. ", p(),
          "Ces observations sont le résultat d'un processus séquentiel:", p(),
          HTML(markdown::renderMarkdown(text= txt)
          )
        )
      )
    )
    
  })

  output$showpop <- renderDataTable({
    if(input$showpop == 0){return(NULL)}
    tbl <- rbind(c("Moyenne", round(parms$mu1, 3), round(parms$mu2,3), round(parms$mu1-parms$mu2, 3)),
                 c("Écart-Type", round(parms$sig1,3), round(parms$sig1, 3), " "))
    DT::datatable(tbl, 
                  colnames=c(" ", "Garçons", "Filles", "Différence"), 
                  rownames=FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center;',
                    htmltools::em(h3("Paramètres des populations"))),
                  class = "cell-border stripe",
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '20%', targets = "_all"))
                    )
                  )
    })
  
  output$showpopgrf <- renderPlot({
    if(input$showpop == 0){return(NULL)}
    fn_dnorm(parms$mu1, parms$mu2, parms$sig1, dat1(), dat2())
  })
  
  output$graf1 <- renderPlot({
    
    Groupe <- Data()$Groupe
    Grp <- ifelse(Groupe == 0, "Garçons", "Filles")
    Y <- Data()$Y
    plt2 <- Data() %>%
#      ggplot(Data, mapping = aes(x=factor(Grp), y=Y, fill=factor(Grp))) +
#      coord_flip() +
#      geom_jitter(width=0.2, color="darkblue", size=2) +
#      stat_boxplot(geom = "errorbar", width = 0.1) +
#      geom_boxplot(alpha=0.6, 
#                   width=0.5, 
#                   outlier.colour="red", 
#                   outlier.shape=19, 
#                   outlier.size=3) +
#      labs(fill="Groupe",
#           y="Performance en Sciences",
#           x="Groupe") +
#      stat_summary(fun="mean", shape=19, col='red', geom='point', size=4) +
#      theme(legend.text=element_text(size=20),
#            legend.key.size = unit(1, 'cm'),
#            legend.title = element_text(size=25),
#            axis.text=element_text(size=12),
#            axis.title=element_text(size=14,face="bold")) 
#    plt2
    
    ggplot(mapping = aes(x=factor(Grp), y=Y, fill=factor(Grp))) +
      
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .4, 
        justification = -.3, 
        .width = 0, 
        point_colour = "NA"
      ) + 
      geom_boxplot(
        width = .15, 
        outlier.color = NA ## `outlier.shape = NA` works as well
      ) +
#      ggdist::stat_dots(
#        side = "left", 
#        justification = 1.2, 
#        binwidth = .25,
#        dotsize=0.2
#      ) + 
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) +
      coord_cartesian(xlim = c(1.2, NA)) +
      labs(fill="Groupe",
                      y="Performance en Sciences",
                      x="Groupe") +
                 stat_summary(fun="mean", shape=19, col='red', geom='point', size=4) +
                 theme(legend.text=element_text(size=20),
                       legend.key.size = unit(1, 'cm'),
                       legend.title = element_text(size=25),
                       axis.text=element_text(size=12),
                       axis.title=element_text(size=14,face="bold"))
    plt2
    
 })
  
  output$descript <- renderDataTable({
    
    tbl <- rbind(c("n", input$n1, input$n2, input$n1 + input$n2, " "),
                 c("Moyenne", round(m1(), 3), round(m2(), 3), round(mean(Data()$Y), 3), round(m1()-m2(), 3)),
                 c("Médiane", round(median(dat1()), 3), round(median(dat2()), 3), round(median(Data()$Y, 3)), round(median(dat1())-median(dat2()), 3)),
                 c("É-T", round(s1(), 3), round(s2(), 3), round(sd(Data()$Y), 3), " "),
                 c("ÉIQ", round(IQR(dat1()), 3), round(IQR(dat2()), 3), round(IQR(Data()$Y), 3), " ")) 
    DT::datatable(tbl, 
                  colnames=c(" ", "Garçons", "Filles", "Total", "Différence"), 
                  rownames=FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center;',
                    htmltools::em(h3("Résumé des Observations"))),
                  class = "cell-border stripe",
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '200px', targets = "_all"))
                    )
                  )
    })
  
  output$descint <- renderUI({
    if(input$interpret == 0){return(NULL)}
    withMathJax(
      helpText(
        h3(
          "L'examen des observations recueillies auprès des deux échantillons indique que la performance moyenne des garçons à l'examen de Sciences du MENFP est ",
          ifelse(m1() > m2(), "supérieure ", "inférieure "), 
          "à celle des filles, la différence étant de ", round(abs(mdiff()), 3), " points.", p(),
          "Si, au niveau de la population, la différence $|\\mu_{Garçons}-\\mu_{Filles}|$ est supérieure à 0, la différence observée, $(\\bar {X}_{Garçons}-\\bar{X}_{Filles})$, qui en est une estimation ponctuelle,  ",
          "aura tendance à s'en approcher, tout en s'éloignant de 0.", p(),
          "À ce stade, la question à laquelle on doit trouver réponse est: ", p(),
          strong("Est-ce que, en terme de probabilité, cette différence observée est un reflet d'une différence réelle au niveau de la population?"), p(),
          "Répondre à cette question conduira, si les conditions d'application sont respectées, à la ", strong("découverte"),
          "ou à la génération de connaissances nouvelles, ce qui est précisément l'objectif ultime de la Science. Pour y parvenir, il est nécessaire",
          "d'appliquer une méthode d'analyse ", strong("inférentielle "), "des observations."
        )
      )
    )
  })
  
  output$graf1b <- renderPlot({
    
    Groupe <- Data()$Groupe
    Grp <- ifelse(Groupe == 0, "Garçons", "Filles")
    Y <- Data()$Y
    plt2 <- Data() %>%
      ggplot(Data, mapping = aes(x=factor(Grp), y=Y, fill=factor(Grp))) +
      coord_flip() +
      geom_violin(trim=FALSE) +
      geom_boxplot(width=0.1) +
      labs(fill="Groupe",
           y="Performance en Sciences",
           x="Groupe") +
      stat_summary(fun="mean", shape=19, col='red', geom='point', size=4) +
      theme(legend.text=element_text(size=20),
            legend.key.size = unit(1, 'cm'),
            legend.title = element_text(size=25),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) 
    plt2
    
  })
  
  output$graf2 <- renderPlot({
    
    gr1 <- hist(moydiff()$Diff, breaks="FD", plot=FALSE)
    cuts <- cut(gr1$breaks, c(-Inf, intc()[1], intc()[2], Inf))
    cols <- c("red", "cadetblue1", "red")
    plot(gr1,
         xlab="Différences de moyennes",
         ylab="Densité",
         freq=FALSE,
         main="Distribution des différences de moyennes",
         col=cols[cuts])
    abline(v=m1() - m2(),
           col="red",
           lwd=2,
           lty=2)
    if(input$showpop == 0){
      lgd1 <- "Différence observée"
      lgd2 <- "red"
      lgd3 <- 2
      } else {
    abline(v=parms$mu1-parms$mu2, 
           col="darkgreen",
           lwd=2)
      lgd1 <- c("Différence observée", "Différence réelle")
      lgd2 <- c("red", "darkgreen")
      lgd3 <- c(2, 1)
      }
    legend("topright", 
           legend=lgd1, 
           col=lgd2, 
           lty=lgd3, 
           cex=1.2)
  })
  
  output$simint2 <- renderUI({
    if(input$interpret == 0){return(NULL)}
    withMathJax(
      helpText(
        titlePanel("Distribution des différences de moyennes:"),
        h3(
          "La recherche a été répétée ", parms$k, " fois, ce qui a permis d'obtenir ", parms$k,
          " différences de moyennes. L'histogramme ci-dessus illustre la distribution de ces différences. La moyenne de cette distribution, ",
          "étant formée à partir d'un très grand nombre d'échantillons tirés des mêmes populations, est à la limite égale à la véritable différence existant entre les moyennes de ces populations. ", 
          "Malheureusement, lors d'une recherche, nous ne disposons que d'", strong("un seul "), "échantillon pour chacune des deux populations, donnant lieu à ", strong("une seule différence. "), 
          "En répétant l'expérience, on obtiendra différentes valeurs de $(\\bar{X}_{Filles} - \\bar{X}_{Garçons})$ qui graviteront autour de $(\\mu_{Filles} - \\mu_{Garçons})$. ", p(),
          "La question qui nous vient à l'esprit, à ce stade, est: ", p(),
          strong("Quelle est la position de la différence observée, par rapport à une différence hypothétiquement nulle, ou par rapport à toute autre différence réelle entre les moyennes des populations?"), p(),
          
          "Cette question est cruciale lors de l'analyse des données, et rien dans une analyse descriptive ne nous permet d'y répondre.  ", p(),
          " La différence observée pour les échantillons initiaux, $(\\bar{X}_{Filles} - \\bar{X}_{Garçons}) = $ ", round(mdiff(), 3),
          ", est représentée par la ligne pointillée verticale rouge. C'est le seul élément que l'on obtient de l'analyse descriptive. ", p(),
          
          "La ligne verticale verte, qui apparait lorsque le bouton ", 
          strong("Paramètres"), " est activé, représente la moyenne de la distribution des différences qui, pour un nombre ",
          "infini d'expériences, est égale à la véritable différence entre les moyennes au niveau de la population. C'est de cette quantité que la différence observée est une estimation ponctuelle. ", p(), 

          "Les extrémités de la distribution $($en rouge$)$,",
          "indiquent les différences que l'on pourrait juger improbables ",
          "entre les moyennes des populations d'où sont tirés les échantillons. Le centre de la ",
          "distribution regroupe les différences que l'on considère comme probables, dans les mêmes conditions.  Donc, ",
          "en tirant un échantillon de chacune des populations, on a de fortes chances que la différence entre leurs moyennes se trouve dans la région centrale, et peu de chance qu'elle se trouve dans l'une ou l'autre des extrémités.  ", p(),
          )
        )
      )
    })
  
  output$graf3 <- renderPlot({
    
    gr1 <- hist(moydiff()$DiffCtr, breaks="FD", plot=FALSE)
    cuts <- cut(gr1$breaks, c(-Inf, intc2()[1], intc2()[2], Inf))
    cols <- c("red", "cadetblue1", "red")
    plot(gr1,
         xlab="Différences de moyennes",
         ylab="Densité",
         freq=FALSE,
         main="Distribution des différences de moyennes \n Centrées sur une différence nulle",
         col=cols[cuts])
    abline(v=m1() - m2(),
           col="red",
           lwd=2,
           lty=2)
    if(input$showpop == 0){
      lgd1 <- "Différence observée"
      lgd2 <- "red"
      lgd3 <- 2
    } else {
      abline(v=parms$mu1 - parms$mu2, 
             col="darkgreen",
             lwd=2)
      lgd1 <- c("Différence observée", "Différence réelle")
      lgd2 <- c("red", "darkgreen")
      lgd3 <- c(2, 1)
    }
    legend("topright", 
           legend=lgd1, 
           col=lgd2, 
           lty=lgd3, 
           cex=1.2)
  })
  
  output$graf4 <- renderPlot({
    
    gr1 <- hist(mdiffctr2(), breaks="FD", plot=FALSE)
    cuts <- cut(gr1$breaks, c(-Inf, intc3()[1], intc3()[2], Inf))
    cols <- c("red", "cadetblue1", "red")
    plot(gr1,
         xlab="Différences de moyennes",
         ylab="Densité",
         freq=FALSE,
         main="Distribution des différences de moyennes \n centrées sur la différence observée",
         col=cols[cuts])
    abline(v=m1() - m2(),
           col="red",
           lwd=2,
           lty=2)
    if(input$showpop == 0){
      lgd1 <- "Différence observée"
      lgd2 <- "red"
      lgd3 <- 2
    } else {
      abline(v=parms$mu1-parms$mu2, 
             col="darkgreen",
             lwd=2)
      lgd1 <- c("Différence observée", "Différence réelle")
      lgd2 <- c("red", "darkgreen")
      lgd3 <- c(2, 1)
    }
    legend("topright", 
           legend=lgd1, 
           col=lgd2, 
           lty=lgd3, 
           cex=1.2)
    arrows(intc3()[1], 0.003, 
           intc3()[2], 0.003,
           col="darkgreen",
           lwd = 2,
           length = 0.25,
           code = 3)
  })
  
  output$simint4 <- renderUI({
    if(input$interpret == 0){return(NULL)}
    withMathJax(
      helpText(
        titlePanel("Distribution centrée sur la différence observée:"),
        h3(
          "Puisque la différence observée est une estimation de la différence réelle au niveau de la population, sa valeur attendue est cette dernière: ", p(),
          "$$E(\\bar{X}_{Garçons}-\\bar{X}_{Filles})=\\mu_{Garçons}-\\mu_{Filles}$$", p(),
          "En centrant la distribution des différences sur la différence observée, on obtient l'histogramme ci-dessus. La moyenne de cette distribution est ",
          
          "$\\bar{X}_{Garçons}-\\bar{X}_{Filles})=$", round(mdiff(), 3), ". Dès lors, il devient possible de déterminer un intervalle regroupant les valeurs plausibles de la véritable différence au niveau de la population.",
          "Cet intervalle est indiqué par la flèche bidirectionnelle, dans l'histogramme. La probabilité que cet intervalle capture la différence des moyennes au niveau de la population peut s'obtenir aisément. On pourra donc affirmer qu'en ",
          "répétant l'expérience un très grand nombre de fois, la véritable différence entre les moyennes des populations serait comprise parmi les valeurs regroupées dans cet intervalle. ", p(),
          
          "Il est intéressant de noter que si l'intervalle décrit ci-dessus exclut une différence nulle, il y a de fortes chances que la différence observée réflète une différence réelle au niveau des populations. ", p(),
          p(),
          "Ce qu'il faut considérer ici, c'est le fait que la différence observée est une estimation de la différence réelle, et elle peut prendre ",
          "n'importe quelle valeur, sous cette distribution.  Le problème est alors de déterminer quelles valeurs sont probables, et lesquelles ne le sont pas, ", strong("sur la base des observations dont nous disposons."), 
          "Si on traite la différence observée comme si elle était égale à la différence réelle, on peut visualiser l'ensemble des valeurs probables du paramètre de la population.  ",
          "Ce travail relève de l'", strong("estimation statistique"), ", une composante essentielle de l'analyse de données quantitatives."
          )
        )
      )
    })
  
  output$simint <- renderUI({
    if(input$interpret == 0){return(NULL)}
    withMathJax(
      helpText(
        titlePanel("Distribution centrée sur une différence nulle:"),
        h3(
          "Considérons la possibilité qu'au niveau de la population, il n'y ait aucune différence entre les performances moyennes des garçons et des filles. La recherche a été répétée ", parms$k, " fois, ce qui a permis d'obtenir ", parms$k,
          " différences de moyennes, que l'on centre par rapport à la moyenne de la distribution $(\\bar{X_1}-\\bar{X_2}-\\mu_{\\bar{X_1}-\\bar{X_2}})$. L'histogramme ci-dessus illustre la distribution de ces différences centrées, dont la moyenne devient nulle. On obtient ",
          "ainsi une distribution conforme à l'idée qu'au niveau des populations, les moyennes sont égales. ", p(),
          "Si vraiment $\\mu_{Garçons}-\\mu_{Filles}=0$, ", strong("quelle est la probabilité d'obtenir la différence de moyennes observée entre les échantillons, ou une différence encore plus extrême"), "?", p(),
          " La différence observée pour les échantillons initiaux, $(\\bar{X}_{Garçons} - \\bar{X}_{Filles}) = $ ", round(m1() - m2() , 3),
          ", est représentée par la ligne pointillée verticale rouge. La ligne verticale verte, qui apparait lorsque le bouton ", strong("Paramètres"), " est activé, représente la moyenne de la distribution des différences qui, pour un nombre ",
          "infini d'expériences, est égale à la véritable différence entre les moyennes au niveau de la population.  ", p(), 
          "Les extrémités de la distribution $($en rouge$)$,",
          "indiquent les différences que l'on pourrait juger improbables, ", strong("s'il n'y avait aucune différence entre les moyennes des populations d'où sont tirés les échantillons"), ". Le centre de la ",
          "distribution, qui s'étend de ", round(intc2()[1], 3), " à ", round(intc2()[2], 3), ", regroupe les différences que l'on considère comme probables, dans les mêmes conditions.", p(),
          "Dans le cas présent, la différence observée entre les moyennes des échantillons initiaux, $\\bar{X}_{Garçons}-\\bar{X}_{Filles}=$", round(mdiff(), 3), " se trouve ",
          ifelse(mdiff() > intc2()[1] & mdiff() < intc2()[2], "dans la région centrale ", "dans une des extrémités "),
          "de la distribution. De ce fait, on peut conclure que cette différence ", 
          ifelse(mdiff() > intc2()[1] & mdiff() < intc2()[2], "n'est pas ", "est "),
          "statistiquement significative. Elle ", 
          ifelse(mdiff() > intc2()[2] | mdiff() > intc2()[1], "est ", "n'est pas "), 
          "compatible avec l'idée que les performances moyennes des garçons et des filles sont égales, puisque la probabilité d'obtenir une telle différence, supposant l'égalité des moyennes au niveau de la population, est ",
          ifelse(mdiff() > intc2()[2] | mdiff() > intc2()[1], "grande.", "faible."), p(),
          "L'analyse que l'on vient de faire des observations obtenues des échantillons initiaux est une ", strong("vérification ou test d'hypothèse"), ", autre composante essentielle de l'analyse de données quantitatives.")
        )
      )
    })
  
    output$ttestgrf <- renderPlot({
      if(input$interpret == 0){return(NULL)}
      studt(ttst(), colreject = "red", colaccept = "lightsteelblue")
    })
  
    output$ttest <- renderUI({
      tbl <- rbind(c("Moyenne chez les garçons", round(m1(), 3)),
                   c("Moyenne chez les filles", round(m2(), 3)),
                   c("Différence observée", round(m1() - m2(), 3)),
                   c("Erreur-Standard de la différence", round(ttst()$stderr, 3)),
                   c("t de Welch", round(ttst()$statistic, 3)),
                   c("Degrés de liberté ", round(ttst()$parameter, 3)),
                   c("Probabilité ", round(ttst()$p.value, 3)),
                   c("Lim. Inférieure", round(ttst()$conf.int[1], 3)),
                   c("Lim. Supérieure", round(ttst()$conf.int[2], 3)),
                   c("Importance de l'effet (d)", round(effsize(), 3)),
                   c("Facteur de Bayes", round(ttstBF()$bf, 3))

      )
      withMathJax(
      DT::datatable(tbl, 
                  colnames=c(" ", " "), 
                  rownames=FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center;',
                    htmltools::em(h3("Inférences Statistiques"))),
                  class = "cell-border stripe",
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    autoWidth = FALSE,
                    columnDefs = list(list(width = '200px', targets = "_all"))
                    )
                  )
      )
      })
  
  output$ttestint <- renderUI({
    if(input$interpret == 0){return(NULL)}
    withMathJax(
    helpText(
      h3("Interprétation:"), p(),
      h4(
      "Le tableau ci-dessus reproduit les résultats d'un test de Welch, une analyse paramétrique, effectué sur les données. ",
      "Ce test produit une statistique standardisant la différence observée entre les moyennes. On trouve $t = $", round(ttst()$statistic, 3), " avec ", round(ttst()$parameter, 3), " degrés de liberté. ",
      "Pour un niveau de signification $\\alpha = 0.05$ et supposant que $H_0$ est vraie, la probabilité d'observer une telle statistique, ou une valeur plus extrême, est égale à ", round(ttst()$p.value, 3), 
      " ce qui en fait un événement ",
      ifelse(ttst()$p.value <= 0.05, "improbable, ", "probable, "), " On peut donc conclure que les observations recueillies sont ",
      ifelse(ttst()$p.value <= 0.05, "incompatibles ", "compatibles "), "avec une hypothèse proposant l'égalité des moyennes au niveau de la population.  ",
      "En répétant l'expérience un nombre infini de fois, on s'attend à ce que dans 95% des cas, la véritable différence entre les moyennes au niveau de la population, $(\\mu_{Garçons}-\\mu_{Filles})$, ",
      "fasse partie de l'intervalle : ", p(),
      "$C[", round(ttst()$conf.int[1], 3), "\\leq (\\mu_{Garçons}-\\mu_{Filles}) \\leq ", round(ttst()$conf.int[2], 3), "] = 95\\%$", p(),
      "Puisqu'une différence nulle ",
      ifelse(ttst()$conf.int[1] < 0 & ttst()$conf.int[2] > 0, "est incluse ", " ne se trouve pas "),
      "dans cet intervalle, on la considère ", 
      ifelse(ttst()$conf.int[1] < 0 & ttst()$conf.int[2] > 0, "probable.", "improbable."), "  En conséquence nous ",
      ifelse(ttst()$p.value <= 0.05, "disposons ", " ne disposons pas "), "d'évidence à l'effet qu'une différence entre les moyennes existe réellement  au niveau de la population.", p(),
      "Finalement, le facteur de Bayes ", paste0("(BF_{10}= ", round(ttstBF()$bf, 3), ")"), " indique une ", ttstBFint(), ". "
      )
      )
    )
    })
  
  output$descript3 <- renderDataTable({
    
    tbl <- rbind(c("n", input$n1, input$n2, input$n1 + input$n2, " "),
                 c("Moyenne", round(m1(), 3), round(m2(), 3), round(mean(Data()$Y), 3), round(m1()-m2(), 3)),
                 c("Médiane", round(median(dat1()), 3), round(median(dat2()), 3), round(median(Data()$Y, 3)), round(median(dat1())-median(dat2()), 3)),
                 c("É-T", round(s1(), 3), round(s2(), 3), round(sd(Data()$Y), 3), " "),
                 c("ÉIQ", round(IQR(dat1()), 3), round(IQR(dat2()), 3), round(IQR(Data()$Y), 3), " ")) 
    DT::datatable(tbl, 
                  colnames=c(" ", "Garçons", "Filles", "Total", "Différence"), 
                  rownames=FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center;',
                    htmltools::em(h3("Résumé des Observations"))),
                  class = "cell-border stripe",
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '30%', targets = "_all"))
                    )
                  )
    })
  
  output$descript2 <- renderUI({
    sstat <- data.frame(Group = c('1', '2', '1-2'),
                        n = c(input$n1, input$n2, input$n1+input$n2),
#                        pmean = c(parms$mu1, parms$mu2, parms$mu1-parms$mu2),
#                        psd = c(round(parms$sig1, 2), round(parms$sig1, 2), " "),
                        smean = c(m1(), m2(), m1()-m2()),
                        ssd = c(round(s1(), 2), round(s2(), 2), " "),
                        sse = c(s1()/sqrt(input$n1), s2()/sqrt(input$n2), ttst()$stderr))
    sstat <- sstat[, 1:5]
    names(sstat) <- c('Groupe', 'n', '$\\bar{X}$',  '$s_X$',  '$s_{\\bar{X}}$')
#    names(sstat) <- c('Groupe', 'n', '$\\mu$', '$\\sigma$', '$\\bar{X}$',  '$s_X$',  '$s_{\\bar{X}}$')
    
    tstat <- data.frame(t(c(ttst()$statistic, ttst()$parameter, ttst()$p.value, 0.05, qt(0.95, ttst()$parameter))))
    
    tstat <- tstat[,1:5]
    tstat[,5] <- abs(tstat[,5]) 
    
    tstat[,5] = sprintf('%.4f', tstat[,5])
    tstat[,4] = sprintf('%1.4f', tstat[,4])
    
    names(tstat) <- c('t Calculé', 'dl', 'Pr(>|t|)', 'Erreur Type 1', 'Valeur critique |t|')
    
withMathJax(
        renderTable(sstat, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c'),
        p(h3("Vérification d'hypothèse")),
        fn_Report(inputVal_Reactive())[['H']],
        renderTable(tstat, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c'))
  })

  output$compplot <- renderPlot({
    p1 <- ggbetweenstats(
      data = Data(),
      x = Groupe,
      y = Y,
      xlab = "Groupe",
      ylab = "Performance",
      plot.type = "boxviolin",
      pairwise.comparisons = TRUE,
      results.subtitle = TRUE,
      effsize.type = "eta",
      type = "parametric",
      pairwise.display = "all",
      bf.message = TRUE,
      outlier.tagging = TRUE,
      outlier.color = "red",
      outlier.label = NULL,
      conf.level = 0.95,
      title = "Comparaison de la Performance des Lycéens et des Lycéennes, en Sciences",
#      caption = "Données générées aléatoirement",
      package = "ggsci",
      palette = "nrc_npg"
    )
    p1
  })
  
  output$showt <- renderUI({
    mdiff <- m1() - m2()
    withMathJax(
      helpText(
        titlePanel("Résultats et Inférences:"),
        h4(paste0("La différence observée entre la moyenne de la performance des filles à l'examen de Sciences ",
                  "et celle des garçons est $(\\bar{X}_{Filles} - \\bar{X}_{Garçons}) = $ ", round(mdiff(), 3), 
                  ". En valeur standardisée, tenant compte de la dispersion des données, cette différence équivaut à $t = ", 
                  round(ttst()$statistic, 3),  "$. En supposant qu'il n'y ait aucune différence entre les moyennes des ",
                  "populations d'où sont tirés ces échantillons, soit $(\\mu_{Filles} - \\mu_{Garçons}) = 0$, ",
                  "la probabilité d'observer une telle différence ou une différence plus extrême est $p = ", 
                  round(ttst()$p.value, 3), "$. Le résultat observé est donc jugé ", ifelse(ttst()$p.value <= 0.05, "improbable", "probable"), 
                  " au seuil de signification $\\alpha = 0.05$. La différence observée ", ifelse(ttst()$p.value > 0.05, "  n'est pas ", " est ")  ,
                  "statistiquement significative.  En conséquence, ", ifelse(ttst()$p.value <= 0.05, " nous disposons ", " il n'y a pas "), 
                  " d'évidence à l'effet que la performance des filles est différente de celle des garçons.")), p(),
        h4("Selon nos estimations, en répétant l'expérience un nombre infini de fois, 95% des différences que nous ",
           "observerions se trouveraient dans l'intervalle s'étendant de ", round(ttst()$conf.int[1], 3), " à ", 
           round(ttst()$conf.int[2], 3), ".", 
           "Puisqu'une différence nulle $(\\bar{X}_{Filles} - \\bar{X}_{Garçons} = 0)$" , 
           ifelse(ttst()$p.value > 0.05, "  fait ", " ne fait pas "), "partie de cet intervalle, un tel résultat est ", 
           ifelse(ttst()$p.value > 0.05, " probable. ", " improbable. "), p(),
           "Finalement, le facteur de Bayes $BF_{10}=$", round(ttstBF()$bf, 3), "$\\pm $", round(ttstBF()$error, 5), " indique qu'il y a ", ttstBFint(), ", par rapport à ",
           ifelse(ttstBF()$bf < 1, "$H_1$.", "$H_0$."),
           "Ainsi, les observations sont ",
           ifelse(ttstBF()$bf > 1, 
                  paste("$$BF_{10}=", round(ttstBF()$bf, 3), "$$"),
                  paste("$$BF_{01}=\\frac{1}{BF_{10}}=\\frac{1}{", round(ttstBF()$bf, 3), "}=", round(1/ttstBF()$bf, 3), "$$")), 
           
           "fois plus vraisemblables sous ",
           ifelse(ttstBF()$bf < 1, "$H_0$ ", "$H_1$ "),
           "que sous ",
           ifelse(ttstBF()$bf < 1, "$H_1$ ", "$H_0$ "),
           ". "

        ) 
        )
      )
    })
  
  }

shinyApp(ui = ui, server = server)
