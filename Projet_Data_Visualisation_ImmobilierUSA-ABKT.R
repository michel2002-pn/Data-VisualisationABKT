library(shiny)
library(readxl)
library(plotly)
library(dplyr)

file_path <- "C:/Users/micho/OneDrive/Bureau/data_viz/Real_Estate_filteres.xlsx"
data <- read_excel(file_path)
data <- data %>%
  mutate(
    price = as.numeric(price),
    house_size = as.numeric(house_size),
    bed = as.numeric(bed)
  ) %>%
  filter(!is.na(price), !is.na(house_size), !is.na(bed))  # Supprimer les lignes avec NA dans ces colonnes

# Calculer le prix moyen par état
state_avg_price <- data %>%
  group_by(state) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# UI
ui <- fluidPage(
  titlePanel("Quels sont les principaux facteurs susceptibles d'influencer les prix de l'immobilier aux États-Unis?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "price_range", 
        "Gamme de prix moyenne par état",
        min   = round(min(state_avg_price$mean_price, na.rm = TRUE)),
        max   = round(max(state_avg_price$mean_price, na.rm = TRUE)),
        value = c(
          round(min(state_avg_price$mean_price, na.rm = TRUE)),
          round(max(state_avg_price$mean_price, na.rm = TRUE))
        ),
        step = 1
      )
    ),
    mainPanel(
      tabsetPanel(
        
        # 1) Introduction
        tabPanel("Introduction",
                 htmlOutput("introTexte")
        ),
        
        # 2) Heatmap (anciennement "Corrélation (Heatmap)")
        tabPanel("Heatmap",
                 plotlyOutput("heatmapPlot"),
                 htmlOutput("heatmapTexte")
        ),
        
        # 3) Graphique 3D
        tabPanel("Graphique 3D", 
                 plotlyOutput("scatter3d"),
                 htmlOutput("scatter3dTexte")  
        ),
        
        # 4) Carte
        tabPanel("Carte", 
                 plotlyOutput("map"),
                 htmlOutput("carteTexte")  
        ),
        
        # 5) Histogramme
        tabPanel("Histogramme", 
                 plotlyOutput("hist"),
                 htmlOutput("histogrammeTexte")
        ),
        
        # 6) Nuage de points
        tabPanel("Nuage de points", 
                 plotlyOutput("scatter"),
                 htmlOutput("nuageDePointsTexte")
        ),
        
        # 7) Boxplot
        tabPanel("Boxplot", 
                 plotlyOutput("boxplot"),
                 htmlOutput("boxplotTexte")
        ),
        
        # 8) Conclusion
        tabPanel("Conclusion",
                 htmlOutput("conclusionTexte")
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Données filtrées par le slider
  filtered_state_avg <- reactive({
    state_avg_price %>%
      filter(mean_price >= input$price_range[1], mean_price <= input$price_range[2])
  })
  
  # --- 1) Introduction ---
  output$introTexte <- renderUI({
    HTML("
      <h3>Bienvenue dans notre application d’analyse immobilière</h3>
      <p>Cette application vous permettra d’explorer plusieurs facteurs 
      influençant le marché immobilier aux États-Unis. 
      Nous allons notamment examiner :</p>
      <ul>
        <li>Le prix moyen des propriétés par État</li>
        <li>La relation entre la taille (superficie) d’un bien, le nombre de chambres et son coût</li>
        <li>L’impact de l’emplacement géographique (États, zones côtières, etc.)</li>
      </ul>
      <p>Notre base de données comporte quatre variables principales :
      <strong>le prix</strong>, le <strong>nom de l’État</strong>, 
      le <strong>nombre de chambres</strong> (bed) et la <strong>superficie</strong> (house_size).</p>
      <p>N’hésitez pas à naviguer dans les différents onglets pour découvrir 
      les cartes, nuages de points, histogrammes, boxplots et graphiques 3D, 
      et obtenir une vue d’ensemble de la dynamique du marché immobilier américain.</p>
    ")
  })
  
  # --- 2) Heatmap (remplace l’onglet “Corrélation (Heatmap)”) ---
  output$heatmapPlot <- renderPlotly({
    # On calcule la matrice de corrélation sur price, house_size, bed
    numeric_vars <- data %>%
      select(price, house_size, bed)
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    
    plot_ly(
      x = colnames(cor_matrix),
      y = rownames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      text = round(cor_matrix, 2),
      texttemplate = "%{text}",
      hoverinfo = "skip",
      colors = colorRamp(c("#084594", "#FFFFFF", "#99000D")),  # Bleu foncé -> blanc -> rouge
      showscale = TRUE
    ) %>%
      layout(
        title = "Heatmap de corrélation entre les variables",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  output$heatmapTexte <- renderUI({
    HTML("
      <h3>Interprétation et explication du tableau de corrélation</h3>
      <p>Voici quelques points clés pour <strong>interpréter</strong> et <strong>expliquer</strong> ce tableau de corrélation :</p>

      <ol>
        <li>
          <strong>Rappel sur le coefficient de corrélation :</strong><br/>
          Sa valeur varie entre <strong>-1</strong> et <strong>+1</strong>.<br/>
          +1 indique une corrélation positive parfaite (les deux variables augmentent ou diminuent ensemble).<br/>
          -1 indique une corrélation négative parfaite (lorsque l’une augmente, l’autre diminue).<br/>
          0 signifie qu’il n’y a pas de relation linéaire notable.
        </li>

        <li>
          <strong>Lecture de la matrice :</strong><br/>
          Les cellules contiennent la valeur numérique du coefficient de corrélation entre la variable en ligne et la variable en colonne.<br/>
          La diagonale correspond à la corrélation d’une variable avec elle-même (= 1).<br/>
          Les couleurs vont du <em>bleu</em> (corrélation proche de -1), au <em>blanc</em> (corrélation proche de 0), 
          jusqu’au <em>rouge</em> (corrélation proche de +1).
        </li>

        <li>
          <strong>Exemples d’interprétation :</strong><br/>
          <ul>
            <li><em>price et house_size :</em> un coefficient d’environ 0.58 signifie une corrélation positive modérée à forte : 
            plus la superficie est grande, plus le prix tend à être élevé.</li>
            <li><em>bed et house_size :</em> environ 0.64, suggérant également une corrélation positive 
            (maisons plus grandes = plus de chambres).</li>
            <li><em>price et bed :</em> environ 0.3, donc corrélation positive mais plus faible 
            (plus de chambres = prix plus élevé, mais ce n’est pas le seul facteur).</li>
          </ul>
        </li>

        <li>
          <strong>Constat général :</strong><br/>
          Toutes les corrélations sont positives, aucune variable ne varie en sens inverse d’une autre.<br/>
          Aucune corrélation (hors diagonale) n’atteint +1 ou -1, donc pas de dépendance <em>parfaite</em>.<br/>
          La superficie (house_size) semble la plus liée au prix (price), 
          un indicateur fort que la taille du bien immobilier est un levier majeur sur son coût.
        </li>
      </ol>
    ")
  })
  
  # --- 3) Graphique 3D ---
  output$scatter3d <- renderPlotly({
    df <- data
    plot_ly(
      df,
      x = ~house_size,
      y = ~bed,
      z = ~price,
      type = 'scatter3d',
      mode = 'markers',
      color = ~state,
      text = ~state
    ) %>%
      layout(title = "Relation 3D : Prix, Taille et Chambres")
  })
  
  output$scatter3dTexte <- renderUI({
    HTML("
      <h3>Analyse du graphique 3D (Prix, Superficie, Chambres)</h3>
      <p>Ce graphique 3D illustre la relation entre la <strong>superficie du bien (axe X)</strong>, 
      le <strong>nombre de chambres (axe Y)</strong> et le <strong>prix (axe Z)</strong>. 
      Chaque point est également associé à l’État dans lequel se trouve le bien (information affichée 
      au survol du point).</p>

      <p>On remarque qu’une concentration de points se situe <em>vers le centre et le haut</em> du graphe, 
      suggérant une <strong>corrélation positive</strong> : plus la maison est spacieuse et pourvue de chambres, 
      plus le prix tend à être élevé. Pourtant, certains points s’éloignent de cette logique et présentent 
      un <strong>prix élevé</strong> malgré une superficie ou un nombre de chambres moins importants. 
      Cela confirme <strong>l’impact déterminant de l’État</strong> (ex. Californie, Floride, New York) : 
      ces régions peuvent atteindre des prix exorbitants, même pour des biens qui ne sont pas forcément 
      très grands ni dotés de nombreuses chambres.</p>

      <p>De l’autre côté, on observe parfois des biens à <strong>grande superficie</strong> 
      ou plusieurs chambres, mais un prix nettement plus faible, comme dans le <em>Texas</em> 
      ou la <em>Caroline du Nord</em>. Cette situation illustre 
      que <strong>l’emplacement géographique pourrait </strong> (l’État, en particulier) 
      peser fortement sur la formation des prix.</p>
    ")
  })
  
  # --- 4) Carte ---
  output$map <- renderPlotly({
    us_geojson <- 'https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json'
    
    # Calcul des statistiques pour chaque état
    filtered_data <- data %>%
      group_by(state) %>%
      summarise(
        min_price = min(price, na.rm = TRUE),
        max_price = max(price, na.rm = TRUE),
        mean_price = mean(price, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Texte pour les infobulles
    filtered_data$text <- paste0(
      "État: ", filtered_data$state, "<br>",
      "Prix minimum: $", formatC(filtered_data$min_price, format = "f", big.mark = ",", digits = 0), "<br>",
      "Prix maximum: $", formatC(filtered_data$max_price, format = "f", big.mark = ",", digits = 0), "<br>",
      "Prix moyen: $", formatC(filtered_data$mean_price, format = "f", big.mark = ",", digits = 0)
    )
    
    # Génération de la carte
    plot_ly(
      type = "choroplethmapbox",
      geojson = us_geojson,
      locations = filtered_data$state,
      featureidkey = "properties.name",
      z = filtered_data$mean_price,
      colorscale = list(c(0, "#cceeff"), c(1, "#00008b")), # Dégradé rouge clair -> rouge foncé
      text = filtered_data$text, # Infobulle avec détails
      hoverinfo = "text", # Affichage des infobulles
      reversescale = FALSE,
      colorbar = list(title = "Prix moyen ($)"),
      marker = list(color="white",line = list(width = 1.5))
    ) %>%
      layout(
        mapbox = list(
          style = "carto-positron",
          center = list(lat = 37.0902, lon = -95.7129),
          zoom = 3.5
        ),
        title = "Coût moyen des propriétés par état"
      )
  })
  
  
  output$carteTexte <- renderUI({
    HTML("
      <p>On constate que certains États, notamment sur la <strong>côte Ouest</strong> (comme la Californie ou l’Oregon),
      ainsi que sur la <strong>côte Est</strong> (par exemple New York, le Massachusetts ou le Connecticut),
      affichent une couleur plus foncée, traduisant une <strong>moyenne de prix plus élevée</strong>.
      Même des États à l’intérieur du pays, comme le <strong>Colorado</strong>,
      se détachent par leur coût immobilier élevé.</p>

      <p>Plusieurs facteurs peuvent expliquer ces prix plus importants :</p>
      <ul>
        <li>Une <strong>forte demande</strong> liée à l’attractivité économique,
        la présence de grands pôles d’emplois et d’universités renommées,
        ou encore un cadre de vie très recherché.</li>

        <li>Des régions côtières souvent associées à de grands centres urbains densément peuplés,
        où la pression immobilière fait grimper les prix.</li>

        <li>Un <strong>environnement naturel</strong> attractif ou un tourisme développé
        (par exemple, les montagnes du Colorado ou les plages californiennes).</li>
      </ul>

      <p>À l’inverse, des États plus « centraux » comme le <strong>Kansas</strong> ou le <strong>Missouri</strong>
      demeurent dans des nuances plus claires, témoignant de <strong>prix immobiliers plus abordables</strong>.
      Ces zones, moins denses et moins touristiques, offrent souvent une <strong>demande plus faible</strong>
      en matière de logements, un <strong>coût de la vie</strong> relativement bas,
      et moins de concurrence pour l’achat ou la location.</p>

      <p>Ainsi, la <strong>localisation géographique</strong> (État, région côtière, pôle urbain ou rural)
      s’avère un facteur déterminant dans la <strong>variation des prix de l’immobilier</strong> aux États-Unis.</p>
    ")
  })
  
  # --- 5) Histogramme ---
  output$hist <- renderPlotly({
    df <- data %>%
      filter(bed >= 1 & bed <= 20)
    
    plot_ly(
      df,
      x = ~bed,
      y = ~price,
      type = 'bar',
      color = ~state
    ) %>%
      layout(
        barmode = "group",
        title = "Répartition des prix par nombre de chambres (bed 1 à 20)",
        xaxis = list(
          title = "Nombre de chambres",
          tickmode = "linear",
          dtick = 1,
          range = c(0.5, 20.5)
        )
      )
  })
  
  output$histogrammeTexte <- renderUI({
    HTML("
      <h3>Analyse des prix en fonction du nombre de chambres</h3>
      <p>En observant cet histogramme, on remarque que <strong>le prix a tendance à augmenter 
      avec le nombre de chambres</strong>. Toutefois, ce qui ressort encore plus est que 
      des États chers comme la <strong>Californie</strong> ou la <strong>Floride</strong> 
      peuvent afficher des prix très élevés, même lorsqu'ils ne comptent pas beaucoup de chambres. 
      Cela confirme une nouvelle fois l'importance déterminante de l’<strong>État</strong> dans 
      la fixation du prix d’un bien immobilier.</p>
      
      <p>Inversement, on trouve aussi des <strong>maisons avec beaucoup de chambres 
      qui ne sont pas forcément très chères</strong>. Plusieurs facteurs peuvent l’expliquer :</p>
      <ul>
        <li>Une <em>localisation</em> moins attractive, par exemple éloignée des centres urbains 
        ou des zones touristiques.</li>
        <li>Un bien nécessitant de lourdes <em>rénovations</em>, 
        ce qui peut décourager les acheteurs malgré un grand nombre de chambres.</li>
        <li>La nature spécifique de la propriété (comme une <em>auberge</em> ou un établissement 
        à usage collectif), où le prix moyen par chambre demeure relativement bas.</li>
      </ul>
      
      <p>En somme, le <strong>nombre de chambres</strong> reste un indicateur pertinent pour estimer 
      la valeur d’un bien, mais l’<strong>emplacement géographique</strong> et la <strong>qualité globale</strong> 
      du logement influencent souvent davantage les prix finaux.</p>
    ")
  })
  
  # --- 6) Nuage de points ---
  output$scatter <- renderPlotly({
    df <- data
    plot_ly(
      df,
      x = ~house_size,
      y = ~price,
      type = 'scatter',
      mode = 'markers',
      color = ~state,
      text = ~state
    ) %>%
      layout(title = "Taille de la maison vs Prix")
  })
  
  output$nuageDePointsTexte <- renderUI({
    HTML("
      <h3>Analyse des nuages de points entre prix et superficie des maisons</h3>
      <p>En observant les nuages de points représentant la relation entre le <strong>prix</strong>
      et la <strong>superficie des maisons</strong>, plusieurs tendances et exceptions se dessinent clairement :</p>
      <ol>
        <li><strong>Tendance générale : plus la maison est grande, plus le prix est élevé</strong>
          <p>La règle générale semble indiquer que le prix des maisons augmente proportionnellement à leur superficie. 
          Cela traduit une relation attendue entre la taille et la valeur d’un bien immobilier.</p>
        </li>

        <li><strong>Les exceptions marquantes</strong>
          <p>Cependant, certaines exceptions ressortent nettement :</p>
          <ul>
            <li><strong>Californie, New York et Floride</strong> : Les prix des maisons y sont souvent exorbitants, 
            indépendamment de leur superficie.</li>
            <li>On observe également des cas où des propriétés de <em>grande superficie</em> ne sont pas très chères. 
            Cela peut s’expliquer par une localisation moins attractive (quartiers périphériques ou éloignés, 
            zones moins demandées), la nécessité de rénovations importantes, 
            ou encore une moindre pression immobilière dans ces régions.</li>
          </ul>
        </li>

        <li><strong>L’impact de l’emplacement géographique</strong>
          <p>Ces anomalies peuvent s’expliquer par le fait que certaines petites maisons, situées dans des 
          <em>États très chers</em>, atteignent des prix faramineux. Cela est probablement influencé par :</p>
          <ul>
            <li>La <strong>demande élevée</strong> dans ces zones.</li>
            <li>La présence de <strong>quartiers prestigieux</strong> qui augmentent la valeur des propriétés, 
            même si leur superficie est réduite.</li>
          </ul>
        </li>

        <li><strong>Le rôle déterminant de l’État</strong>
          <p>Cette observation confirme que le choix de l’<strong>État</strong> dans lequel une maison est située 
          impacte significativement son prix. En effet, dans certaines régions, l’emplacement 
          peut peser bien davantage sur le coût que la taille réelle de la maison.</p>
        </li>
      </ol>

      <p>En conclusion, bien que la taille d’une maison reste un facteur clé dans la détermination de son prix, 
      l’emplacement géographique — notamment au niveau de l’État et du quartier — joue parfois 
      un rôle encore plus déterminant.</p>
    ")
  })
  
  # --- 7) Boxplot ---
  output$boxplot <- renderPlotly({
    df <- data
    plot_ly(df, x = ~state, y = ~price, type = 'box', color = ~state) %>%
      layout(title = "Distribution des prix par état")
  })
  
  output$boxplotTexte <- renderUI({
    HTML("
      <h3>Interprétation du boxplot par État</h3>
      <p>Sur ce boxplot, chaque <strong>État</strong> est représenté en abscisse, 
      tandis que la <strong>distribution des prix</strong> apparaît en ordonnée. 
      La <em>boîte</em> (box) représente l’intervalle interquartile (IQR), 
      la ligne à l’intérieur marque la <strong>médiane</strong>, 
      et les <em>points isolés</em> au-dessus (ou en dessous) des moustaches sont des <strong>valeurs atypiques</strong> (outliers).</p>

      <p>On observe ainsi que certains États présentent des prix <strong>extrêmement élevés</strong> 
      (au-delà de plusieurs dizaines de millions de dollars), 
      traduisant la présence de propriétés luxueuses ou situées dans des zones extrêmement demandées 
      (Californie, Floride, New York, etc.). D’autres, au contraire, ont une dispersion plus faible et 
      une médiane plus basse, suggérant des prix davantage <strong>homogènes</strong> et <strong>abordables</strong>.</p>

      <p>En résumé, ce boxplot confirme à quel point la localisation (l’État) peut influencer 
      la variabilité et le niveau de prix, avec des <strong>outliers</strong> marquant 
      des biens d’exception fortement valorisés, souvent situés dans des régions touristiques, 
      urbaines ou à forte attractivité économique.</p>
    ")
  })
  
  # --- 8) Conclusion ---
  output$conclusionTexte <- renderUI({
    HTML("
      <h3>Conclusion générale sur l’analyse</h3>
      <p>Au terme de cette exploration du marché immobilier américain, plusieurs éléments 
      ressortent clairement :</p>
      <ul>
        <li>La <strong>localisation</strong> demeure un facteur clé : 
        certains États, notamment côtiers ou très attractifs (Californie, Floride, New York), 
        affichent des prix souvent bien plus élevés, même pour des surfaces ou un nombre de chambres limité.</li>
        
        <li>La <strong>taille</strong> et le <strong>nombre de chambres</strong> influencent 
        indéniablement la valeur d’un bien, mais leur impact est parfois éclipsé par la 
        <strong>demande locale</strong> et la réputation de l’État ou du quartier.</li>
        
        <li>La <strong>variabilité</strong> des prix d’un État à l’autre se retrouve 
        dans des distributions (boxplots) très étendues ou, au contraire, relativement homogènes 
        selon le niveau socio-économique, la densité de population et d’autres critères (tourisme, urbanisation…).</li>
      </ul>
      <p>En somme, si la superficie et le nombre de chambres constituent des marqueurs évidents 
      pour déterminer la valeur d’un bien immobilier, c’est souvent l’<strong>emplacement géographique</strong> 
      (au sens large : État, ville, quartier) qui établit la limite supérieure ou inférieure des prix possibles. 
      La présence d’anomalies (biens très chers pour des surfaces modestes, ou inversement) 
      met en évidence la complexité de ce marché, influencé tant par la localisation 
      que par la demande, l’urbanisation, et les spécificités de chaque zone.</p>
      
      <p>Nous espérons que cette application vous aura permis de mieux comprendre les facteurs 
      clés qui pèsent sur le prix des propriétés aux États-Unis, et nous vous invitons 
      à poursuivre votre exploration en approfondissant ces différentes pistes d’analyse !</p>
    ")
  })
}

shinyApp(ui = ui, server = server)
