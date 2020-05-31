helpPageText <- function() {
    tagList(
      tags$p("Via de browser kun je navigeren door een netwerk van 
                  gegevens over informatie-uitwisseling in de zorg in Nederland.
                  De iconen (nodes) representeren personen, systemen, objecten, partijen, et cetera. 
                  De lijnen (links) geven verbanden aan. 
                 "),
      tags$p("Browsen is simpel. Als je de nodes van een bepaalde soort wilt zien, selecteer die soort dan uit de lijst.
                  Klik vervolgens op een node en kies welke verbanden (soorten links) je 
                  wilt toevoegen aan de view. De kleuren/letters van de knopjes geven de mogelijke 
                 verbanden aan:"),
      
      tags$ul(
        tags$li("a(ctor) links"), 
        tags$li("u(se) links"), 
        tags$li("s(ystem) links"), 
        tags$li("o(bject) links"), 
        tags$li("p(art) links"),
        tags$li("r(efer) links"),
        tags$li("* alle  links")
      ),
      tags$p(
        "Je kunt deze knoppen ook toepassen op alle nodes die zichtbaar zijn in de view."
      ),
      tags$p("De betekenis van andere knoppen:"),
      tags$ul(
        tags$li("H(ide) verwijdert de node uit de view"),
        tags$li("F(ocus) focusseert de view op deze node (dubbel-klik op de node doet dit ook)"),
        
        tags$li("All- toon alle nodes en links in het onderliggende netwerk"),
        tags$li(">View - maak een apart (read-only) viewpanel voor de huidige weergave"),
        tags$li("Export - exporteer de view naar een HTML bestand"),
        tags$li("Redraw - teken de view opnieuw (leidt tot andere layout)"),
        tags$li("Restart - breng de view terug naar de begintoestand")
      )
    )
}