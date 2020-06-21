helpPageText <- function() {
    tagList(
      tags$p("Via de browser kun je navigeren door een netwerk van 
                  gegevens over informatie-uitwisseling in de zorg in Nederland.
                  De iconen (nodes) representeren personen, systemen, objecten, partijen, et cetera. 
                  De lijnen (links) geven verbanden aan. 
                 "),
      tags$p("Browsen is simpel. Als je de nodes van een bepaalde soort wilt zien, selecteer die soort dan uit de lijst met domeinen.
                  Klik vervolgens op een node en kies welke verbanden (soorten links) je 
                  wilt toevoegen aan de view. Gebruik hiervoor het menu:"),
      tags$p(tags$img(src="NetVismenu.png")),
      tags$p("De kleuren/letters van de knopjes geven de mogelijke 
                 verbanden aan:"),
      tags$ul(
        tags$li("a(ctor) links"), 
        tags$li("u(se) links"), 
        tags$li("s(ystem) links"), 
        tags$li("o(bject) links"), 
        tags$li("r(efer) links"),
        tags$li("* alle  links")
      ),
      tags$p(
        "Je kunt deze knoppen ook toepassen op alle nodes die zichtbaar zijn in de view.
        Daar heb je ook de mogelijkheid om alle links te tonen tussen nodes in de view (via #)."
      ),
      tags$p("De betekenis van andere knoppen:"),
      tags$ul(
        tags$li("H(ide) verwijdert de node uit de view"),
        tags$li("F(ocus) focusseert de view op deze node"),
        tags$li("C(lone) clone de geselecteerde node"),
        
        tags$li("All- toon alle nodes en links in het onderliggende netwerk"),
 
        tags$li("Redraw - teken de view opnieuw (leidt mogelijk tot andere layout)"),
      ),
      tags$p(
        "Bovenin vind je controls om de huidige view te bewaren (in een JSON download), een bestaande (JSON) view te laden of 
        de view opnieuw te beginnen. Je vindt daar ook een aantal opties voor de weergave van de view."
      ),
      
    )
}