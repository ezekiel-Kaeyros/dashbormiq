box::use(
  shiny[div, NS, tags, tagList, img, h1, a,downloadButton,h3],
  shiny.fluent[fluentPage, ThemeProvider, Text,Dropdown.shinyInput]
)

box::use(
  app/view/components/sidebar,
  app/view/components/header,
  app/logic/import_data
)


theme <- list(
  palette = list(
    themePrimary = "#5547AC",
    themeLighterAlt = "#f6f6fc",
    themeLighter = "#dedbf2",
    themeLight = "#c3bde6",
    themeTertiary = "#8e84cd",
    themeSecondary = "#6457b5",
    themeDarkAlt = "#4b3f9a",
    themeDark = "#3f3582",
    themeDarker = "#2f2760",
    neutralLighterAlt = "#faf9f8",
    neutralLighter = "#f3f2f1",
    neutralLight = "	#edebe9",
    neutralQuaternaryAlt = "#2c2b2a",
    neutralQuaternary = "#e1dfdd",
    neutralTertiaryAlt = "#c8c6c4",
    neutralTertiary = "#a19f9d",
    neutralSecondary = "#605e5c",
    neutralPrimaryAlt = "#3b3a39",
    neutralPrimary = "#323130",
    neutralDark = "#201f1e",
    black = "#000000",
    white = "#ffffff"
  )
)

#' @export
main_layout <- function(main_ui) {
  ThemeProvider(
    theme = theme,
    div(class = "container",
    #div(class = "sidebar", sidebar$sidebar_ui),
      div(class = "content",
          div(class = "header", header$header_ui),
          div(class = "main", main_ui)),
      div(class = "footer")
    )
  )
}


#' @export
home_layout <- function(home_data_overview,home_recent) {
  fluentPage(
    div(class = "home",
        tagList(
          div(class = "home__header",
              h1("Good Morning,"),
              h1(class = "title--colored", "Name"),
          ),
          div(class = "home__overview",
              h1(class = "overview__title", "Overview"),
              home_data_overview,
          ),
          div(class = "recent_page",
              home_recent
          )
        )
    )
  )
}


# Qualitative data page
# tl: top left card; tr_card: top right card; bl: bottom left card; br: bottom right card

#' @export
quantitative_page_layout <- function(tl_card, bl_card,mid_b_card, middle_card,mid_card, mid_b_card2,tr_card, br_card,mid_b_card3) {#mid_card
  div(class = "quantitative_page",
    div(class = "quantitative_page__content",
      div(class = "left",
        tl_card,
        bl_card,
        mid_b_card,
        middle_card,
        mid_card,
        mid_b_card2,
        tr_card,
        br_card
      ),
    ),
    div(#style="width: 60% !important;margin-right: 300px;height: auto;margin: auto;",#class = "middle_bottom",
      style="width: 60% !important;margin-right: 300px;height: auto;margin: auto;",
      mid_b_card3
    )
  )
}

#' @export
quantitative_page_layout_org <- function(tl_card, bl_card,mid_b_card, middle_card,mid_card, mid_b_card2) {#mid_card
  div(class = "quantitative_page",
      div(class = "quantitative_page__content",
          div(class = "left",
              tl_card,
              bl_card,
              mid_b_card,
              mid_card,
              mid_b_card2,
              middle_card
          ),
      )
  )
}

#' @export
quantitative_bivariate_layout <- function(head,tl_card, bl_card,tr_card, br_card, token) {#mid_card
  div(class = "quantitative_bivariate",
      head,
      div(style="height: 20px;"),
      div(class = "quantitative_bivariate__content",
          div(class = "quantitative_bivariate_left",
              tl_card,
              bl_card,
              tr_card,
              br_card
          )
      )

  )
}

#' @export
qualitative_layout <- function(card, token) {#mid_card
  div(class="layout_qualitative",
      div(class = "head_section",
          h1(class = "quantitative_page__title", ""),
          div( style = " display: flex; gap: 0.5rem; margin-top:15px; padding-right:18px",#float: right;
               div(#style = "float: right;  gap: 0.5rem; margin-top: 10px;",#28px
                 shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                        iconProps = list(iconName = "Refresh"),
                                                        style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em;
                            text-align: center; border-color: #fff; border-radius: 12px;
                            border: 1px solid black;height:60px;
                           color: #fff; font-weight: bold;"
                 )),
               shiny.fluent::Link(href = paste("#!/wordcloud?token=", token, sep = ""),
                                  "Siehe Wordcloud",
                                  style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em;
                                  border-color: #fff; border-radius: 12px; border: 1px solid black;
                           color: #fff; font-weight: bold; display: flex;"),
               shiny.fluent::DefaultButton.shinyInput("export_bivariate", "Daten exportieren",
                                                      iconProps = list(iconName = "Download")
               )
          ),
      ),
      # tags$br(),
      # tags$br(),
      div(style = "height:2rem;"),
      h1(""),#class = "qualitative_page__title",
      div(#class = "qualitative_page__content",
        div(style=" width:100%",#class = "qualitative_page_middle",
            card
        )
      )
      
  )
}


#' @export
wordcloud_layout <- function(card){#, token) {#mid_card
div(class = "qualitative_page",
    
    h1(class = "qualitative_page__title",  ""), #Wordcloud view
    
    
    div(class = "qualitative_page__content",
        div(class = "qualitative_page_middle",
            
            div(style = "background-color: #ffffff;", card),
            
        )
    ),
    
    
)
}