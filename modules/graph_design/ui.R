# Load required packages
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinyhelper)
library(visNetwork)

# Define graph_design UI function
graphDesignUI <- function(id = "graph_design") {
  # Load module namespace
  ns <- NS(id)

  # Accepted extensions
  formats <- list(
    "EdgeList (.txt)" = ".txt",
    "GraphML (.graphml)" = ".graphml",
    "GML (.gml)" = ".gml"
  )

  # Module UI
  tagList(
    fluidRow(
      column(
        3,
        box(
          title = "Graph Builder",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(
            ns("definition"),
            "Choose a graph builder:",
            list(
              "Interactive builder" = "interactive",
              "Import from file" = "file"
            )
          ) %>%
            helper(
              type = "markdown",
              content = "graph_design-definition"
            ),
          conditionalPanel(
            condition = "input.definition == 'file'",
            ns = ns,
            selectInput(
              ns("upload_format"),
              "Choose an upload format:",
              formats
            ) %>%
              helper(
                type = "markdown",
                content = "graph_design-upload_format"
              ),
            fileInput(
              ns("file"),
              "Choose a file to upload:",
              multiple = FALSE
            ) %>%
              helper(
                type = "markdown",
                content = "graph_design-file"
              ),
          )
        ),
        box(
          title = "Export Graph",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(
            ns("download_format"),
            "Choose a download format:",
            formats
          ) %>%
            helper(
              type = "markdown",
              content = "graph_design-download_format"
            ),
          downloadButton(ns("download_graph"))
        )
      ),
      box(
        title = "Graph Interactive Preview",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        visNetworkOutput(ns("preview")) %>%
          helper(
            type = "markdown",
            content = "graph_design-preview"
          ),
        tags$script(
          sprintf(
            "
            $(document).on('keyup', function (e) {
              Shiny.setInputValue('%s', [e.which, e.timeStamp]);
            });
          ",
            ns("keyup")
          )
        )
      ),
      column(
        3,
        box(
          title = "Plot Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(
            ns("graph_layout"),
            "Choose the graph layout:",
            list(
              "Circular" = "layout_in_circle",
              "Grid" = "layout_on_grid",
              "Tree" = "layout_as_tree"
            )
          ),
          selectInput(
            ns("graph_node_shape"),
            "Choose node shape:",
            list(
              "Box" = "box",
              "Square" = "square",
              "Triangle" = "triangle",
              "Circle" = "circle",
              "Dot" = "dot",
              "Star" = "star",
              "Ellipse" = "ellipse",
              "Diamond" = "diamond"
            )
          ),
          colorSelectorInput(
            ns("graph_node_color"),
            "Choose graph color:",
            c("white", "grey", palette()),
            selected = "#2297E6"
          ),
          HTML("<h5><b>Choose graph shadows:</b></h5>"),
          checkboxInput(ns("graph_node_shadow"),
            "Enable node shadow",
            value = TRUE
          ),
          checkboxInput(ns("graph_edge_shadow"),
            "Enable edge shadow",
            value = TRUE
          )
        ),
        box(
          title = "Export Plot",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(
            ns("download_plot_format"),
            "Choose a plot format:",
            c("png", "jpeg")
          ),
          uiOutput(ns("download_plot"))
        )
      )
    )
  )
}
