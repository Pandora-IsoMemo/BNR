# Load required packages
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyhelper)

# Define data_import UI function
dataImportUI <- function(id = "data_import") {
  # Load module namespace
  ns <- NS(id)

  # Accepted extensions
  formats <- list(
    "Comma Serparated Value (.csv)" = ".csv",
    "Excel Spreadsheet 2003 (.xls)" = ".xls",
    "Excel Spreadsheet 2007 (.xlsx)" = ".xlsx",
    "OpenDocument Spreadsheet (.ods)" = ".ods"
  )

  # Module UI
  tagList(
    fluidRow(
      box(
        title = "Data Import",
        status = "primary",
        solidHeader = TRUE,
        width = 3,
        selectInput(ns("format"), "Choose a file format:", formats) %>%
          helper(
            type = "markdown",
            content = "data_import-format"
          ),
        checkboxInput(ns("header"), "First row contains header.", TRUE) %>%
          helper(
            type = "markdown",
            content = "data_import-header"
          ),
        checkboxInput(ns("index"), "First column contains index.", FALSE) %>%
          helper(
            type = "markdown",
            content = "data_import-index"
          ),
        fileInput(
          ns("file"),
          "Choose a file to upload:",
          multiple = FALSE,
          accept = unlist(formats, use.names = FALSE),
        ) %>%
          helper(
            type = "markdown",
            content = "data_import-file"
          )
      ),
      box(
        title = "Dataset Preview",
        status = "primary",
        solidHeader = TRUE,
        width = 9,
        dataTableOutput(ns("preview"))
      )
    ),
    fluidRow(
      box(
        title = "Dataset Summary",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        dataTableOutput(ns("summary"))
      )
    ),
  )
}
