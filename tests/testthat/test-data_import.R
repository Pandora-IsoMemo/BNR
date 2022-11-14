library(shiny)
library(readxl)
library(WriteXLS)
library(readODS)

# Test csv upload
testServer(
  dataImportServer,
  expr = {
    path <- tempfile(fileext = ".csv")
    write.csv(iris, path, row.names = FALSE)
    session$setInputs(
      format = ".csv",
      header = TRUE,
      file = list(datapath = path),
    )
    expect_identical(dataset(), read.csv(path))
  }
)

# Test xls upload
testServer(
  dataImportServer,
  expr = {
    path <- tempfile(fileext = ".xls")
    WriteXLS(iris, path, row.names = FALSE)
    session$setInputs(
      format = ".xls",
      header = TRUE,
      file = list(datapath = path),
    )
    expect_identical(
      dataset(),
      read_excel(path, sheet = 1, col_names = TRUE)
    )
  }
)

# Test xlsx upload
testServer(
  dataImportServer,
  expr = {
    path <- tempfile(fileext = ".xlsx")
    WriteXLS(iris, path, row.names = FALSE)
    session$setInputs(
      format = ".xlsx",
      header = TRUE,
      file = list(datapath = path),
    )
    expect_identical(
      dataset(),
      read_excel(path, sheet = 1, col_names = TRUE)
    )
  }
)

# Test ods upload
testServer(
  dataImportServer,
  expr = {
    path <- tempfile(fileext = ".ods")
    write_ods(iris, path, row_names = FALSE)
    session$setInputs(
      format = ".ods",
      header = TRUE,
      file = list(datapath = path),
    )
    expect_identical(
      dataset(),
      read_ods(path, sheet = 1, col_names = TRUE)
    )
  }
)
