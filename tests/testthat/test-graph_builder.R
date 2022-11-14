library(shiny)
library(igraph)

# Setup tests
g <- make_graph(c(1, 2, 1, 3, 3, 4), directed = TRUE)

# Test edgelist upload
testServer(
  graphDesignServer,
  expr = {
    path <- tempfile(fileext = ".txt")
    write_graph(g, path, format = "edgelist")
    session$setInputs(
      definition = "file",
      upload_format = ".txt",
      file = list(datapath = path),
    )
    session$flushReact()
    h <- isolate(graph())
    expect_true(is_directed(h))
    expect_identical(gorder(g), gorder(h))
    expect_identical(gsize(g), gsize(h))
  },
  args = list(dataset = reactive({
    NULL
  }))
)

# Test graphml upload
testServer(
  graphDesignServer,
  expr = {
    path <- tempfile(fileext = ".graphml")
    write_graph(g, path, format = "graphml")
    session$setInputs(
      definition = "file",
      upload_format = ".graphml",
      file = list(datapath = path),
    )
    session$flushReact()
    h <- isolate(graph())
    expect_true(is_directed(h))
    expect_identical(gorder(g), gorder(h))
    expect_identical(gsize(g), gsize(h))
  },
  args = list(dataset = reactive({
    NULL
  }))
)

# Test gml upload
testServer(
  graphDesignServer,
  expr = {
    path <- tempfile(fileext = ".gml")
    write_graph(g, path, format = "gml")
    session$setInputs(
      definition = "file",
      upload_format = ".gml",
      file = list(datapath = path),
    )
    session$flushReact()
    h <- isolate(graph())
    expect_true(is_directed(h))
    expect_identical(gorder(g), gorder(h))
    expect_identical(gsize(g), gsize(h))
  },
  args = list(dataset = reactive({
    NULL
  }))
)
