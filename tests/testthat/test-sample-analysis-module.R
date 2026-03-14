test_that("sample analysis module initializes with empty data", {
  state <- shiny::reactiveValues(
    dpcr_data = new_empty_dpcr_data()
  )

  expect_no_error(
    shiny::testServer(
      mod_sample_analysis_server,
      args = list(id = "sample_analysis", state = state),
      {
        session$flushReact()
      }
    )
  )
})
