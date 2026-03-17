test_that("default app settings keep ggplot2 palette and export defaults", {
  settings <- get_default_app_settings()

  expect_equal(settings$palette_id, APP_DEFAULT_PALETTE_ID)
  expect_equal(
    settings$export,
    list(
      width = PLOT_EXPORT_WIDTH_DEFAULT,
      height = PLOT_EXPORT_HEIGHT_DEFAULT,
      dpi = PLOT_EXPORT_DPI_DEFAULT,
      format = PLOT_EXPORT_FORMAT_DEFAULT
    )
  )
  expect_equal(get_palette_preview_colors(settings, list(), n = 4), scales::hue_pal()(4))
})

test_that("hex palette validation rejects invalid colors", {
  valid <- validate_hex_colors(c("#112233", "#ABCDEF"))
  invalid <- validate_hex_colors(c("#11223G", "blue"))

  expect_true(valid$valid)
  expect_false(invalid$valid)
  expect_equal(invalid$invalid, c("#11223G", "BLUE"))
})

test_that("sanitize export settings falls back to defaults for invalid values", {
  settings <- sanitize_export_settings(list(width = -1, height = 0, dpi = NA, format = "svg"))

  expect_equal(settings$width, PLOT_EXPORT_WIDTH_DEFAULT)
  expect_equal(settings$height, PLOT_EXPORT_HEIGHT_DEFAULT)
  expect_equal(settings$dpi, PLOT_EXPORT_DPI_DEFAULT)
  expect_equal(settings$format, PLOT_EXPORT_FORMAT_DEFAULT)
})

test_that("plot download spec binds filename and writes plot files", {
  plot_obj <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(x = wt, y = mpg, color = factor(cyl))
  ) +
    ggplot2::geom_point()

  spec <- build_plot_download_spec(
    plot_fn = function() plot_obj,
    export_settings_fn = function() list(width = 7, height = 4, dpi = 144, format = "png"),
    filename_prefix = "Quality Plot"
  )

  tmp <- tempfile(fileext = ".png")

  expect_equal(spec$filename(), "plot_quality_plot.png")
  expect_no_error(spec$content(tmp))
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)
})
