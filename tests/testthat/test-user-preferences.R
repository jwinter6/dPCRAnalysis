test_that("resolve browser user id prefers explicit server id and falls back to query string", {
  expect_equal(resolve_browser_user_id("alice", "?userId=bob"), "alice")
  expect_equal(resolve_browser_user_id(NULL, "?userId=bob"), "bob")
  expect_equal(resolve_browser_user_id(NULL, "?user_id=carol"), "carol")
  expect_equal(resolve_browser_user_id(NULL, NULL), "anonymous")
})

test_that("browser preferences preserve app settings and custom palettes", {
  custom_palettes <- list(Brand = c("#112233", "#445566"))
  prefs <- browser_preferences_from_state(
    app_settings = list(
      palette_id = custom_palette_id("Brand"),
      export = list(width = 7, height = 4, dpi = 144, format = "jpeg")
    ),
    custom_palettes = custom_palettes
  )

  expect_equal(prefs$theme$paletteName, custom_palette_id("Brand"))
  expect_equal(prefs$appSettings$palette_id, custom_palette_id("Brand"))
  expect_equal(prefs$appSettings$export$format, "jpeg")
  expect_equal(prefs$customPalettes$Brand, c("#112233", "#445566"))
  expect_equal(prefs$theme$primaryColor, "#112233")
  expect_equal(prefs$theme$accentColor, "#445566")
})

test_that("sanitize user preferences falls back to defaults on invalid data", {
  prefs <- sanitize_user_preferences(
    list(
      theme = list(
        paletteName = "invalid",
        primaryColor = "red",
        accentColor = "#ABCDEF"
      ),
      appSettings = list(
        palette_id = "invalid",
        export = list(width = -1, height = 0, dpi = NA, format = "svg")
      ),
      customPalettes = list(Broken = c("blue"))
    )
  )

  expect_equal(prefs$appSettings$palette_id, APP_DEFAULT_PALETTE_ID)
  expect_equal(prefs$appSettings$export, get_default_export_settings())
  expect_equal(prefs$customPalettes, list())
  expect_equal(prefs$theme$paletteName, APP_DEFAULT_PALETTE_ID)
  expect_match(prefs$theme$primaryColor, "^#")
  expect_equal(prefs$theme$accentColor, "#ABCDEF")
})
