testthat::test_that("reader-facing source avoids disallowed wording", {
  files <- c(list.files("R", pattern = "\\.R$", full.names = TRUE), "app.R", "README.md",
             list.files("docs", pattern = "\\.md$", full.names = TRUE), "www/app.js")
  text <- unlist(lapply(files, readLines, warn = FALSE))
  blocked <- c("aim", "bolster", "commendable", "delve", "enable", "encompass", "enhance", "ensure",
               "equip", "esteemed", "facilitate", "foster", "friendly", "functionality", "grasp", "guarantee",
               "hone", "influence", "instrumental", "intricate", "invaluable", "journey", "landscape", "leverage",
               "maximize", "meticulous", "multifaceted", "nuance", "perspective", "pivotal", "plethora", "realm",
               "rigor", "robust", "sacrifice", "showcasing", "strive", "technique", "transformative", "tweak",
               "utilize", "vital", "wishlist")
  hits <- character(0)
  for (word in blocked) {
    found <- grep(paste0("\\b", word, "[a-z]*\\b"), text, value = TRUE, ignore.case = TRUE)
    if (length(found)) hits <- c(hits, paste0(word, ": ", found[1]))
  }
  testthat::expect_length(hits, 0, info = paste(hits, collapse = "\n"))
})

testthat::test_that("reader-facing source has no long dashes or contractions", {
  files <- c(list.files("R", pattern = "\\.R$", full.names = TRUE), "app.R", "README.md",
             list.files("docs", pattern = "\\.md$", full.names = TRUE), "www/app.js")
  text <- unlist(lapply(files, readLines, warn = FALSE))
  testthat::expect_false(any(grepl("\u2014|\u2013", text)))
  contractions <- "\\b(it's|don't|can't|won't|you're|we're|they're|isn't|aren't|doesn't|didn't|hasn't|haven't|wouldn't|couldn't|shouldn't|that's|there's|what's|let's|i'm|i've|we've|you've)\\b"
  testthat::expect_false(any(grepl(contractions, text, ignore.case = TRUE)))
})
