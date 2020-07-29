context("Testing text_manipulation_in_scripts")

txt <- paste0('test ', paste(sample(c(rep(" ", 10), letters), 100, replace = TRUE), collapse = ""))
list2env(fakeTextFile(txt = txt), globalenv())

test_that("gsubInOneScript works", {
   gsubInOneScript(fullPath = tmp_path, 'test', 'experiment', userCheck = FALSE)
   changed_object <- readLines(tmp_path)
   expect_equal(length(changed_object), 1)
   expect_equal(nchar(changed_object[1]), 111)
   expect_true(grepl('experiment', changed_object[1]))
}
)


testfiles <- lapply(1:10, function(i) tempfile(fileext = '.R'))
lapply(testfiles, function(testfilepath) {
   txt <- paste0('test ', paste(sample(c(rep(" ", 10), letters), 100, replace = TRUE), collapse = ""))
   cat(txt, file = testfilepath, fill = TRUE)
})

test_that("gsubInOneFolder works", {
   gsubInOneFolder(fullPath = tempdir(), 'test', 'experiment')
   lapply(testfiles, function(tmp_path) {
      changed_object <- readLines(tmp_path)
      expect_equal(length(changed_object), 1)
      expect_equal(nchar(changed_object[1]), 111)
      expect_true(grepl('experiment', changed_object[1]))
   })
})
