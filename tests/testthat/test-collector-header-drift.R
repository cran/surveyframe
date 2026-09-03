# tests/testthat/test-collector-header-drift.R
# Guards against the collector corrupting data when an instrument changes
# mid-study. Writing the header once and then building rows positionally put
# every value under the wrong heading, with no error.
#
# The collector is JavaScript the package never runs, so a test that only read
# the template would pass with the logic wrong. This runs the real doPost()
# against a mock of the Sheets API.

collector_source <- function() {
  p <- system.file("static_survey", "collector_template.gs",
                   package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "collector template not found")
  paste(readLines(p, warn = FALSE), collapse = "\n")
}

# Enough of SpreadsheetApp to run doPost(): a sheet is a list of rows, and
# getRange()/setValues() address it the way Apps Script does, 1-indexed.
sheets_mock <- "
function Sheet(){ this.rows = []; }
Sheet.prototype.getLastRow = function(){ return this.rows.length; };
Sheet.prototype.getLastColumn = function(){
  if (!this.rows.length) return 0;
  return Math.max.apply(null, this.rows.map(function(r){ return r.length; }));
};
Sheet.prototype.appendRow = function(r){ this.rows.push(r.slice()); };
Sheet.prototype.getRange = function(row, col, nRows, nCols){
  var self = this;
  return {
    getValues: function(){
      var out = [];
      for (var i=0;i<nRows;i++){
        var src = self.rows[row-1+i] || [], line = [];
        for (var j=0;j<nCols;j++) line.push(src[col-1+j] !== undefined ? src[col-1+j] : '');
        out.push(line);
      }
      return out;
    },
    setValues: function(vals){
      for (var i=0;i<vals.length;i++){
        var t = row-1+i;
        while (self.rows.length <= t) self.rows.push([]);
        for (var j=0;j<vals[i].length;j++) self.rows[t][col-1+j] = vals[i][j];
        for (var k=0;k<self.rows[t].length;k++) if (self.rows[t][k]===undefined) self.rows[t][k]='';
      }
      return this;
    },
    setFontWeight: function(){ return this; },
    setBackground: function(){ return this; },
    setFontColor:  function(){ return this; }
  };
};
function Spreadsheet(){ this.sheets = {}; }
Spreadsheet.prototype.getSheetByName = function(n){ return this.sheets[n] || null; };
Spreadsheet.prototype.insertSheet = function(n){ this.sheets[n] = new Sheet(); return this.sheets[n]; };
var __ss = new Spreadsheet();
var SpreadsheetApp = { getActiveSpreadsheet: function(){ return __ss; } };
var ContentService = {
  createTextOutput: function(t){ return { setMimeType: function(){ return t; } }; },
  MimeType: { JSON: 'json' }
};
"

# Load the collector with EXPECTED_COLUMNS bound to a given instrument version,
# the way export_google_sheet() fills the template in.
load_collector <- function(ctx, src, columns) {
  filled <- sub("\\{\\{EXPECTED_COLUMNS\\}\\}", jsonlite::toJSON(columns), src)
  filled <- sub("\\{\\{TARGET_SHEET_URL\\}\\}", '""', filled)
  filled <- sub("\\{\\{SHEET_URL_COMMENT\\}\\}", "mock", filled)
  # Each load is scoped in a closure. The template declares SHEET_NAME and
  # EXPECTED_COLUMNS with const, so evaluating a second version at top level in
  # the same context would be a redeclaration error rather than the redeploy it
  # is standing in for.
  ctx$eval(paste0("globalThis.doPost = (function(){\n", filled,
                  "\nreturn doPost;\n})();"))
}

post_response <- function(ctx, values) {
  body <- jsonlite::toJSON(as.list(values), auto_unbox = TRUE)
  ctx$eval(sprintf("doPost({ postData: { contents: %s } });",
                   jsonlite::toJSON(as.character(body), auto_unbox = TRUE)))
}

test_that("an item added mid-collection does not shift existing columns", {
  skip_if_not_installed("V8")
  src <- collector_source()

  ctx <- V8::v8()
  ctx$eval(sheets_mock)

  v1 <- c("respondent_id", "submitted_at", "q_freq", "q_prior")
  # the new item is inserted in the middle, which is what shifts everything
  v2 <- c("respondent_id", "submitted_at", "q_language", "q_freq", "q_prior")

  load_collector(ctx, src, v1)
  post_response(ctx, c(respondent_id = "r1", submitted_at = "t1",
                       q_freq = "Daily", q_prior = "Yes"))

  # instrument gains an item, collector regenerated and redeployed onto the
  # same sheet, which already holds r1 and the v1 header
  load_collector(ctx, src, v2)
  post_response(ctx, c(respondent_id = "r2", submitted_at = "t2",
                       q_language = "Chinese", q_freq = "Weekly",
                       q_prior = "No"))

  header <- ctx$get("__ss.sheets['Responses'].rows[0]")
  r1 <- ctx$get("__ss.sheets['Responses'].rows[1]")
  r2 <- ctx$get("__ss.sheets['Responses'].rows[2]")

  # the new column goes to the right-hand end, so no existing column moves
  expect_identical(header[seq_along(v1)], v1)
  expect_true("q_language" %in% header)

  # every value sits under its own header, for the row collected before the
  # change and the row collected after it
  val <- function(row, col) row[[which(header == col)]]
  expect_identical(val(r1, "q_freq"), "Daily")
  expect_identical(val(r1, "q_prior"), "Yes")
  expect_identical(val(r2, "q_freq"), "Weekly")
  expect_identical(val(r2, "q_prior"), "No")
  expect_identical(val(r2, "q_language"), "Chinese")
})

test_that("a fresh sheet still gets its header written", {
  skip_if_not_installed("V8")
  ctx <- V8::v8()
  ctx$eval(sheets_mock)
  cols <- c("respondent_id", "submitted_at", "q1")
  load_collector(ctx, collector_source(), cols)
  post_response(ctx, c(respondent_id = "r1", submitted_at = "t1", q1 = "a"))

  header <- ctx$get("__ss.sheets['Responses'].rows[0]")
  row <- ctx$get("__ss.sheets['Responses'].rows[1]")
  expect_identical(header, cols)
  expect_identical(row, c("r1", "t1", "a"))
})

test_that("the collector maps by header name rather than position", {
  # A structural guard that runs even without V8, so the property is asserted
  # somewhere unconditionally.
  src <- collector_source()
  expect_true(grepl("readHeader_", src, fixed = TRUE))
  # the positional mapping that caused the corruption
  expect_false(grepl("EXPECTED_COLUMNS.map(col =>", src, fixed = TRUE))
})
