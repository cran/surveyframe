# tests/testthat/helper-apps-script.R
#
# A mock of the Sheets API that models how Apps Script actually stores a
# value, for tests that run the real collector JavaScript under V8.
#
# The mock in test-collector-header-drift.R pushes each row into an array
# verbatim. That is enough to prove a column-mapping property and it cannot
# show what Sheets does to the value on the way in: appendRow() and
# setValues() apply user-entered semantics, so a string is parsed the way a
# typed cell would be. A respondent answer of "=1+1" is stored as 2, and a
# participant code of "007" is stored as 7. Modelling that coercion is what
# makes A8 visible.

# Enough of SpreadsheetApp to run doPost(), with user-entered coercion.
apps_script_mock <- "
function coerceUserEntered_(v, fmt){
  // Google documents setValues() as applying user-entered semantics, and makes
  // no promise that the plain-text number format suppresses parsing. The mock
  // used to return the string unchanged for fmt === '@', which assumed exactly
  // the thing under test and made the assertions pass by construction. It now
  // parses regardless of format, so only a write through the documented RAW
  // path can store an answer literally.
  if (typeof v !== 'string') return v;
  if (v.charAt(0) === '=') {                  // Sheets parses the cell
    var expr = v.slice(1);
    if (/^[0-9+\\-*/(). ]+$/.test(expr)) {
      try { return eval(expr); } catch (e) { return '#ERROR!'; }
    }
    return '#FORMULA:' + expr;                // e.g. IMPORTXML, HYPERLINK
  }
  if (/^-?[0-9]+(\\.[0-9]+)?$/.test(v)) return Number(v);  // '007' -> 7
  return v;
}

function Sheet(){ this.rows = []; this.fmt = {}; this.name = 'Responses'; }
Sheet.prototype.getName = function(){ return this.name; };
// Returns the single mock spreadsheet rather than a stored reference: holding
// the parent on the sheet makes __ss circular, and a test reading a sheet back
// through JSON then kills the V8 context outright.
Sheet.prototype.getParent = function(){ return __ss; };
Sheet.prototype.key_ = function(r, c){ return r + ':' + c; };
Sheet.prototype.getLastRow = function(){ return this.rows.length; };
Sheet.prototype.getLastColumn = function(){
  if (!this.rows.length) return 0;
  return Math.max.apply(null, this.rows.map(function(r){ return r.length; }));
};
Sheet.prototype.appendRow = function(r){
  var t = this.rows.length, line = [];
  for (var j=0;j<r.length;j++) line.push(coerceUserEntered_(r[j], this.fmt[this.key_(t+1, j+1)]));
  this.rows.push(line);
};
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
    setNumberFormat: function(f){
      for (var i=0;i<nRows;i++)
        for (var j=0;j<nCols;j++)
          self.fmt[self.key_(row+i, col+j)] = f;
      return this;
    },
    setValues: function(vals){
      for (var i=0;i<vals.length;i++){
        var t = row-1+i;
        while (self.rows.length <= t) self.rows.push([]);
        for (var j=0;j<vals[i].length;j++)
          self.rows[t][col-1+j] = coerceUserEntered_(vals[i][j], self.fmt[self.key_(row+i, col+j)]);
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
Spreadsheet.prototype.insertSheet = function(n){
  var sh = new Sheet(); sh.name = n;
  this.sheets[n] = sh; return sh;
};
Spreadsheet.prototype.getId = function(){ return 'mock-spreadsheet-id'; };
var __ss = new Spreadsheet();

// The Sheets advanced service, which is the documented literal-write path.
// valueInputOption RAW stores a value without parsing it, so this writes the
// string through untouched. __sheetsEnabled lets a test run the fallback too.
var __sheetsEnabled = true;
var __rawWrites = [];
var Sheets = {
  Spreadsheets: {
    Values: {
      update: function(body, spreadsheetId, rangeA1, opts){
        if (!__sheetsEnabled) throw new Error('advanced service disabled');
        __rawWrites.push({ range: rangeA1, option: opts && opts.valueInputOption });
        var m = /^(.*)!A([0-9]+)$/.exec(rangeA1);
        var sheet = __ss.getSheetByName(m[1]);
        var t = parseInt(m[2], 10) - 1;
        while (sheet.rows.length <= t) sheet.rows.push([]);
        // RAW: exactly what was sent, with no coercion at all.
        sheet.rows[t] = body.values[0].slice();
        return { updatedCells: body.values[0].length };
      }
    }
  }
};
var SpreadsheetApp = { getActiveSpreadsheet: function(){ return __ss; } };
var __lock = { waited: 0, released: 0 };
var LockService = { getScriptLock: function(){ return {
  waitLock: function(){ __lock.waited++; },
  releaseLock: function(){ __lock.released++; } }; } };
var ContentService = {
  createTextOutput: function(t){ return { setMimeType: function(){ return t; } }; },
  MimeType: { JSON: 'json' }
};
"

apps_script_context <- function(columns) {
  skip_if_not_installed("V8")
  p <- system.file("static_survey", "collector_template.gs",
                   package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "collector template not found")
  src <- paste(readLines(p, warn = FALSE), collapse = "\n")

  ctx <- V8::v8()
  ctx$eval(apps_script_mock)

  filled <- sub("\\{\\{EXPECTED_COLUMNS\\}\\}", jsonlite::toJSON(columns), src)
  filled <- sub("\\{\\{TARGET_SHEET_URL\\}\\}", '""', filled)
  filled <- sub("\\{\\{SHEET_URL_COMMENT\\}\\}", "mock", filled)
  ctx$eval(paste0("globalThis.doPost = (function(){\n", filled,
                  "\nreturn doPost;\n})();"))
  ctx
}

apps_script_post <- function(ctx, values) {
  body <- jsonlite::toJSON(as.list(values), auto_unbox = TRUE)
  # The collector's own JSON reply is kept, so a test can read what the
  # collector told the respondent as well as what it stored.
  ctx$eval(sprintf("__lastReply = doPost({ postData: { contents: %s } });",
                   jsonlite::toJSON(as.character(body), auto_unbox = TRUE)))
  invisible(ctx)
}

# The collector's JSON reply to the last submission, parsed.
apps_script_reply <- function(ctx) {
  jsonlite::fromJSON(ctx$get("__lastReply"))
}

# The stored cell, as a string, for one column of one collected row.
apps_script_cell <- function(ctx, row, column) {
  header <- ctx$get("__ss.sheets['Responses'].rows[0]")
  stored <- ctx$get(sprintf("__ss.sheets['Responses'].rows[%d]", row))
  as.character(stored[[which(header == column)]])
}
