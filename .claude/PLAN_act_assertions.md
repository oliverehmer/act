# PLAN: Zentrale Typ-Assertions in act (.assert_corpus / .assert_transcript / .assert_search)

## Ziel

Die ueber das ganze Paket duplizierte Inline-Typpruefung fuer die S4-Klassen
`corpus`, `transcript` (und `search`) durch wenige zentrale, act-lokale interne
Helfer ersetzen. Nebeneffekt: bestehende Inkonsistenzen und einen Bug
vereinheitlichen/beheben.

Analog zu iclos `.assert_corpus`, aber als EIGENE, in act angesiedelte Funktion
(kein Import aus iclo - die Abhaengigkeit ist strikt iclo -> act). act darf kein
iclo-Wissen enthalten; daher KEIN class_load()-Listen-Hinweis wie in iclo.

## Verifizierte Bestandsaufnahme (Stand der Pruefung am Code)

- act/R hat 102 Dateien; 62 enthalten einen corpus/transcript/search-Typcheck.
- KEIN zentraler Assert-Helfer vorhanden (reine Inline-Duplikation).
- Interne Dot-Helfer-Konvention in act: fuehrender Punkt, KEIN helper_-Praefix,
  thematische Datei. Sammeldatei fuer Nicht-Exporte: `helper_no_export.R`
  (z. B. `.detect_os`). -> dort die neuen Helfer ablegen.
- Einrueckung: Tabs.
- Fehlermechanismus durchgaengig `cli::cli_abort()` + `methods::is()`.
- Parameter-Konvention: `x` = corpus, `t` = transcript, `s` = search,
  `l` = layout (layout wird NIRGENDS typgeprueft - bleibt unangetastet).

### WICHTIG: keine echten Schutzluecken
Ein gezielter Gegencheck (Funktionen, die `x@`/`t@` zugreifen, aber keinen
methods::is-Check haben) ergab: KEINE. Jede zugreifende Funktion ist bereits
geschuetzt. Dieser Umbau ist also Deduplizierung + Konsistenz + Bugfix, NICHT
Absturzsicherung (anders als der iclo-Fall).

### Dominantes Muster (kombinierter Einzeiler: missing + Typ)
```r
if (missing(x)) {cli::cli_abort("Corpus object in parameter {.arg x} is missing.")}
  else { if (!methods::is(x,"corpus")) {cli::cli_abort("Parameter {.arg x} needs to be a {.cls corpus} object.")} }
```

### Call-Sites (exakt)

CORPUS (Param x), 40 Standard-Einzeiler:
annotations_all.R:21, annotations_delete.R:22, annotations_delete_empty.R:23,
annotations_insert_from_search_to_tier.R:43*, annotations_matrix.R:30*,
annotations_replace_copy.R:34, corpus_export.R:48*, corpus_import.R:39*,
helper_corpus_export_cured.R:26, helper_transcript_names_get.R:14,
helper_transcript_names_set.R:15, media_assign.R:60, media_delete.R:17,
media_format_names.R:36, search_concordance.R:16, search_cuts.R:34,
search_cuts_media.R:111, search_cuts_printtranscript.R:45, search_cuts_srt.R:35,
search_makefilter.R:34, search_new.R:89, search_openresult_inelan.R:39,
search_openresult_inpraat.R:43, search_openresult_inquicktime.R:38,
search_run.R:23, search_searchandopen_inpraat.R:33, search_sub.R:52,
tiers_add.R:40, tiers_all.R:25, tiers_convert.R:30, tiers_delete.R:24,
tiers_rename.R:29, tiers_sort.R:28, transcripts_add.R:29, transcripts_cure.R:41,
transcripts_delete.R:16, transcripts_filter.R:31, transcripts_rename.R:30,
transcripts_update_fulltexts.R:29, transcripts_update_normalization.R:24
(* = 4 Stellen OHNE `{.cls}`-Styling)

TRANSCRIPT (Param t, ausser annotiert), 18 Stellen:
export_docx.R:84 (ohne {.cls}), export_docx.R:118 ("ERROR:"-Praefix),
export_eaf.R:29, export_edl.R:59, export_exb.R:30, export_rpraat.R:27,
export_srt.R:34, export_textgrid.R:28, export_txt.R:67,
media_get_path_to_existing_file.R:16, search_transcript_content.R:15,
search_transcript_fulltext.R:15, transcripts_cure_single.R:38,
transcripts_filter_remove_single.R:25, transcripts_filter_single.R:42,
transcripts_openin_elan.R:44 (BUG: Meldung sagt {.cls corpus} statt transcript),
annotations_add.R:81 (Param x, ohne {.cls}),
annotations_fill_tier_section.R:50 (Param x, ohne {.cls})

SEARCH (Param s), 14 Stellen:
annotations_insert_from_search_to_tier.R:44 (ohne {.cls}),
search_concordance.R:17, search_cuts.R:35, search_cuts_media.R:112,
search_cuts_printtranscript.R:46, search_cuts_srt.R:36,
search_openresult_inelan.R:40, search_openresult_inpraat.R:44,
search_openresult_inquicktime.R:39, search_results_export.R:40,
search_run.R:24, search_sub.R:53, search_transcript_content.R:16,
search_transcript_fulltext.R:16

SONDERFAELLE (NICHT per einfachem Wrapper ersetzen):
- corpus_merge.R:29-38 - x via Wrapper ersetzbar; `insert` (corpus ODER Liste
  von corpus) bleibt custom.
- transcripts_merge.R:65-87 - `destinationTranscript` (ggf. aus Liste [[1]])
  + `updateTranscripts` (Liste von transcripts). Custom lassen; optional
  `destinationTranscript` nach [[1]] via Wrapper.
- act_info.R:25-40 und act_info_summarized.R - variadisch (`...`), akzeptieren
  corpus ODER transcript mit eigener Meldung. Custom lassen.

### Inkonsistenzen, die durch Vereinheitlichung verschwinden
- 8 Stellen erhalten einheitlich `{.cls}`-Styling (4 corpus, 3 transcript,
  1 search - siehe * oben).
- export_docx.R:118 verliert redundantes "ERROR:"-Praefix.
- transcripts_openin_elan.R:44 BUGFIX: {.cls corpus} -> {.cls transcript}.

## Helfer-Design (in helper_no_export.R, Tabs)

Drei kleine, in sich geschlossene Wrapper. Das `missing`-Argument loest das
R-Problem, dass `missing()` nur in der aufrufenden Funktion ausgewertet werden
kann: der Aufrufer reicht `missing(x)` als Logical herein; bei TRUE bricht der
Helfer ab, BEVOR das Objekt erzwungen wird.

```r
# Assert that `x` is an act corpus object (or abort with a clear message).
.assert_corpus <- function(x, arg = "x", missing = FALSE) {
	if (missing) cli::cli_abort("Corpus object in parameter {.arg {arg}} is missing.")
	if (!methods::is(x, "corpus")) cli::cli_abort("Parameter {.arg {arg}} needs to be a {.cls corpus} object.")
	invisible(TRUE)
}

# Assert that `t` is an act transcript object.
.assert_transcript <- function(t, arg = "t", missing = FALSE) {
	if (missing) cli::cli_abort("Transcript object in parameter {.arg {arg}} is missing.")
	if (!methods::is(t, "transcript")) cli::cli_abort("Parameter {.arg {arg}} needs to be a {.cls transcript} object.")
	invisible(TRUE)
}

# Assert that `s` is an act search object.
.assert_search <- function(s, arg = "s", missing = FALSE) {
	if (missing) cli::cli_abort("Search object in parameter {.arg {arg}} is missing.")
	if (!methods::is(s, "search")) cli::cli_abort("Parameter {.arg {arg}} needs to be a {.cls search} object.")
	invisible(TRUE)
}
```

Hinweis: `{.arg {arg}}` (verschachtelte cli-Interpolation) ist gueltig und in
iclo bereits erprobt. Meldungstexte bleiben wortgleich zum Bestand (nur die 8
Styling-Luecken, das ERROR-Praefix und der elan-Bug werden korrigiert).

## Call-Site-Transformation (Regeln)

- corpus (Param x):         `\t.assert_corpus(x, missing = missing(x))`
- transcript (Param t):     `\t.assert_transcript(t, missing = missing(t))`
- transcript (Param x):     `\t.assert_transcript(x, arg = "x", missing = missing(x))`
- search (Param s):         `\t.assert_search(s, missing = missing(s))`

Jeweils eine Tab-Einrueckung (act-Stil). Wo missing + Typ in EINER Zeile
standen, wird daraus eine Zeile. Wo sie (corpus_merge) getrennt standen, ersetzt
der Wrapper beide.

## Backward-Compatibility / Constraints

- Exportierte Signaturen und Rueckgabewerte bleiben unveraendert -> konform.
- Nur Fehlermeldungs-WORTLAUT/Styling aendert sich (Verhalten/Format) - laut
  CLAUDE.md ausdruecklich erlaubt.
- Keine S4-Klassenaenderung.
- Keine neue Abhaengigkeit; act bleibt frei von iclo.
- Interne Helfer in act unterliegen NICHT der Backward-Compat-Regel.

## Version

act + iclo synchron 3.9.41 -> 3.9.42 (Versionen vor dem Bump live aus beiden
DESCRIPTION lesen, nicht annehmen).

## Entscheidungen (vom Nutzer bestaetigt)

1. Scope: corpus + transcript + `search` (s) - alle drei.
2. corpus_merge / transcripts_merge / act_info: nur den einfachen x/t-Teil
   umstellen, Listen-/variadik-Logik bleibt custom.
3. Voller Durchgang: alle ~72 Call-Sites jetzt.
4. Helfer in `helper_no_export.R` (bestehende Konvention).

## Bei Umsetzung selbst zu verifizieren (kein Nutzer-Input noetig)
- export_docx.R hat ZWEI transcript-Checks (Z. 84 + Z. 118) - pruefen, ob
  gleicher Param `t` oder zweiter transcript-Parameter; beide auf
  `.assert_transcript` umstellen.
- transcripts_merge: nur den `destinationTranscript`-Typcheck (nach `[[1]]`)
  via `.assert_transcript(destinationTranscript, arg = "destinationTranscript")`;
  updateTranscripts-Schleife bleibt custom.

## Umsetzungsschritte - ABGESCHLOSSEN

- [x] 3 Wrapper in helper_no_export.R (Tabs)
- [x] Call-Sites umgestellt (Bulk-Skript, Protokoll): 40 corpus, 16 transcript,
      14 search = 70 Einzeiler
- [x] Sonderfaelle manuell: corpus_merge (x-Teil), transcripts_merge
      (destinationTranscript), annotations_add + annotations_fill_tier_section
      (transcript via Param x, mehrzeilig). act_info/act_info_summarized +
      corpus_merge insert-Liste + transcripts_merge updateTranscripts-Schleife
      bewusst custom belassen.
- [x] Bug behoben: transcripts_openin_elan.R:44 ({.cls corpus} -> transcript)
- [x] Inkonsistenzen behoben: 8x fehlendes {.cls}, "ERROR:"-Praefix in export_docx
- [x] Parse-Check aller R-Dateien OK
- [x] roxygenize("act") - keine Rd-Aenderungen (Helfer dot-intern)
- [x] devtools::check("act"): 0 errors. 2 WARNINGs nur Vignetten-Artefakt des
      --no-vignettes-Laufs (nicht durch Aenderung verursacht). 2 NOTES vorbestehend.
- [x] act 3.9.42 installiert; iclo laedt + funktioniert gegen neue act-Version
- [x] Version-Bump 3.9.41 -> 3.9.42 (act + iclo DESCRIPTION)
- [x] Funktionstest: korrekter Typ -> kein Fehler; falscher Typ + missing ->
      erwartete Meldungen (corpus/transcript/search, Wrapper + konvertierte
      exportierte Funktionen)

## Gegencheck
Nach Umstellung: KEINE verbliebenen Inline-Checks der Standard-Parameter
(x/t/s) ausser den bewusst custom gelassenen. Keine verbliebenen
"needs to be a ... object"-Altmeldungen ausserhalb der Wrapper.
```
