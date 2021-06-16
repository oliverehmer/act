# ============================================================================================
# 3.1 Installation und Beispieldaten
# ============================================================================================

# --------------------------------------------------------------------------------------------
# 3.1.2	Installation der Bibliotheken
# --------------------------------------------------------------------------------------------
install.packages("remotes")
remotes::install_github("oliverehmer/act/act")

#install.packages("act")
#library("act")

install.packages("rstudioapi")
library(rstudioapi)

# --------------------------------------------------------------------------------------------
# --- 3.1.4	Herunterladen des Beispielkorpus
# --------------------------------------------------------------------------------------------
workDir <- rstudioapi::selectDirectory()
temp <- tempfile()
download.file("http://www.romanistik.uni-freiburg.de/ehmer/files/digitalhumanities/act_examplecorpus.zip", temp)
unzip(zipfile=temp, exdir=workDir)

# ============================================================================================
# === 3.2 Das Korpus-Objekt und der Import/Export von Annotationsdateien
# ============================================================================================

# --------------------------------------------------------------------------------------------
# --- 3.2.1 Erstellen eines Korpus-Objekts
# --------------------------------------------------------------------------------------------
library(act, rstudioapi)
corpusDir <- rstudioapi::selectDirectory()
corpus <- act::corpus_new(pathsAnnotationFiles = corpusDir, 
						  pathsMediaFiles = corpusDir)

# --------------------------------------------------------------------------------------------
# --- 3.2.3 Informationen ober das Korpus-Objekt und Zugriff auf die Daten 
# --------------------------------------------------------------------------------------------
corpus
# Zugriff auf die Liste mit allen Transkript-Objekten
corpus@transcripts
# Zugriff auf das erste Transkript-Objekt
corpus@transcripts[[1]]
# Anzeige der Annotationen in einem Transkript-Objekt
View(corpus@transcripts[[1]]@annotations)
# Anzeige der Tiers in einem Transkript-Objekt
View(corpus@transcripts[[1]]@tiers)
# Zugriff auf ein Transkript-Objekt über den Namen
View(corpus@transcripts[['ARG_I_CHI_Santi']]@tiers)

# --------------------------------------------------------------------------------------------
# --- 3.2.4 Zusammenfassungen des Korpus-Objekts
# --------------------------------------------------------------------------------------------
#Zusammenfassung der Annotationen und tiers aus allen Transkripten in einer Tabelle
View(act::annotations_all(corpus))
View(act::tiers_all(corpus))
#Aggregierte Informationen über alle Transkripte und Tiers
View(act::info(corpus)$transcripts)
View(act::info(corpus)$tiers)

#Anzahl Wörter in den Transkripten
info <- act::info(corpus)
par(mar=c(4,16,4,4))
barplot(height=info$transcripts$words.org.count, 
		names.arg=info$transcripts$transcript.name,
		main="Wörter pro Transkript",
		horiz=TRUE,
		las=2)

#Histogramm der Annotationsdauern
par(mar=c(4,4,4,4))
ann <- act::annotations_all(corpus)
ann$durationSec <- ann$endSec - ann$startSec
hist(x=ann$durationSec,
	 breaks=60)

# --------------------------------------------------------------------------------------------
# --- 3.2.5 Verlinken von Mediendateien
# --------------------------------------------------------------------------------------------
#Verlinken von Mediendateien direkt beim Erstellen eines Korpus-Objekts
corpus <- act::corpus_new(pathsAnnotationFiles = corpusDir,
						  pathsMediaFiles	= corpusDir)

#Nachträgliches Verlinken von Mediendateien 
corpus <- act::corpus_new(pathsAnnotationFiles = corpusDir,
						  assignMedia = FALSE)
corpus@transcripts[['SYNC_rotar_y_flexionar']]@media.path

corpus@paths.media.files <- corpusDir
corpus <- act::media_assign(corpus)
corpus@transcripts[['SYNC_rotar_y_flexionar']]@media.path


# ============================================================================================
# 3.3 Export von Annotationsdateien
# ============================================================================================
# Auswahl eines Zielverzeichnisses und Erstellen des Ordners "act_export"
exportDir <- rstudioapi::selectDirectory()

act::corpus_export(x=corpus, 
				   outputFolder = file.path(exportDir, "act_export_1"))

act::corpus_export(x=corpus, 
				   outputFolder = file.path(exportDir, "act_export_2"),
				   formats      = "eaf")

act::corpus_export(x=corpus, 
				   outputFolder          = file.path(exportDir, "act_export_3"),
				   filterTranscriptNames = c('ARG_I_CHI_Santi', 'ARG_I_PAR_Beto'),
				   formats               = "printtranscript")

act::corpus_export(x=corpus, 
				   outputFolder          = file.path(exportDir, "act_export_4"),
				   filterTranscriptNames = 'BOL_CBBA_SoniaYJanette',
				   filterTierNames       = c('ROB', 'FRA'),
				   formats               = c("printtranscript", 'textgrid'))


# ============================================================================================
# 3.4 Erstellung von Drucktranskripten	   
# ============================================================================================
# Erstellen eines Drucktranskripts
printtrans <- act::export_printtranscript(t=corpus@transcripts[['BOL_CBBA_SoniaYJanette']])
cat(printtrans)

#Erstellen eines Layouts und Ausgabe des Drucktranskripts mit verschiedenen Sprechersiglen
meinlayout <- methods::new("layout")
meinlayout
# Anzeigen der Hilfe zum Layout-Objekt mit allen Layout Optionen
?act::`layout-class`

printtrans <- act::export_printtranscript(t=corpus@transcripts[['BOL_CBBA_SoniaYJanette']],
								          l=meinlayout)
cat(printtrans)

# Ändern der Zeichenanzahl für die Sprechersiglen
meinlayout@speaker.width <- 1
printtrans2 <-act::export_printtranscript(t=corpus@transcripts[['BOL_CBBA_SoniaYJanette']],
	                                      l=meinlayout)
cat(printtrans2)


# ============================================================================================
# 3.5 Suche im Korpus
# ============================================================================================

# --------------------------------------------------------------------------------------------
# 3.5.1 Erstellen und Ändern einer Suche
# --------------------------------------------------------------------------------------------
#Erstellen einer neuen Suche
suche <- act::search_new(x=corpus, pattern="yo")
suche
suche@results.nr
View(suche@results)

# Ändern und erneutes Ausführen einer Suche
suche@filter.transcript.includeRegEx  <- 'ARG'
suche <- act::search_run(x=corpus, s=suche)
suche@results.nr
suche@results$transcript.name

# --------------------------------------------------------------------------------------------
# 3.5.2.  Suchmodi und Normalisierung
# --------------------------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.5.2.1 Normalisierte Suche
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
corpus@transcripts[['BOL_CCBA_SP_MeryGaby1']]@annotations$content[6]
corpus@transcripts[['BOL_CCBA_SP_MeryGaby1']]@annotations$content.norm[6]

suche <- act::search_new(x=corpus,
						 pattern='fieras',
						 searchNormalized = FALSE)
suche@results.nr
suche <- act::search_new(x=corpus,
						 pattern='fieras',
						 searchNormalized = TRUE)
suche@results.nr
suche@results$hit[1]
suche@results$content[1]

# Die aktuelle Normalisierungmatrix anzeigen
View(corpus@normalization.matrix)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.5.2.1 Volltextsuche
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suche <- act::search_new(x=corpus,
						 pattern="\\blos\\b.{1,20}\\bque\\b",
						 filterTranscriptNames = 'BOL_CCBA_SP_MeryGaby1',
						 searchMode='content')
suche@results.nr

suche <- act::search_new(x=corpus,
						 pattern="\\blos\\b.{1,20}\\bque\\b",
						 filterTranscriptNames = 'BOL_CCBA_SP_MeryGaby1',
						 searchMode='fulltext')
suche@results.nr
View(suche@results)
suche@results$hit[1]
suche@results$hit.span[1]


# ============================================================================================
# 3.6 Arbeiten mit den Suchergebnissen
# ============================================================================================

# --------------------------------------------------------------------------------------------
# 3.6.1	Erstellung von Ausschnitten
# --------------------------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.6.1.1 Erstellung von Transkriptausschnitten
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suche <- act::search_new(x=corpus,
						 pattern="pero")
suche <- act::search_cuts_printtranscript(x=corpus,
								 s=suche)
cat(suche@cuts.printtranscripts)
cat(suche@results$printtranscript)
cat(suche@results$printtranscript[3])

#Ändern der Breite des Fensters
suche@cuts.span.beforesec <- 1
suche@cuts.span.aftersec <- 3
suche <- act::search_cuts_printtranscript(x=corpus, s=suche)
cat(suche@results$printtranscript[3])

#Speichern der Transkriptausschnitte in einen Ordner
suche@name <- 'act_search_pero'
searchDir <- rstudioapi::selectDirectory()
suche <- act::search_cuts_printtranscript(x=corpus,
										  s=suche, 
										  outputFolder=searchDir)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.6.1.2 Erstellung von Medienausschnitten
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suche <- act::search_cuts_media(x=corpus,
							    s=suche, 
							    outputFolder=searchDir)
cat(suche@cuts.cutlist.mac)
cat(suche@cuts.cutlist.win)
cat(suche@results$cuts.cutlist.mac[3])
cat(suche@results$cuts.cutlist.win[3])

# Ausführen der Schnittbefehle direkt aus RStudio
termId = rstudioapi::terminalExecute(suche@results$cuts.cutlist.mac[3])
rstudioapi::terminalKill(termId)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.6.1.3 Erstellen von Transkript-, Medien- und Untertitelausschnitten
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outputDir <- rstudioapi::selectDirectory()
suche.1PSG <- act::search_new(x=corpus,
							  pattern="\\byo\\b",
							  name= "act_search_Pron-1PSG")

suche.1PSG <- act::search_cuts(x                = corpus,
							   s                = suche.1PSG,
							   cutSpanBeforesec = 1,
					           cutSpanAftersec  = 3,
					           outputFolder     = outputDir)

#unter macOS:
termId = rstudioapi::terminalExecute(suche.1PSG@cuts.cutlist.mac)	
#unter Windows:
termId = rstudioapi::terminalExecute(suche.1PSG@cuts.cutlist.win)	
rstudioapi::terminalKill(termId)

# --------------------------------------------------------------------------------------------
# 3.6.2 Export und Re-Import Suchergebnissen
# --------------------------------------------------------------------------------------------
suche <- act::search_new(x=corpus, pattern="\\bpero\\b")
path <- rstudioapi::selectFile(existing = FALSE)
if(tolower(tools::file_ext(path))!="xlsx"){path <- paste(path, "xlsx", sep=".")}
act::search_results_export(suche, path=path)

suche.reimport <- act::search_results_import(path=path)
View(suche.reimport@results)

# --------------------------------------------------------------------------------------------
# 3.6.3 Kookkurrenz-Suche
# --------------------------------------------------------------------------------------------
pattern.laugh <- "\\br[ií]e\\w*"
suche <- act::search_new(x=corpus, 
						 pattern=pattern.laugh,
						 searchNormalized=FALSE)
suche <- act::search_sub(x=corpus,
						 s=suche,
						 pattern=pattern.laugh,
						 searchNormalized=FALSE)
View(suche@results)

# --------------------------------------------------------------------------------------------
# 3.6.4 Öffnen von Suchergebnissen in anderen Programmen
# --------------------------------------------------------------------------------------------
suche <- act::search_new(x=corpus, pattern="\\bpero\\b")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.6.4.1 ELAN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(act.path.elan=rstudioapi::selectFile())
act::search_openresult_inelan(x=corpus, s=suche, resultNr = 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.6.4.2 Praat
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(act.path.praat=rstudioapi::selectFile(label="Select Praat executable"))
options(act.path.sendpraat=rstudioapi::selectFile(label="Select sendpraat executable"))
act::search_openresult_inpraat(x=corpus, s=suche, resultNr = 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.6.4.3 Quicktime
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
act::search_openresult_inquicktime(x=corpus, s=suche, resultNr = 1)

suche <- act::search_new(x=corpus, pattern="\\bpero\\b", cutSpanBeforesec = 1, cutSpanAftersec = 3)
suche <- act::search_cuts_printtranscript(x=corpus, s=suche)
act::search_playresults_inquicktime(x=corpus, s=suche)

# ============================================================================================
# 3.7 Interoperabilität mit der rPraat-Bibliothek 
# ============================================================================================
install.packages("rPraat")
library(rPraat)
rPraatTextgrid <- act::export_rpraat(corpus@transcripts[[1]])
rPraat::tg.plot(rPraatTextgrid)


# ============================================================================================
# 3.8 Bearbeiten von Daten
# ============================================================================================
# --------------------------------------------------------------------------------------------
# 3.8.1 Bearbeiten von Annotationen
# --------------------------------------------------------------------------------------------
#Löschen von Annotation, die einem Suchmuster entsprechen
corpus2 <- act::annotations_delete(x=corpus, 
								   pattern = "\\bpero\\b")
#Hier können die Annotationen eines Transkripts verglichen werden, das verändert wurde.
View(corpus@transcripts[['ARG_I_PER_Alejo']]@annotations)
View(corpus2@transcripts[['ARG_I_PER_Alejo']]@annotations)


#Ersetzen eines Suchmusters im Annotationstext
corpus2 <- act::annotations_replace_copy(x=corpus, 
										 pattern = "\\bpero\\b",
										 replacement = "XXX")
#In den Zeilen 8 und 9 wurden Ersetzungen vorgenommen
View(corpus2@transcripts[['ARG_I_PER_Alejo']]@annotations)


#Kopieren eines Suchmusters von einer Annotationsschicht in eine andere
corpus2 <- act::annotations_replace_copy(x=corpus, 
										 pattern = "\\bpero\\b",
										 destTier="sequence")
#Es wurden zwei neue Annotationen in der Schicht "sequence" hinzugefügt.
View(corpus2@transcripts[['ARG_I_PER_Alejo']]@annotations)

corpus2@history

# --------------------------------------------------------------------------------------------
# 3.8.2 Bearbeiten von Annotationsschichten ('tiers')
# --------------------------------------------------------------------------------------------
#Hinzufügen eines Tiers in allen Transkript-Objekten
corpus2 <- act::tiers_add(x=corpus, tierName="TEST")
corpus@transcripts[[1]]@tiers
corpus2@transcripts[[1]]@tiers

#Umsortieren der Tiers in allen Transkript-Objekten
corpus2 <- act::tiers_sort(x=corpus2, 
						   sortVector=c("Entrevistador", "TEST"))
corpus2@transcripts[[1]]@tiers
corpus2@transcripts[[2]]@tiers

#Umbenennen von Tiers
corpus2 <- act::tiers_rename(x=corpus2, 
							 searchPattern="TEST", 
							 searchReplacement="XXX")
corpus2@transcripts[[1]]@tiers

#Löschen von Tiers
corpus2 <- act::tiers_delete(corpus2, tierNames="XXX")
corpus2@transcripts[[1]]@tiers

# --------------------------------------------------------------------------------------------
# 3.8.3 Bearbeiten von Transkript-Objekten
# --------------------------------------------------------------------------------------------

# Umbenennen von Transkripten
corpus2 <-act::transcripts_rename(x = corpus,
								  searchPatterns = c("ARG_", "BOL_"),
								  searchReplacements = c("Argentina_", "Bolivia_"))
act::info_summarized(corpus)$transcript.names
act::info_summarized(corpus2)$transcript.names

#Löschen von Transkripten
corpus2 <- act::transcripts_delete(x = corpus, 
								   transcriptNames = c('ARG_I_CHI_Santi', 'ARG_I_PAR_Beto'))
act::info_summarized(corpus2)$transcript.names

#Hinzufügen von Transkripten
transkripte <- corpus@transcripts[1:2]
corpus2<-act::transcripts_add(x = corpus2, transkripte)
act::info_summarized(corpus2)$transcript.names

# --------------------------------------------------------------------------------------------
# 3.8.4 Zusammenführen der Inhalte von Transkript-Objekten
# --------------------------------------------------------------------------------------------

# Einfügen der in 'transUpdate' markierten Bereiche in 'transDestination'
corpus2 <- act::transcripts_merge(x=examplecorpus,
								  destinationTranscriptName='update_destination', 
								  updateTranscriptNames = 'update_update1')

View(corpus@transcripts[['update_destination']]@annotations)
View(corpus@transcripts[['update_update1']]@annotations)
View(corpus2@transcripts[['update_destination']]@annotations)

act::info_summarized(corpus)$transcript.names
act::info_summarized(corpus2)$transcript.names

#Mehrere Update Transkripte verwenden
corpus2 <- act::transcripts_merge(x=examplecorpus,
							   destinationTranscriptName='update_destination', 
							   updateTranscriptNames = c('update_update1','update_update2'))
View(corpus2@transcripts[['update_destination']]@annotations)


#Speichern des neuen Transkript-Objekts in eine .TextGrid Datei
path <- rstudioapi::selectFile(existing=FALSE)
if(tolower(tools::file_ext(path))!="textgrid"){path <- paste(path, "TextGrid", sep=".")}
act::export_textgrid(t=corpus2@transcripts[['update_destination']], outputPath=path)

# ============================================================================================
# 3.9 Erstellen von Filtern
# ============================================================================================
filters <- act::search_makefilter(x                           = corpus, 
				                  filterTranscriptIncludeRegEx="ARG")
filters
filters$transcript.names
filters$tier.names

act::search_makefilter(x                            = corpus, 
				       filterTierIncludeRegEx       = "A")

act::search_makefilter(x                            = corpus, 
				       filterTierNames              = c("A","B"))

act::search_makefilter(x                            = corpus, 
			           filterTranscriptIncludeRegEx = "ARG",
				       filterTierIncludeRegEx       = "A")

act::search_makefilter(x                            = corpus, 
				       filterTranscriptExcludeRegEx = "ARG",
				       filterTierIncludeRegEx       = "A")

