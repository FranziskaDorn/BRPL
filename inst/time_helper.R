# Pfad zu deinem lokalen Paketverzeichnis (ersetzen!)
pkg_path <- "/Users/rkruse/Git/BRPL"

# Alle Dateien rekursiv auflisten
files <- list.files(pkg_path, recursive = TRUE, full.names = TRUE)

# Infos sammeln
file_times <- file.info(files)[, c("mtime")]

# Aktuelle Systemzeit
now <- Sys.time()

# Dateien mit Änderungszeitpunkt in der Zukunft
future_files <- file_times[file_times > now, , drop = FALSE]

# Ergebnis anzeigen
if (nrow(future_files) > 0) {
  cat("⚠️ Dateien mit Zeitstempel in der Zukunft:\n")
  print(future_files)
} else {
  cat("✅ Keine Datei mit zukünftiger Änderungszeit gefunden.\n")
}