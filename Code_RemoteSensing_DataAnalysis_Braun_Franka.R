################################################################################
#                                M5 CROPPING                                   #
################################################################################
#Pakete laden
library(sf)        
library(terra)     
library(dplyr)
library(raster)
library(caret)

# Verzeichnis mit den TIFF-Dateien
input_dir <- 'C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData'
output_dir <- 'C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/cropped'

# Shapefile laden
shp <- vect('C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/M5_vorläufig.shp')
shp <- project(shp, crs(test))

# Alle TIFF-Dateien im einlesen
tif_files <- list.files(input_dir, pattern = '\\.tif$', full.names = TRUE)

# Schleife über alle TIFF-Dateien
for (file in tif_files) {
  # Raster laden
  r <- rast(file)
  
  if (!compareCRS(r, shp)) {
    r <- project(r, crs(shp))
  }
  
  # Zuschneiden und maskieren
  r_crop <- crop(r, shp)
  r_mask <- mask(r_crop, shp)
  
  date_part <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", basename(file))  # Extrahiert "2024-03-04"
  output_file <- file.path(output_dir, paste0(date_part, ".tif"))
  
  writeRaster(r_mask, output_file, overwrite = TRUE)
}

test <- rast('merged_2017-09-03.tif')
plot(test)

# Datum aus den Dateinamen extrahieren
dates <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", basename(tif_files))

# Daten als Dataframe speichern
df <- data.frame(Datum = dates)

# Als CSV-Datei speichern
write.csv(df, "extrahierte_daten.csv", row.names = FALSE)

################################################################################
#                                NDVI tiffs                                    #
################################################################################
library(terra)

# Ordnerpfad laden
input_folder <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/cropped"
output_folder <- "E:/NDVI_cropped"

# Liste aller TIFF-Dateien im Ordner
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Schleife über alle TIFF-Dateien
for (file in tif_files) {
  
  # Extrahieren der Dateinamen 
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Extrahieren des Jahres
  year <- substr(file_name, 1, 4)  # Annahme: Jahr steht am Anfang des Dateinamens
 
  raster_stack <- tryCatch({
    rast(file)
  }, error = function(e) {
    cat("Fehler beim Laden der Datei:", file_name, "- Datei wird übersprungen.\n")
    return(NULL)  
  })
  
  if (is.null(raster_stack)) {
    next
  }
  
  red_band <- raster_stack[[3]]   # Band 3 (rot)
  nir_band <- raster_stack[[4]]   # Band 4 (NIR)
  
  #NDVI: (NIR - ROT) / (NIR + ROT)
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  
  output_file <- file.path(output_folder, paste0(file_name, "_NDVI.tif"))
  
  writeRaster(ndvi, output_file, overwrite = TRUE)
  
  cat("NDVI gespeichert für:", file_name, "\n")
}


################################################################################
#                  calculating missing tiffs an NA Values                      #
################################################################################
# Pakete laden
library(lubridate)
library(dplyr)
library(ggplot2)

# Pfad zu den Bilddaten
img_path <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/cropped"

# Alle Dateinamen im Verzeichnis einlesen
files <- list.files(img_path)

# Datumsmuster extrahieren 
date_matches <- regmatches(files, regexpr("\\d{4}-\\d{2}-\\d{2}", files))
dates_available <- ymd(date_matches)

# Entfernen ungültiger Datumsangaben 
dates_available <- dates_available[!is.na(dates_available)]

# Erstellen einer Zeitreihe 
ideal_dates <- seq(from = as.Date("2017-10-03"), to = as.Date("2024-07-31"), by = "5 days")

# Entfernen aller Daten aus August und September
ideal_dates <- ideal_dates[!month(ideal_dates) %in% c(8, 9)]

# Fehlende Daten ermitteln
missing_dates <- setdiff(ideal_dates, dates_available)

# Ausgabe der fehlenden Daten als Date-Objekte
missing_dates_formatted <- as.Date(missing_dates, origin = "1970-01-01")
print(missing_dates_formatted)

# Gruppieren der fehlenden Daten nach Jahr und Monat
missing_per_month <- data.frame(
  year = year(missing_dates_formatted),
  month = month(missing_dates_formatted, label = TRUE, abbr = FALSE)
) %>%
  count(year, month) %>%
  arrange(year, month)

# Deutschen zu Englischen Monatsnamen
month_translation <- c(
  "Januar" = "January", "Februar" = "February", "März" = "March", "April" = "April", 
  "Mai" = "May", "Juni" = "June", "Juli" = "July", "August" = "August", 
  "September" = "September", "Oktober" = "October", "November" = "November", "Dezember" = "December"
)

# Erstellen einer leeren DataFrame-Kopie von den Daten
missing_per_month$month_english <- month_translation[missing_per_month$month]

# Monatsnamen in Monatszahlen umwandeln (diese Werte sind von 1-12, also korrekt für Datum)
missing_per_month$month_num <- match(missing_per_month$month_english, month.name)

# Kombinieren Jahr und Monat zu einem Datum im Format YYYY-MM-DD
missing_per_month$date <- as.Date(paste(missing_per_month$year, missing_per_month$month_num, "01", sep = "-"), format = "%Y-%m-%d")

# Manuelle Liste der englischen Monatsabkürzungen
month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Erstellen der DataFrame-Spalte für englische Monatsabkürzungen
missing_per_month$month_abbr <- month_abbr[missing_per_month$month_num]

# Kombinieren Jahr und Monatsabkürzung
missing_per_month$month_year <- paste(missing_per_month$month_abbr, missing_per_month$year)

# Erstellen des Balkendiagramm
barplot(missing_per_month$n, 
        names.arg = missing_per_month$month_year,  # Verwende manuell erstellte Monatsabkürzungen und Jahre
        las = 2,  # Rotiert die x-Achsen-Beschriftungen
        col = "steelblue", 
        main = "Missing Images per Month", 
        xlab = "", 
        ylab = "Number of Missing Images",
        cex.names = 0.7,  # Reduziert die Schriftgröße der x-Achsen-Beschriftungen
        space = 0.7)  # Erhöht den Abstand zwischen den Balken (standardmäßig 0, hier auf 0.5 gesetzt)

########
#verfügbare bilder pro monat
# Manuelle Liste der englischen Monatsabkürzungen
month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Erstelle eine vollständige Zeitreihe von Bildern alle 5 Tage, beginnend am 3. Oktober 2017
start_date <- as.Date("2017-10-03")
end_date <- as.Date("2024-07-31")

# Alle 5 Tage ein Datum in dieser Zeitspanne erstellen
dates <- seq.Date(from = start_date, to = end_date, by = "5 days")

# Entferne die Monate August und September aus der Zeitreihe
dates <- dates[!(format(dates, "%m") %in% c("08", "09"))]

# Lese alle Daten im Ordner
folder_path <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/cropped"  # Pfad zum Ordner
file_dates <- list.files(folder_path, full.names = TRUE)  # Liste der Dateipfade im Ordner

# Extrahiere das Datum aus den Dateinamen 
file_dates <- gsub(".*/(\\d{4}-\\d{2}-\\d{2}).*", "\\1", file_dates)
file_dates <- as.Date(file_dates)

# Umwandlung der verfügbaren Daten in Datumsformat
available_dates <- as.Date(available_dates, origin = "1970-01-01")

# Formatieren der Daten in year-month
available_per_month <- format(available_dates, "%Y-%m")

# Zählen der Anzahl der verfügbaren Bilder pro Monat
available_per_month_table <- table(available_per_month)

# Umwandeln der Tabelle in einen DataFrame
available_per_month_df <- as.data.frame(available_per_month_table)

# Umbenennen der Spalten
colnames(available_per_month_df) <- c("month_year", "available_images")

# Umwandeln der Monatsangaben in "Date" Format, um sicherzustellen, dass wir das Jahr und den Monat korrekt extrahieren
available_per_month_df$month_year <- as.Date(paste0(available_per_month_df$month_year, "-01"))

# Setze Locale auf Englisch für die Monatsabkürzungen
Sys.setlocale("LC_TIME", "C")

# Erstellen der englischen Monatsabkürzungen
available_per_month_df$month_abbr <- format(available_per_month_df$month_year, "%b")


# Kombinieren von Jahr und englischer Monatsabkürzung
available_per_month_df$year_month_label <- paste(format(available_per_month_df$month_year, "%Y"), available_per_month_df$month_abbr, sep = "-")

# Erstelle das Balkendiagramm
barplot(available_per_month_df$available_images, 
        names.arg = available_per_month_df$year_month_label,
        las = 2,
        col = "steelblue",
        main = "Available Images per Month", 
        xlab = "", 
        ylab = "Number of Available Images",
        cex.names = 0.7,
        space = 0.5)

################################################################################
#                            Interpolation  LINEAR                             #
################################################################################
library(terra)
library(dplyr)

# Verzeichnisse
ndvi_dir <- "E:/test"
output_dir <- "E:/test_interpolated"
dir.create(output_dir, showWarnings = FALSE)

# NDVI-Dateien & Daten extrahieren
ndvi_files <- list.files(ndvi_dir, pattern = "*.tif", full.names = TRUE)
extract_date <- function(filename) {
  date_str <- gsub("_NDVI.tif", "", basename(filename))
  as.Date(date_str, format = "%Y-%m-%d")
}
dates <- as.Date(sapply(ndvi_files, extract_date))

# Komplette Zeitreihe 
full_dates <- seq(min(dates), max(dates), by = "5 days")

# NDVI-Rasterstack laden
ndvi_stack <- rast(ndvi_files)

# Leerer Rasterstack mit der vollen Zeitreihe
full_ndvi_stack <- rast(ndvi_stack, nlyr = length(full_dates))

# Zeitachse setzen
time(full_ndvi_stack) <- full_dates

# Interpolation pro Pixel
interpolated_stack <- app(ndvi_stack, function(x) {
  valid_values <- na.omit(x)
  if (length(valid_values) < 2) {
    return(rep(NA, length(full_dates)))
  } else {
    return(approx(dates[!is.na(x)], valid_values, xout = full_dates, method = "linear", rule = 2)$y)
  }
})

# Speichern
for (t in 1:nlyr(interpolated_stack)) {
  output_filename <- file.path(output_dir, paste0(format(full_dates[t], "%Y-%m-%d"), "_NDVI.tif"))
  writeRaster(interpolated_stack[[t]], output_filename, overwrite = TRUE)
}


################################################################################
#                           Savitzky Golay Filter                              #
################################################################################
library(raster)
library(signal)  

# Pfad zum NDVI-Ordner
input_folder <- "E:/test_interpolated"

# Alle NDVI-Dateien einlesen
tiff_files <- list.files(input_folder, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}_NDVI\\.tif$", full.names = TRUE)
dates <- as.Date(sub("_NDVI\\.tif$", "", basename(tiff_files)), format="%Y-%m-%d")

# Sortieren nach Datum
tiff_files <- tiff_files[order(dates)]
dates <- dates[order(dates)]

# RasterStack mit allen NDVI-Bildern
ndvi_stack <- stack(tiff_files)

# NDVI-Matrix efnxtrahieren (Pixel x Zeitschritte)
ndvi_matrix <- getValues(ndvi_stack)
total_pixels <- nrow(ndvi_matrix)
total_layers <- ncol(ndvi_matrix)

# Leere Matrix für geglättete Werte
smooth_ndvi_matrix <- matrix(NA, nrow = total_pixels, ncol = total_layers)

# Optimierte Parameter für Savitzky-Golay
sgolay_n <- 9  # Fenstergröße
sgolay_p <- 2  # Polynomgrad 

# Glättung für jeden Pixel
for (i in 1:total_pixels) {
  if (i %% 100000 == 0 || i == 1) {
    cat(sprintf("Bearbeite Pixel %d von %d (%.2f%% abgeschlossen)\n", i, total_pixels, (i / total_pixels) * 100))
    flush.console()
  }
  
  ts <- as.numeric(ndvi_matrix[i, ])
  
  if (!all(is.na(ts))) {

    smooth_ndvi_matrix[i, ] <- sgolayfilt(ts, p = sgolay_p, n = sgolay_n)
  }
}

# Gespeicherte Raster erstellen
output_folder <- "E:/test_interpolated_linear_smoothed"
dir.create(output_folder, showWarnings = FALSE)

for (layer_idx in 1:total_layers) {
  smooth_layer <- raster(ndvi_stack)
  values(smooth_layer) <- smooth_ndvi_matrix[, layer_idx]
  
  date_str <- as.character(dates[layer_idx])
  output_path <- file.path(output_folder, paste0(date_str, "_NDVI_smoothed.tif"))
  
  writeRaster(smooth_layer, output_path, format = "GTiff", overwrite = TRUE)
  
  flush.console()
}


################################################################################
#                       NDVI time series (clostest reference)                  #
################################################################################
library(terra)
library(parallel)
library(raster)

# NDVI-Zeitreihe laden
ndvi_folder <- "E:/test_interpolated_linear_smoothed"
ndvi_files <- list.files(ndvi_folder, pattern = "\\d{4}-\\d{2}-\\d{2}_NDVI_smoothed\\.tif$", full.names = TRUE)
ndvi_stack <- rast(ndvi_files)

# Weizen-Referenzdaten
weizen_dates <- as.Date(c(
  "2018-10-02", "2018-10-20", "2018-11-02", "2018-11-11", "2018-11-24", "2018-12-02",
  "2019-01-05", "2019-01-25", "2019-02-06", "2019-03-05", "2019-03-15", "2019-03-22",
  "2019-04-08", "2019-04-14", "2019-05-02", "2019-05-11", "2019-06-07", "2019-06-17",
  "2019-06-29", "2019-07-14", "2019-07-22", "2019-07-29"
))

weizen_ndvi <- c(0.1, 0.1, 0.12, 0.15, 0.23, 0.3, 0.5, 0.55, 0.6, 0.75, 0.74, 0.74, 
                 0.65, 0.44, 0.22, 0.17, 0.14, 0.11, 0.1, 0.1, 0.1, 0.09)

# NDVI-Bilddaten extrahieren
image_dates <- as.Date(gsub(".*(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", basename(ndvi_files)))

# Funktion zur Bestimmung des nächsten Weizen-Referenzdatums
find_closest_weizen_date <- function(date, weizen_dates) {
  date_md <- format(as.Date(date), "%m-%d")
  weizen_md <- format(weizen_dates, "%m-%d")
  diffs <- abs(as.numeric(as.Date(paste0("2000-", weizen_md)) - as.Date(paste0("2000-", date_md))))
  closest_idx <- which.min(diffs)
  return(weizen_dates[closest_idx])  
}

# Funktion zur Berechnung der Ähnlichkeit für jedes Pixel
correlate_pixelwise <- function(ndvi_image, weizen_ref_ndvi, tolerance = 0.1) {
  ndvi_values <- values(ndvi_image)  
  valid_pixels <- !is.na(ndvi_values)  
  similarity_matrix <- rep(NA, length(ndvi_values))  
  similarity_matrix[valid_pixels] <- abs(ndvi_values[valid_pixels] - weizen_ref_ndvi) <= tolerance
  return(similarity_matrix)
}

# Anwendung auf jedes Bild in der Liste
weizen_similarity_list <- lapply(seq_along(ndvi_files), function(index) {
  image_date <- image_dates[index]
  image_name <- basename(ndvi_files[index])
  cat("\nVerarbeitung von Bild:", image_name, "\n")
  cat("Bilddatum:", format(image_date, "%Y-%m-%d"), "\n")
  closest_weizen_date <- find_closest_weizen_date(image_date, weizen_dates)
  cat(" Nächstes Weizen-Referenzdatum:", format(closest_weizen_date, "%Y-%m-%d"), "\n")
  weizen_ref_ndvi <- weizen_ndvi[which.min(abs(weizen_dates - closest_weizen_date))]
  cat(" Weizen-Referenz-NDVI:", weizen_ref_ndvi, "\n")
  similarity <- correlate_pixelwise(ndvi_stack[[index]], weizen_ref_ndvi, tolerance = 0.1)
  cat("Ähnlichkeit berechnet\n")
  return(similarity)
})

# Umwandlung der Weizen-Ähnlichkeitsliste in georeferenzierte Spatial Raster
weizen_similarity_rasters <- lapply(weizen_similarity_list, function(similarity_vector) {
  similarity_raster <- rast(nrows = nrow(ndvi_stack), ncols = ncol(ndvi_stack))
  values(similarity_raster) <- similarity_vector
  ext(similarity_raster) <- ext(ndvi_stack)  
  crs(similarity_raster) <- crs(ndvi_stack)  
  return(similarity_raster)
})
plot(weizen_similarity_rasters[[1]]) #test

# Setze den Schwellenwert für die 75% Übereinstimmung
threshold <- 0.8 * length(weizen_similarity_rasters)

# Konvertieren aller NA-Werte in den Ähnlichkeitsrastern zu 0 für die Summierung
output_dir <- "E:/weizen_similarity_cleaned"
dir.create(output_dir, showWarnings = FALSE)
for (i in seq_along(weizen_similarity_rasters)) {
  cleaned_raster <- ifel(is.na(weizen_similarity_rasters[[i]]), 0, weizen_similarity_rasters[[i]])
  # speichern & direkt aus RAM löschen
  writeRaster(cleaned_raster, filename = file.path(output_dir, paste0("cleaned_", i, ".tif")), overwrite = TRUE)
  rm(cleaned_raster)
  gc()  
}

# Neuer Pfad zu den bereinigten Dateien
cleaned_folder <- "E:/weizen_similarity_cleaned"
# Liste der bereinigten Raster-Dateien abrufen
cleaned_files <- list.files(cleaned_folder, pattern = "cleaned_.*\\.tif$", full.names = TRUE)
# Die Raster erneut als Spatial Raster-Objekte laden
weizen_similarity_rasters_clean <- lapply(cleaned_files, rast)

# Summieren für jedes Pixel die Anzahl der Bilder mit Wert 1 (Treffer als Weizen)
pixel_count_raster <- Reduce("+", weizen_similarity_rasters_clean)
# Berechnen des finalen Klassifikationsraster -> Pixel mit genügend Treffern werden als Weizen gesetzt
final_classification <- ifel(pixel_count_raster >= threshold, 1, 0)
# Plot des finalen Klassifikationsrasters
plot(final_classification, main = "Weizen-Klassifikation (fix: keine unnötigen NA-Werte)")

# Entferne alle 0-Werte, ersetze sie durch NA
final_classification <- ifel(final_classification == 0, NA, final_classification)
# Speichern als TIF-Datei
writeRaster(final_classification, "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/maske_2324_linear_sg0390.tif", overwrite = TRUE)

################################################################################
#                            mask + landcover overlay                          #
################################################################################
# Pakete laden
library(terra)

# Pfade definieren
landcover_path <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/cropped_idlib_LandCover.tif"
mask_path <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/maske_2324_linear_sg0390.tif"

# ️ Rasterdaten laden
landcover_raster <- rast(landcover_path)
mask_raster <- rast(mask_path)

#  crop-wert definieren nach Landcover Classification 
crop_value <- 40  

# Alles außer Crop zu NA machen
landcover_crop_only <- ifel(landcover_raster == crop_value, landcover_raster, NA)

# Maske räumlich anpassen (damit gleiche Abdeckung)
mask_resampled <- resample(mask_raster, landcover_crop_only, method = "near")

# Maske auf Landcover anwenden (nur Crop-Flächen bleiben erhalten)
final_landcover <- mask(landcover_crop_only, mask_resampled)
plot(final_landcover)

# Ergebnis speichern
writeRaster(final_landcover, "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/maske_2324_linear_sg0390.tif", overwrite = TRUE)

################################################################################
#                                mask CROPPING                                 #
################################################################################

#pakete laden
library(terra)
library(ggplot2)
library(dplyr)

# Pfade
mask_path <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData/maske_2324_linear_sg0390.tif"
ndvi_folder <- "E:/test_interpolated_linear_smoothed"

# NDVI-Dateien einlesen
ndvi_files <- list.files(ndvi_folder, pattern = "\\d{4}-\\d{2}-\\d{2}_NDVI_smoothed\\.tif$", full.names = TRUE)
ndvi_dates <- as.Date(gsub(".*(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", basename(ndvi_files)))

# Weizenmaske laden
weizen_maske <- rast(mask_path)

# Sicherstellen, dass Maske binär ist (1 = Weizen, NA = Nicht-Weizen)
weizen_maske[weizen_maske == 0] <- NA  
weizen_maske[weizen_maske > 0] <- 1    

# Berechnung des mittleren NDVI unter der Weizenmaske
mean_ndvi_values <- sapply(ndvi_files, function(file) {
  ndvi_raster <- rast(file)
  
  # NDVI auf Extent der Maske zuschneiden
  ndvi_crop <- crop(ndvi_raster, ext(weizen_maske))
  
  # NDVI auf gleiche Auflösung und Gitterpunkte resamplen
  ndvi_resampled <- resample(ndvi_crop, weizen_maske, method = "bilinear")
  
  # Maske auf den NDVI anwenden (nur Weizenflächen behalten)
  ndvi_masked <- mask(ndvi_resampled, weizen_maske)
  
  # Mittleren Wert berechnen (ohne NA)
  mean(values(ndvi_masked), na.rm = TRUE)
})

# Dataframe für ggplot
ndvi_df <- data.frame(Datum = ndvi_dates, NDVI = mean_ndvi_values)

# Festlegen des Start- und Enddatums
start_date <- as.Date("2023-10-07")
end_date <- as.Date("2024-07-28")
# Datenrahmen filtern
ndvi_df_filtered <- ndvi_df %>%
  filter(Datum >= start_date & Datum <= end_date)
# Plot der NDVI-Kurve mit gefilterten Daten
Sys.setlocale("LC_TIME", "C")  # Setzt englische Monatsnamen
ggplot(ndvi_df_filtered, aes(x = Datum, y = NDVI)) +
  geom_line(color = "#92a500", linewidth = 1.15) +
  geom_point(color = "#b5ea09") +
  labs(title = "NDVI Time Series (2023/2024 Season)", x = "Date", y = "Mean NDVI") +
  theme_minimal()

# Ergebnis speichern
write.csv(ndvi_df, "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/Ergebnisse (Grafiken)/NDVI2324.csv", row.names = FALSE)

################################################################################                                 # 
#                              Detecting Outliers                              #
################################################################################
################################################################################
#                         NDVI Time Series einlesen                            #
################################################################################
# Benötigte Pakete
library(readxl)
library(dplyr)
library(ggplot2)

# Pfad zur Datei
file_path <- "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/Ergebnisse (Grafiken)/All NDVI Time Series.xlsx"
sheet_names <- c("AllData")

# Leerer DataFrame zum Speichern der Daten
all_ndvi_data <- data.frame()

# Lese jedes Blatt ein und füge es zum DataFrame hinzu
for(sheet in sheet_names) {
  ndvi_df <- read_excel(file_path, sheet = sheet, col_names = FALSE)[, 1:2]

  ndvi_df <- ndvi_df %>%
    rename(Datum = 1, NDVI = 2) %>%  
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"), 
           NDVI = as.numeric(NDVI),  
           Jahr = sheet)  
  
  all_ndvi_data <- bind_rows(all_ndvi_data, ndvi_df)
}

head(all_ndvi_data)


all_ndvi_data <- all_ndvi_data %>%
  filter(!is.na(Datum) & !is.na(NDVI))  

## Datum konvertieren und Season definieren
all_ndvi_data <- all_ndvi_data %>%
  mutate(
    Datum = as.Date(sub("\\.tif\\.tif$", "", Datum)),  
    Jahr = format(Datum, "%Y"),
    Monat = as.integer(format(Datum, "%m")), 
    Season = case_when(
      Monat >= 10 ~ paste0(as.integer(format(Datum, "%Y")), "/", as.integer(format(Datum, "%Y")) + 1), 
      Monat <= 7 ~ paste0(as.integer(format(Datum, "%Y")) - 1, "/", as.integer(format(Datum, "%Y")))  
    Season = factor(Season, levels = unique(Season)) 
  ))

head(all_ndvi_data)

# Neue Monatsvariable für die korrekte Position auf der x-Achse
all_ndvi_data <- all_ndvi_data %>%
  mutate(
    Monat_für_Plot = case_when(
      Monat == 10 ~ 1,
      Monat == 11 ~ 2,
      Monat == 12 ~ 3,
      Monat == 1  ~ 4,
      Monat == 2  ~ 5,
      Monat == 3  ~ 6,
      Monat == 4  ~ 7,
      Monat == 5  ~ 8,
      Monat == 6  ~ 9,
      Monat == 7  ~ 10
    ) + (as.numeric(format(Datum, "%d")) / 30)  # Feinanpassung mit Tageswerten
  )

# Plot 
ggplot(all_ndvi_data, aes(x = Monat_für_Plot, 
                          y = NDVI, 
                          group = Season, 
                          color = Season)) +
  geom_line() +  
  geom_point() +  
  labs(title = "NDVI Time Series for All Seasons",
       x = "Month", y = "NDVI Value") +
  scale_x_continuous(
    breaks = 1:10, 
    labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
  ) +
  scale_color_manual(
    values = c(
      "2017/2018" = "#008800",
      "2018/2019" = "#40e0d0",
      "2019/2020" = "#ffa500",
      "2020/2021" = "#c154c1",
      "2021/2022" = "#f82c00",
      "2022/2023" = "#1294a7",
      "2023/2024" = "#92a500"
    )
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Größere Symbole in der Legende
  theme_minimal() +
  theme(legend.position = "top"))

################################################################################
#                             Gain and Loss Plots                              #
################################################################################
library(terra)

# Pfad zur Rasterdaten
raster_files <- list.files(
  "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData",
  pattern = ".*\\.tif$",
  full.names = TRUE
)

# Raster einlesen und vorbereiten
file_list <- lapply(raster_files, function(x) {
  r <- rast(x)
  r[is.na(r)] <- 0       
  r[r == 40] <- 1       
  return(r)
})

# Benennung der Raster anhand Dateinamen 
names(file_list) <- tools::file_path_sans_ext(basename(raster_files))

# Zugriff auf spezifische Raster per Name
base_raster   <- file_list[["maske_1819_linear_sg0390"]]
target_raster <- file_list[["maske_1920_linear_sg0390"]]

gain  <- ifel(base_raster == 0 & target_raster == 1, 1, 0)
loss  <- ifel(base_raster == 1 & target_raster == 0, 1, 0)
change <- ifel(gain == 1 | loss == 1, 1, 0)

# Pixel zählen
cat("Gain:   ", global(gain, "sum", na.rm = TRUE)$sum, "\n")
cat("Loss:   ", global(loss, "sum", na.rm = TRUE)$sum, "\n")
cat("Change: ", global(change, "sum", na.rm = TRUE)$sum, "\n")

# Plot
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2), oma = c(0, 0, 3, 0))

plot(gain, main = "Gain", col = c("white", "#3b27ba"))       # 0 = weiß, 1 = blau
plot(loss, main = "Loss", col = c("white", "#fc6e22"))       # 0 = weiß, 1 = orange
plot(change, main = "Total Change", col = c("white", "#ff1493"))  # 0 = weiß, 1 = pink

mtext("Land Cover Change between Season 2019/2020 and 2020/2021", side = 3, outer = TRUE, line = 0.5, cex = 1.2)

# 0 → NA
gain_na   <- classify(gain, rcl = matrix(c(0, NA), ncol = 2, byrow = TRUE))
loss_na   <- classify(loss, rcl = matrix(c(0, NA), ncol = 2, byrow = TRUE))
change_na <- classify(change, rcl = matrix(c(0, NA), ncol = 2, byrow = TRUE))

# Speichern
writeRaster(gain_na,   "gain_1920_2021.tif",   overwrite = TRUE)
writeRaster(loss_na,   "loss_1920_2021.tif",   overwrite = TRUE)
writeRaster(change_na, "change_1920_2021.tif", overwrite = TRUE)


################################################################################
#                                 Diverse Plots                                #
################################################################################
# Pakete laden
library(dplyr)
library(ggplot2)
library(anomalize)
library(tibbletime)

#Mean NDVI 
# Durchschnittlichen NDVI pro Season berechnen
ndvi_summary <- all_ndvi_data %>%
  group_by(Season) %>%
  summarise(mean_NDVI = mean(NDVI, na.rm = TRUE))

# Balkendiagramm erstellen
ggplot(ndvi_summary, aes(x = Season, y = mean_NDVI, fill = Season)) +
  geom_bar(stat = "identity") +  
  labs(title = "Mean NDVI Value per Season",
       x = "Season",
       y = "Mean NDVI") +
  scale_fill_manual(
    values = c(
      "2017/2018" = "#008800",
      "2018/2019" = "#40e0d0",
      "2019/2020" = "#ffa500",
      "2020/2021" = "#c154c1",
      "2021/2022" = "#f82c00",
      "2022/2023" = "#1294a7",
      "2023/2024" = "#92a500"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Legende ausblenden, weil Farben nur Season zeigen


################################################################################
#vergleich
# Daten für die drei Zeiträume filtern
planting_data <- all_ndvi_data %>%
  filter(Monat >= 10 | Monat <= 12) %>%
  group_by(Season) %>%
  summarise(mean_NDVI = mean(NDVI, na.rm = TRUE)) %>%
  mutate(Phase = "Planting")

mid_season_data <- all_ndvi_data %>%
  filter(Monat >= 1 & Monat <= 5) %>%
  group_by(Season) %>%
  summarise(mean_NDVI = mean(NDVI, na.rm = TRUE)) %>%
  mutate(Phase = "Mid-Season")

harvesting_data <- all_ndvi_data %>%
  filter(Monat >= 6 & Monat <= 7) %>%
  group_by(Season) %>%
  summarise(mean_NDVI = mean(NDVI, na.rm = TRUE)) %>%
  mutate(Phase = "Harvesting")

# Alle zusammenführen
seasonal_summary <- bind_rows(planting_data, mid_season_data, harvesting_data)

# Plot 
ggplot(seasonal_summary, aes(x = Season, y = mean_NDVI, group = Phase, color = Phase)) +
  geom_line(linewidth = 1) +  # Linien für jede Phase
  geom_point(size = 4) +  # Einzelne Punkte für jede Phase
  labs(title = "Mean NDVI for Different Growth Stages",
       x = "Season",
       y = "Mean NDVI",
       color = "Growth Stage") +  # Legendenname anpassen
  scale_color_manual(
    values = c(
      "Planting" = "#265384",
      "Mid-Season" = "#008800",
      "Harvesting" = "#ea5500"
    )
  ) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Größere Symbole in der Legende
  theme(
    legend.position = "top",  # Legende oben
    legend.text = element_text(size = 10),  # Größere Schrift für Legendentext
    legend.title = element_text(size = 10, face ="bold"),  # Größere & fette Schrift für Legendentitel
    axis.text.x = element_text(color = "black")  # X-Achsenbeschriftung schwarz
  )


################################################################################
#Standartabweichung
seasonal_summary <- all_ndvi_data %>%
  group_by(Season) %>%
  summarise(
    mean_NDVI = mean(NDVI, na.rm = TRUE),
    sd_NDVI = sd(NDVI, na.rm = TRUE)  
  )


################################################################################
#                                 CLIMATE DATA                                 #
################################################################################
################################################################################
# KLIMADIAGRAMM                                                                #
################################################################################
# Klimadiagramm
library(dplyr)
library(ggplot2)
library(readr)

# .csv-Dateien einlesen
rainfall_data <- read.csv("APAS_Average_Rainfall_2017-2025.csv", sep=";")
temperature_data <- read.csv("APAS_Average_Temperature_2017-2025.csv", sep=";")

# Datum korrekt umwandeln
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")
temperature_data$Date <- as.Date(temperature_data$Date, format = "%d.%m.%Y")

# Prüfen, ob das Datum jetzt richtig ist
str(rainfall_data$Date)  
str(temperature_data$Date)

#️  Daten für 2017-2021 filtern
rainfall_filtered <- rainfall_data %>%
  filter(Date >= as.Date("2017-10-01") & Date <= as.Date("2021-12-31"))

temperature_filtered <- temperature_data %>%
  filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2021-7-31"))

# Daten zusammenführen 
climate_data <- merge(rainfall_filtered, temperature_filtered, by = "Date")

# Plot
ggplot(climate_data, aes(x = Date)) +
  geom_bar(aes(y = `Average.Rainfall` - 0.1), stat = "identity", fill = "#007cb0", alpha = 0.5) +  
  geom_line(aes(y = `Average.Temperature` * 2), color = "red", size = 1) +  
  scale_y_continuous(
    name = "Precipitation [mm]", 
    sec.axis = sec_axis(~ . / 2, name = "Temperature [°C]", 
                        breaks = seq(0, max(climate_data$Average.Temperature, na.rm = TRUE), by = 5)),  # Temperatur-Ticks alle 5°C
    expand = c(0, 0),  
    breaks = seq(0, max(climate_data$Average.Rainfall, na.rm = TRUE), by = 10)  # Niederschlags-Ticks alle 10 mm
  ) +
  scale_x_date(
    date_labels = "%m/%Y",  
    date_breaks = "3 months",  
    minor_breaks = "1 month",  
    expand = c(0, 0)  
  ) +
  labs(title = "Temperature and Precipitation Averaged Across Idlib and Jabal Saman (2017–2021)",
       x = "Date",
       y = "Precipitation [mm]") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, color = "black"),  
    axis.title.y = element_text(size = 14, color = "#007cb0"),  
    axis.title.y.right = element_text(size = 14, color = "red"),  
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 12, color = "black"),  
    plot.title = element_text(size = 16, color = "black", hjust = 0.5),  
    axis.line.x = element_line(color = "black", size = 1),  
    axis.line.y = element_line(color = "black", size = 1),  
    axis.ticks.x = element_line(color = "black", size = 0.5),  
    axis.ticks.y = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.2, "cm")  
  )
################################################################################
#Pakete laden
library(dplyr)
library(ggplot2)

# CSV-Dateien einlesen
rainfall_data <- read.csv("APAS_Average_Rainfall_2017-2025.csv", sep=";")
temperature_data <- read.csv("APAS_Average_Temperature_2017-2025.csv", sep=";")
ndvi_data_1819 <- read.csv("NDVI2021.csv", sep=";")  # NDVI für Saison 2018/2019

# Datum korrekt umwandeln
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")
temperature_data$Date <- as.Date(temperature_data$Date, format = "%d.%m.%Y")
ndvi_data_1819$Datum <- as.Date(ndvi_data_1819$Datum, format = "%d.%m.%Y")

# Zeitraum Oktober 2018 - Juli 2019
start_date <- as.Date("2020-10-01")
end_date <- as.Date("2021-07-31")

rainfall_data <- rainfall_data %>% filter(Date >= start_date & Date <= end_date)
temperature_data <- temperature_data %>% filter(Date >= start_date & Date <= end_date)
ndvi_data_1819 <- ndvi_data_1819 %>% filter(Datum >= start_date & Datum <= end_date)

# Mergen der Daten
merged_data_1819 <- merge(rainfall_data, temperature_data, by = "Date", all = TRUE)
merged_data_1819 <- merge(merged_data_1819, ndvi_data_1819, by.x = "Date", by.y = "Datum", all = TRUE)

names(merged_data_1819) <- c("Date", "Average.Rainfall", "Average.Temperature", "NDVI")

ndvi_labels <- merged_data_1819[!is.na(merged_data_1819$NDVI), ]
ndvi_labels <- ndvi_labels[seq(1, nrow(ndvi_labels), by = 3), ]  

# Plot
ggplot(merged_data_1819, aes(x = Date)) +
  geom_bar(aes(y = Average.Rainfall - 0.1), stat = "identity", fill = "#007cb0", alpha = 0.5) +  
  geom_line(data = merged_data_1819[!is.na(merged_data_1819$Average.Temperature), ], 
            aes(y = Average.Temperature * 2), 
            color = "red", size = 1) +  
  geom_point(data = merged_data_1819[!is.na(merged_data_1819$NDVI), ], 
             aes(y = NDVI * 100), 
             color = "#c154c1", size = 2) +
  geom_line(data = merged_data_1819[!is.na(merged_data_1819$NDVI), ], 
            aes(y = NDVI * 100), 
            color = "#c154c1", size = 1) +
  geom_text(data = ndvi_labels,
            aes(x = Date, y = NDVI * 100, label = round(NDVI, 2)), 
            color = "black", size = 3,fontface = "bold",
            vjust = +1) +  
  scale_y_continuous(
    name = "Precipitation [mm]", 
    sec.axis = sec_axis(~ . / 2, name = "Temperature [°C]"), 
    expand = c(0, 0),  
    limits = c(0, 75)  
  ) +
  scale_x_date(
    date_labels = "%m/%Y",  
    date_breaks = "1 month",  
    minor_breaks = "1 month",  
    expand = c(0.05, 0.05)  
  ) +
  labs(title = "Temperature, Precipitation, and NDVI (2020-2021)",
       x = "Date",
       y = "Precipitation [mm]") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, color = "black"),  
    axis.title.y = element_text(size = 14, color = "#007cb0"),  
    axis.title.y.right = element_text(size = 14, color = "red"),  
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 12, color = "black"),  
    plot.title = element_text(size = 16, color = "black", hjust = 0.5),  
    axis.line.x = element_line(color = "black", size = 1),  
    axis.line.y = element_line(color = "black", size = 1),  
    axis.ticks.x = element_line(color = "black", size = 0.5),  
    axis.ticks.y = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.2, "cm")
  ) +
  # Legende für den NDVI
  annotation_custom(
    grob = grid::textGrob("NDVI", gp = grid::gpar(col = "#c154c1", fontsize = 12)), 
    xmin = as.Date("2021-02-01"), xmax = as.Date("2021-02-01"), 
    ymin = max(merged_data_1819$Average.Rainfall, na.rm = TRUE) +10, 
    ymax = max(merged_data_1819$Average.Rainfall, na.rm = TRUE)
  )


################################################################################
# NIEDERSCHLAG                                                                 #
################################################################################
################################################################################
#nochmal irgendwas korrelation
# Pakete laden
library(dplyr)
library(broom) 
library(tidyr)

# NDVI-Daten und Niederschlagsdaten einlesen
ndvi_data <- read.csv("All_NDVI.csv", sep = ";")
rainfall_data <- read.csv("APAS_Average_Rainfall.csv", sep = ";")

# Datum in richtiges Format umwandeln
ndvi_data$Date <- as.Date(ndvi_data$Date, format = "%d.%m.%Y")
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")

# Daten nach Year-Month gruppieren und den monatlichen Mittelwert berechnen
ndvi_data$YearMonth <- format(ndvi_data$Date, "%Y-%m")
rainfall_data$YearMonth <- format(rainfall_data$Date, "%Y-%m")

monthly_ndvi <- ndvi_data %>%
  group_by(YearMonth) %>%
  summarise(NDVI_mean = mean(NDVI, na.rm = TRUE), .groups = "drop")

monthly_rainfall <- rainfall_data %>%
  group_by(YearMonth) %>%
  summarise(Average_Rainfall = mean(Average.Rainfall, na.rm = TRUE), .groups = "drop")

# Daten mergen
merged_data <- merge(monthly_ndvi, monthly_rainfall, by = "YearMonth", all.x = TRUE)

# Season und Phase korrekt erstellt
merged_data <- merged_data %>%
  mutate(
    Year = as.numeric(substr(YearMonth, 1, 4)),
    Month = as.numeric(substr(YearMonth, 6, 7)),
    Season = case_when(
      Month >= 10 ~ paste0(Year, "/", Year + 1),
      Month <= 7  ~ paste0(Year - 1, "/", Year),
      TRUE ~ NA_character_
    ),
    Phase = case_when(
      Month %in% 10:12 ~ "Planting",
      Month %in% 1:5   ~ "Mid-Season",
      Month %in% 6:7   ~ "Harvesting",
      TRUE ~ NA_character_
    )
  )

head(merged_data)

# Berechnung der Pearson-Korrelation und des p-Werts
cor_test <- cor.test(merged_data$NDVI_mean, merged_data$Average_Rainfall, method = "pearson", use = "complete.obs")
cor_test

library(dplyr)
library(broom)

# Funktion für sicheren Korrelations-Test
correlation_test <- function(x, y) {
  if (sum(!is.na(x) & !is.na(y)) >= 3) {
    result <- cor.test(x, y, method = "pearson")
    tidy(result)
  } else {
    return(data.frame(
      estimate = NA,
      statistic = NA,
      p.value = NA,
      parameter = NA,
      conf.low = NA,
      conf.high = NA,
      method = NA,
      alternative = NA
    ))
  }
}

# Berechnung der Korrelation mit vollen Testwerten je Season & Phase
season_correlation <- merged_data %>%
  group_by(Season, Phase) %>%
  filter(sum(!is.na(NDVI_mean) & !is.na(Average_Rainfall)) >= 3) %>%
  summarise(
    cor_result = list(correlation_test(NDVI_mean, Average_Rainfall)),
    .groups = "drop"
  ) %>%
  unnest(cor_result)

print(season_correlation, n = 40)
library(ggplot2)

# Angenommene vorherige Berechnungen mit den merged_data
# Korrelation berechnen und die Daten nach Season und Phase gruppieren
season_correlation <- merged_data %>%
  group_by(Season, Phase) %>%
  filter(!is.na(NDVI_mean) & !is.na(Average_Rainfall)) %>%
  summarise(
    correlation = cor(NDVI_mean, Average_Rainfall, method = "pearson", use = "complete.obs"),
    .groups = "drop"
  )

print(season_correlation, n=40)

################################################################################
#Korrelationen NDVI Prec

#NDVI in Dekaden rechnen
library(dplyr)
library(lubridate)
# Daten einlesen
ndvi_data <- read.csv("All_NDVI.csv", sep = ";")
ndvi_data$Date <- as.Date(ndvi_data$Date, format = "%d.%m.%Y")
# Dekade bestimmen 
ndvi_data <- ndvi_data %>%
  mutate(
    Day = day(Date),
    Dekade = case_when(
      Day <= 10 ~ 1,
      Day <= 20 ~ 2,
      TRUE ~ 3
    ),
    Month = month(Date),
    Year = year(Date)
  )
# Season zuweisen 
ndvi_data <- ndvi_data %>%
  mutate(
    Season = case_when(
      Month >= 10 ~ paste0(Year, "/", Year + 1),
      Month <= 7  ~ paste0(Year - 1, "/", Year),
      TRUE ~ NA_character_
    )
  )
# Kombinierte Dekaden-ID innerhalb der Season
ndvi_data <- ndvi_data %>%
  arrange(Date) %>%
  group_by(Season) %>%
  mutate(Dekade_ID = dense_rank(paste0(Year, "-", Month, "-", Dekade))) %>%
  ungroup()
# Mittelwert je Dekade berechnen
ndvi_dekaden <- ndvi_data %>%
  group_by(Season, Dekade_ID) %>%
  summarise(
    Start_Datum = min(Date),
    NDVI_mean = mean(NDVI, na.rm = TRUE),
    .groups = "drop"
  )

print(ndvi_dekaden, n = 40)
write.csv(ndvi_dekaden, "NDVI_in_Decades.csv")




#Korrelation NDVIxPREC/Season
ndvi_data <- read.csv("NDVI_in_Decades.csv", sep = ";")
ndvi_data$Start_Datum <- as.Date(ndvi_data$Start_Datum, format = "%d.%m.%Y")
rainfall_data <- read.csv("APAS_Average_Rainfall.csv", sep = ";")
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")
# Sicherstellen, dass Datum korrekt ist
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")
rainfall_data$Year <- as.numeric(format(rainfall_data$Date, "%Y"))
rainfall_data$Month <- as.numeric(format(rainfall_data$Date, "%m"))

# Season zuweisen basierend auf dem Monat
rainfall_data$Season <- ifelse(
  rainfall_data$Month >= 10, 
  paste0(rainfall_data$Year, "/", rainfall_data$Year + 1),
  paste0(rainfall_data$Year - 1, "/", rainfall_data$Year)
)

merged_dekaden <- merge(
  ndvi_data[, c("Season", "Decade", "NDVI")],
  rainfall_data[, c("Season", "Decade", "Average.Rainfall")],
  by = c("Season", "Decade")
)
merged_dekaden <- merged_dekaden[order(merged_dekaden$Season, merged_dekaden$Decade), ]

merged_dekaden <- merged_dekaden %>%
  rename(Average_Rainfall = Average.Rainfall)

# Korrelation pro Season berechnen
seasonal_correlations <- merged_dekaden %>%
  group_by(Season) %>%
  summarise(
    cor_result = list(cor.test(NDVI, Average_Rainfall, method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    estimate = sapply(cor_result, function(x) x$estimate),
    r_squared = estimate^2,  
    p_value = sapply(cor_result, function(x) x$p.value),
    conf_low = sapply(cor_result, function(x) x$conf.int[1]),
    conf_high = sapply(cor_result, function(x) x$conf.int[2]),
    t_stat = sapply(cor_result, function(x) x$statistic),
    df = sapply(cor_result, function(x) x$parameter)
  ) %>%
  select(Season, estimate, r_squared, p_value, conf_low, conf_high, t_stat, df)

print(seasonal_correlations, n=40)


#Korrelation NDVIxPREC/Growth Stage 
library(dplyr)

# Stage-Spalte basierend auf Dekade
merged_dekaden <- merged_dekaden %>%
  mutate(Stage = case_when(
    Decade >= 1  & Decade <= 9  ~ "Planting",
    Decade >= 10 & Decade <= 24 ~ "Mid Season",
    Decade >= 25 & Decade <= 30 ~ "Harvesting"
  ))

# Korrelation pro Season UND Stage berechnen
stage_correlations <- merged_dekaden %>%
  group_by(Season, Stage) %>%
  summarise(
    cor_result = list(cor.test(NDVI, Average_Rainfall, method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    estimate   = sapply(cor_result, function(x) x$estimate),
    r_squared  = estimate^2,
    p_value    = sapply(cor_result, function(x) x$p.value),
    conf_low   = sapply(cor_result, function(x) x$conf.int[1]),
    conf_high  = sapply(cor_result, function(x) x$conf.int[2]),
    t_stat     = sapply(cor_result, function(x) x$statistic),
    df         = sapply(cor_result, function(x) x$parameter)
  ) %>%
  select(Season, Stage, estimate, r_squared, p_value, conf_low, conf_high, t_stat, df)

stage_correlations <- stage_correlations %>%
  mutate(Stage = factor(Stage, levels = c("Planting", "Mid Season", "Harvesting"))) %>%
  arrange(Season, Stage)

print(stage_correlations, n=40)

################################################################################
#Korrelation NDVI SPI
#Korrelation NDVIxPREC/Season
ndvi_data <- read.csv("NDVI_in_Decades.csv", sep = ";")
ndvi_data$Start_Datum <- as.Date(ndvi_data$Start_Datum, format = "%d.%m.%Y")
SPI_data <- read.csv("SPI_Results.csv", sep = ";")
SPI_data$Date <- as.Date(SPI_data$Date, format = "%d.%m.%Y")
# Sicherstellen, dass Datum korrekt ist
SPI_data$Date <- as.Date(SPI_data$Date, format = "%d.%m.%Y")
SPI_data$Year <- as.numeric(format(SPI_data$Date, "%Y"))
SPI_data$Month <- as.numeric(format(SPI_data$Date, "%m"))

# Season zuweisen basierend auf dem Monat
SPI_data$Season <- ifelse(
  SPI_data$Month >= 10, 
  paste0(SPI_data$Year, "/", rainfall_data$Year + 1),
  paste0(SPI_data$Year - 1, "/", rainfall_data$Year)
)

merged_dekaden <- merge(
  ndvi_data[, c("Season", "Decade", "NDVI")],
  SPI_data[, c("Season", "Decade", "SPI_3")],
  by = c("Season", "Decade")
)
merged_dekaden <- merged_dekaden[order(merged_dekaden$Season, merged_dekaden$Decade), ]

# Korrelation pro Season berechnen
seasonal_correlations <- merged_dekaden %>%
  group_by(Season) %>%
  summarise(
    cor_result = list(cor.test(NDVI, SPI_3, method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    estimate = sapply(cor_result, function(x) x$estimate),
    r_squared = estimate^2, 
    p_value = sapply(cor_result, function(x) x$p.value),
    conf_low = sapply(cor_result, function(x) x$conf.int[1]),
    conf_high = sapply(cor_result, function(x) x$conf.int[2]),
    t_stat = sapply(cor_result, function(x) x$statistic),
    df = sapply(cor_result, function(x) x$parameter)
  ) %>%
  select(Season, estimate, r_squared, p_value, conf_low, conf_high, t_stat, df)

print(seasonal_correlations)


#Korrelation NDVIxPREC/Growth Stage 
library(dplyr)

# Stage-Spalte basierend auf Dekade
merged_dekaden <- merged_dekaden %>%
  mutate(Stage = case_when(
    Decade >= 1  & Decade <= 9  ~ "Planting",
    Decade >= 10 & Decade <= 24 ~ "Mid Season",
    Decade >= 25 & Decade <= 30 ~ "Harvesting"
  ))

# Korrelation pro Season UND Stage berechnen
stage_correlations <- merged_dekaden %>%
  group_by(Season, Stage) %>%
  summarise(
    cor_result = list(cor.test(NDVI,SPI_3, method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    estimate   = sapply(cor_result, function(x) x$estimate),
    r_squared  = estimate^2,
    p_value    = sapply(cor_result, function(x) x$p.value),
    conf_low   = sapply(cor_result, function(x) x$conf.int[1]),
    conf_high  = sapply(cor_result, function(x) x$conf.int[2]),
    t_stat     = sapply(cor_result, function(x) x$statistic),
    df         = sapply(cor_result, function(x) x$parameter)
  ) %>%
  select(Season, Stage, estimate, r_squared, p_value, conf_low, conf_high, t_stat, df)

stage_correlations <- stage_correlations %>%
  mutate(Stage = factor(Stage, levels = c("Planting", "Mid Season", "Harvesting"))) %>%
  arrange(Season, Stage)

print(stage_correlations, n=40)

################################################################################
#Random Summen
#season sum
library(ggplot2)
library(dplyr)

rainfall_data <- read.csv("APAS_Average_Rainfall.csv", sep = ";")

# Datum in richtiges Format umwandeln
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")

# Saison-Funktion definieren
rainfall_data$Season <- with(rainfall_data, ifelse(
  format(Date, "%m") %in% c("11", "12"),
  paste0(as.numeric(format(Date, "%Y")), "/", as.numeric(format(Date, "%Y")) + 1),
  ifelse(format(Date, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),
         paste0(as.numeric(format(Date, "%Y")) - 1, "/", format(Date, "%Y")),
         NA)
))

seasonal_rainfall <- rainfall_data %>%
  filter(Season %in% c("2018/2019", "2019/2020", "2020/2021")) %>%
  group_by(Season) %>%
  summarise(Total.Rainfall = sum(Average.Rainfall, na.rm = TRUE))

ggplot(seasonal_rainfall, aes(x = Season, y = Total.Rainfall, fill = Season)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("#40e0d0", "#ffa500", "#c154c1")) +
  labs(title = "Total Precipitation per Season",
       x = "Season",
       y = "Total Precipitation [mm]") +
  theme_minimal() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#growth stages per season sum
library(dplyr)
library(ggplot2)
library(lubridate)

# Datum formatieren
rainfall_data$Date <- as.Date(rainfall_data$Date)

# Season und Phase zuweisen
rainfall_data <- rainfall_data %>%
  mutate(
    year = year(Date),
    month = month(Date),
    
    Phase = case_when(
      month %in% 10:12 ~ "Planting",
      month %in% 1:5   ~ "Mid-Season",
      month %in% 6:7   ~ "Harvesting",
      month %in% 8:9   ~ "Off-Season",
      TRUE ~ NA_character_
    ),
    
    Season = case_when(
      Phase == "Off-Season"      ~ paste0(year, "/", year + 1),
      month >= 10 & Phase != "Off-Season" ~ paste0(year, "/", year + 1),
      month <= 7 & Phase != "Off-Season"  ~ paste0(year - 1, "/", year),
      TRUE ~ NA_character_
    )
  )

# Zusammenfassen
season_phase_summary <- rainfall_data %>%
  filter(Season %in% c("2018/2019", "2019/2020", "2020/2021")) %>%
  group_by(Season, Phase) %>%
  summarise(Total.Rainfall = sum(Average.Rainfall, na.rm = TRUE), .groups = "drop")

# Faktorreihenfolge
season_phase_summary$Phase <- factor(season_phase_summary$Phase,
                                     levels = c("Off-Season","Planting", "Mid-Season", "Harvesting"))

# Plot
ggplot(season_phase_summary, aes(x = Season, y = Total.Rainfall, fill = Phase)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c(
    "Off-Season" = "#29abe2",
    "Planting" = "#007cb0",
    "Mid-Season" = "#0e1f53",
    "Harvesting" = "#abd9e9"  # schöne Kontrastfarbe
  )) +
  labs(
    title = "Total Precipitation per Growth Stage and Season",
    x = "",
    y = "Total Precipitation [mm]",
    fill = "Growth Stage"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.28),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

################################################################################
# Temperatur                                                                   #
################################################################################
################################################################################
#Random Summen 
library(dplyr)
library(ggplot2)
library(lubridate)

# Temperaturdaten einlesen
temperature_data <- read.csv("APAS_Average_Temperature.csv", sep = ";")
temperature_data$Date <- as.Date(temperature_data$Date, format = "%d.%m.%Y")

# Season + Phase zuweisen 
temperature_data <- temperature_data %>%
  mutate(
    year = year(Date),
    month = month(Date),

    Phase = case_when(
      month %in% 10:12 ~ "Planting",
      month %in% 1:5   ~ "Mid-Season",
      month %in% 6:7   ~ "Harvesting",
      month %in% 8:9   ~ "Off-Season",
      TRUE ~ NA_character_
    ),

    Season = case_when(
      Phase == "Off-Season"           ~ paste0(year, "/", year + 1),
      month >= 10 & Phase != "Off-Season" ~ paste0(year, "/", year + 1),
      month <= 7 & Phase != "Off-Season"  ~ paste0(year - 1, "/", year),
      TRUE ~ NA_character_
    )
  )


# Seasons filtern und mitteln
season_phase_summary_temp <- temperature_data %>%
  filter(Season %in% c("2018/2019", "2019/2020", "2020/2021")) %>%
  group_by(Season, Phase) %>%
  summarise(Mean.Temperature = mean(Average.Temperature, na.rm = TRUE), .groups = "drop")

# Reihenfolge
season_phase_summary_temp$Phase <- factor(season_phase_summary_temp$Phase,
                                          levels = c("Off-Season", "Planting", "Mid-Season", "Harvesting"))

# Plot 
ggplot(season_phase_summary_temp, aes(x = Season, y = Mean.Temperature, fill = Phase)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c(
    "Off-Season" = "#ff9999",
    "Planting" = "#ff1f1f",
    "Mid-Season" = "#b40000",
    "Harvesting" = "#7a0000"
  ))+
  labs(
    title = "Average Temperature per Growth Stage and Season",
    x = "",
    y = "Average Temperature [°C]",
    fill = "Growth Stage"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.28),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

################################################################################
# Anomalien
# Lade die Daten
library(dplyr)
library(readr)
library(lubridate)

# Einlesen der aktuellen Temperaturdaten
temperature_data <- read.csv("APAS_Average_Temperature.csv", sep = ";")
temperature_data$Date <- as.Date(temperature_data$Date, format = "%d.%m.%Y")

# Einlesen der historischen Temperaturdaten
historical_data <- read.csv("Historial_Temperature.csv", sep = ";")
historical_data$Date <- as.Date(historical_data$Date, format = "%d.%m.%Y")

temperature_data <- temperature_data %>%
  filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2021-12-31"))
# Tag & Monat extrahieren für Matching
temperature_data <- temperature_data %>%
  mutate(DayMonth = format(Date, "%d-%m"))

historical_data <- historical_data %>%
  mutate(DayMonth = format(Date, "%d-%m"))

# Join mit aktueller Temperaturtabelle
merged_data <- temperature_data %>%
  left_join(historical_data, by = "DayMonth")

# Anomalie berechnen
merged_data <- merged_data %>%
  mutate(Anomaly = Average.Temperature - HistoricalTemperature_mean)

head(merged_data)
print(merged_data, n="200")
write.csv(merged_data, "temperature_anomalies.csv", row.names = FALSE)

# Plot der Temperaturabweichungen (Anomalien)
# Basisplot
# Leerer Plot vorbereiten 
plot(merged_data$Date.x, merged_data$Anomaly, type = "n",
     ylab = "Anomaly [°C]", xlab = "", main = "Temperature Anomalies",
     xaxt = "n")

# X-Achse
years <- seq(as.Date("2017-01-01"), as.Date("2021-12-31"), by = "year")
axis(1, at = years, labels = format(years, "%Y"))

# Y-Achse
y_ticks <- axTicks(2)

# Grid-Linien
abline(v = years, col = "gray90")     
abline(h = y_ticks, col = "gray90")   

# Datenlinie zeichnen
lines(merged_data$Date.x, merged_data$Anomaly, col = "red", lwd = 2)

# Horizontale Linie für historisches Mittel (z.B. 0)
abline(h = 0, col = "gray30", lty = 2)

legend("topleft", legend = "Historical Mean", lty = 2, col = "gray30", bty = "n")

################################################################################
#Korrelation T und NDVI per Season
#  Temperaturdaten einlesen
ndvi_data <- read.csv("NDVI_in_Decades.csv", sep = ";")
ndvi_data$Start_Datum <- as.Date(ndvi_data$Start_Datum, format = "%d.%m.%Y")
temperature_data <- read.csv("APAS_Average_Temperature.csv", sep = ";")
temperature_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")

# Sicherstellen dass Datum korrekt ist
temperature_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")
temperature_data$Year <- as.numeric(format(rainfall_data$Date, "%Y"))
temperature_data$Month <- as.numeric(format(rainfall_data$Date, "%m"))

# Season zuweisen basierend auf dem Monat
temperature_data$Season <- ifelse(
  temperature_data$Month >= 10, 
  paste0(temperature_data$Year, "/", temperature_data$Year + 1),
  paste0(temperature_data$Year - 1, "/", temperature_data$Year)
)

merged_dekaden <- merge(
  ndvi_data[, c("Season", "Decade", "NDVI")],
  temperature_data[, c("Season", "Decade", "Average.Temperature")],
  by = c("Season", "Decade")
)
merged_dekaden <- merged_dekaden[order(merged_dekaden$Season, merged_dekaden$Decade), ]

merged_dekaden <- merged_dekaden %>%
  rename(Average_Temperature = Average.Temperature)

# Korrelation pro Season berechnen
seasonal_correlations <- merged_dekaden %>%
  group_by(Season) %>%
  summarise(
    cor_result = list(cor.test(NDVI, Average_Temperature, method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    estimate = sapply(cor_result, function(x) x$estimate),
    r_squared = estimate^2,  
    p_value = sapply(cor_result, function(x) x$p.value),
    conf_low = sapply(cor_result, function(x) x$conf.int[1]),
    conf_high = sapply(cor_result, function(x) x$conf.int[2]),
    t_stat = sapply(cor_result, function(x) x$statistic),
    df = sapply(cor_result, function(x) x$parameter)
  ) %>%
  select(Season, estimate, r_squared, p_value, conf_low, conf_high, t_stat, df)

print(seasonal_correlations)


#Korrelation NDVIxPREC/Growth Stage 
library(dplyr)

# Stage
merged_dekaden <- merged_dekaden %>%
  mutate(Stage = case_when(
    Decade >= 1  & Decade <= 9  ~ "Planting",
    Decade >= 10 & Decade <= 24 ~ "Mid Season",
    Decade >= 25 & Decade <= 30 ~ "Harvesting"
  ))

# Korrelation pro Season UND Stage berechnen
stage_correlations <- merged_dekaden %>%
  group_by(Season, Stage) %>%
  summarise(
    cor_result = list(cor.test(NDVI, Average_Temperature, method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    estimate   = sapply(cor_result, function(x) x$estimate),
    r_squared  = estimate^2,
    p_value    = sapply(cor_result, function(x) x$p.value),
    conf_low   = sapply(cor_result, function(x) x$conf.int[1]),
    conf_high  = sapply(cor_result, function(x) x$conf.int[2]),
    t_stat     = sapply(cor_result, function(x) x$statistic),
    df         = sapply(cor_result, function(x) x$parameter)
  ) %>%
  select(Season, Stage, estimate, r_squared, p_value, conf_low, conf_high, t_stat, df)

# Stage 
stage_correlations <- stage_correlations %>%
  mutate(Stage = factor(Stage, levels = c("Planting", "Mid Season", "Harvesting"))) %>%
  arrange(Season, Stage)

print(stage_correlations, n=40)

################################################################################
#KORRELATION T,P,NDVI                                                          #
################################################################################
# Lade die Daten
temperature_data <- read.csv("APAS_Average_Temperature.csv", sep = ";")
rainfall_data <- read.csv("APAS_Average_Rainfall.csv", sep = ";")
ndvi_data <- read.csv("All_NDVI.csv", sep = ";")

# Konvertieren der Datumswerte 
temperature_data$Date <- as.Date(temperature_data$Date, format = "%d.%m.%Y")
rainfall_data$Date <- as.Date(rainfall_data$Date, format = "%d.%m.%Y")
ndvi_data$Date <- as.Date(ndvi_data$Date, format = "%d.%m.%Y")

# Kombiniere alle Daten basierend auf Datum
combined_data <- temperature_data %>%
  left_join(rainfall_data, by = "Date") %>%
  left_join(ndvi_data, by = "Date")

# Funktion zur Berechnung der Korrelation mit verschobenem Niederschlag
calculate_correlation_with_shift <- function(season_start, season_end) {
  seasonal_data <- combined_data %>% 
    filter(Date >= season_start & Date <= season_end)
  seasonal_data$Average.Rainfall_shifted <- lag(seasonal_data$Average.Rainfall, 3)
  correlation_result <- cor(seasonal_data$Average.Temperature, seasonal_data$Average.Rainfall_shifted, use = "complete.obs")
  correlation_ndvi <- cor(seasonal_data$Average.Temperature, seasonal_data$NDVI, use = "complete.obs")
  correlation_rainfall_ndvi <- cor(seasonal_data$Average.Rainfall_shifted, seasonal_data$NDVI, use = "complete.obs")
  
  return(list(
    temperature_rainfall = correlation_result,
    temperature_ndvi = correlation_ndvi,
    rainfall_ndvi = correlation_rainfall_ndvi
  ))
}

# Liste der Saisons 
seasons <- c("2018/2019", "2019/2020", "2020/2021")

# Berechne die Korrelation für jede Saison mit dem 3-Monats-Versatz für den Niederschlag
correlations_with_shift <- lapply(seasons, function(season) {
  season_start <- as.Date(paste0(substr(season, 1, 4), "-10-01"))
  season_end <- as.Date(paste0(substr(season, 6, 9), "-07-31"))
  
  calculate_correlation_with_shift(season_start, season_end)
})

print(correlations_with_shift)

################################################################################
#                                  FIRMS DATA                                  #
################################################################################
# Pakete laden
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Daten einlesen
df <- read.csv("fire_archive_J1V-C2_62748_2012_2018.csv", sep = ";")

# Datum umwandeln 
df$ACQ_DATE <- as.Date(df$ACQ_DATE, format = "%d.%m.%Y")

# Anzahl der Events pro Monat berechnen
monthly_counts <- df %>%
  mutate(
    year = year(ACQ_DATE),
    month = month(ACQ_DATE, label = TRUE, abbr = TRUE),
    ym = format(ACQ_DATE, "%Y-%m")
  ) %>%
  count(ym) %>%
  arrange(ym)

# Plot
ggplot(monthly_counts, aes(x = ym, y = n)) +
  geom_col(fill = "#FC4E07") +
  theme_minimal() +
  labs(
    title = "VIIRS Detections per Month",
    x = "",
    y = "Number of Detections"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


################################################################################
#                                  ACLED DATA                                  #
################################################################################
# Lokaleinstellung auf Englisch setzen 
Sys.setlocale("LC_TIME", "C")

# Pakete laden
library(readxl)

# Excel-Datei einlesen
data <- read_excel("ACLED_Data_cropped.xlsx")

# Datum konvertieren
data$event_date <- as.Date(data$event_date)

# Monat extrahieren
data$month <- as.Date(format(data$event_date, "%Y-%m-01"))

# Events pro Monat zählen
event_counts <- aggregate(event_date ~ month, data = data, FUN = length)

# Alle Monate für Ticks
x_all <- seq(min(event_counts$month), max(event_counts$month), by = "1 month")

# Nur jeden zweiten Monat beschriften
x_labels <- x_all[seq(1, length(x_all), by = 2)]

# Vertikale Linien
x_grid <- seq(min(event_counts$month), max(event_counts$month), by = "6 months")

# Plot vorbereiten
plot(1, type = "n", xlim = c(1, nrow(event_counts) * 1.3), ylim = c(0, max(event_counts$event_date) * 1.1),
     xlab = "", ylab = "Number of Events", main = "ACLED Monthly Event Frequency",
     xaxt = "n", yaxt = "n")

# Grid
abline(h = pretty(event_counts$event_date), col = "lightgray", lty = "dotted")

# Vertikale Gridlinien 
grid_indices <- match(x_grid, event_counts$month)
abline(v = grid_indices * 1.3 - 0.15, col = "lightgray", lty = "dotted")

# Balken 
bar_x <- barplot(height = event_counts$event_date, col = "#dd873b", border = NA,
                 space = 0.3, add = TRUE)
# Y-Achse
axis(2)

# X-Achse
axis(1, at = bar_x, labels = FALSE)

# X-Achse
label_y <- par("usr")[3] - 0.05 * diff(par("usr")[3:4])
text(x = bar_x, y = label_y, labels = format(event_counts$month, "%b %Y"),
     srt = 45, adj = 1, xpd = TRUE, cex = 0.6)

###############
#Gliederung in Event Type
# Lade benötigte Pakete
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Daten einlesen
data <- read_excel("ACLED_Data_cropped.xlsx")


# 
data <- data %>%
  mutate(
    event_date = as.Date(event_date),  
    year_month = floor_date(event_date, unit = "month")  
  )

# Gruppieren nach Monat und Event Type
monthly_events <- data %>%
  group_by(year_month, event_type) %>%
  summarise(count = n(), .groups = "drop")

# Plotten
ggplot(monthly_events, aes(x = year_month, y = count, fill = event_type)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(
    title = "ACLED Events by Month and Type",
    x = "Month",
    y = "Number of Events",
    fill = "Event Type"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
#                                   UCDP Data                                  #
################################################################################
# Lokaleinstellung auf Englisch setzen
Sys.setlocale("LC_TIME", "C")

# Pakete laden
library(readxl)

# Excel-Datei einlesen 
data <- read_excel("UCDP_Syria_cropped.xls")

# Datum
data$date_start <- as.Date(sub(" .*", "", data$date_start), format = "%d.%m.%Y")

# Jahr 2022 ausschließen
data <- subset(data, format(date_start, "%Y") != "2022")

# Monat extrahieren 
data$month <- as.Date(format(data$date_start, "%Y-%m-01"))

# Events pro Monat zählen
event_counts <- aggregate(date_start ~ month, data = data, FUN = length)

# Alle Monate für X-Achse
x_all <- seq(min(event_counts$month), max(event_counts$month), by = "1 month")

# Vertikale Linien
x_grid <- seq(min(event_counts$month), max(event_counts$month), by = "6 months")

# Plot vorbereiten
plot(1, type = "n", xlim = c(1, nrow(event_counts) * 1.3), ylim = c(0, max(event_counts$date_start) * 1.1),
     xlab = "", ylab = "Number of Events", main = "UCDP Monthly Event Frequency",
     xaxt = "n", yaxt = "n")

# Horizontale Rasterlinien
abline(h = pretty(event_counts$date_start), col = "lightgray", lty = "dotted")

# Vertikale Rasterlinien
grid_indices <- match(x_grid, event_counts$month)
abline(v = grid_indices * 1.3 - 0.15, col = "lightgray", lty = "dotted")

# Balken 
bar_x <- barplot(height = event_counts$date_start, col = "darkcyan", border = NA,
                 space = 0.3, add = TRUE)

# Y-Achse zeichnen
axis(2)

# X-Achse
axis(1, at = bar_x, labels = FALSE)

# X-Achse
label_y <- par("usr")[3] - 0.05 * diff(par("usr")[3:4])
text(x = bar_x, y = label_y, labels = format(event_counts$month, "%b %Y"),
     srt = 45, adj = 1, xpd = TRUE, cex = 0.6)
