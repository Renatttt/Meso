library(tidyverse)
library(arrow)
library(dbplyr, warn.conflicts = FALSE)
library(duckdb)
library(utils)
library(stringr)
library(haven)
library(readxl)
library(openxlsx)

# 1. Obter a lista Microsoft

# Ler tls221
tls201 <- read_parquet("D:/Pat_4.0/PATSTAT2024/tls201_total.parquet") %>%
  as.data.frame()

#Filtrar para patentes com earliest_filing_year entre 2015-2024

tls201_filtrada <- tls201 %>%
  filter(earliest_filing_year >= 2015,
         earliest_filing_year <= 2024)

tls201_filtrada <- tls201_filtrada %>%
  select(appln_id, appln_kind, appln_filing_year, earliest_filing_year,
         docdb_family_id, docdb_family_size, granted, nb_applicants, nb_inventors, appln_auth) %>%
  collect()

#Lendo 206 e 207
tls207 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls207/tls207_total.parquet") %>%
  as.data.frame()

tls206 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls206/tls206_total.parquet") %>%
  as.data.frame()


#União
tls201_207 <- tls201_filtrada %>%
  inner_join(tls207, by = "appln_id")

n_distinct(tls201_207$appln_id)
# 41.861.096

tls201_207_206 <- tls201_207 %>%
  inner_join(tls206, by = "person_id")


tls201_207_206 <- tls201_207_206 %>%
  select(appln_id, appln_kind, appln_filing_year, earliest_filing_year,
         docdb_family_id, granted, nb_applicants, nb_inventors,applt_seq_nr, 
         invt_seq_nr, person_id, person_name, person_ctry_code ) %>%
  collect()

write_parquet(tls201_207_206, "D:/Financ_MI/tls201_207_206.parquet")

tls201_207_206 <- read_parquet("D:/Financ_MI/tls201_tls207_206.parquet") %>%
  as.data.frame()

#Buscando a Microsoft
tls201_207_206 <- read_parquet("C:/Users/cveneo/Desktop/Financ_MI/tls201_tls207_206.parquet") %>%
  as.data.frame()



Microsoft <- tls201_207_206 %>%
  filter(str_detect(person_name, regex("microsoft", ignore_case = TRUE)))

n_distinct(Microsoft$appln_id)
# 42.459




# Em busca de todas as subsidiárias que sejam 100% da Microsoft

# lista de termos
subsidiarias <- c("saskatchewan", "acompli", "adallom", "adrm software", "ally technologies",
                  "altspacevr", "apiphany", "blue talon", "bluetalon", "capptain", "citus data", 
                  "clear software", "cloudknox security", "compton acquisition", "compulsion games", 
                  "cyberx", "cycle computing", "double labs", "flipgrid", "fungible", "groove", 
                  "id8 group", "incent games", "jclarity", "kinvolk", "liveloop", "lobe artificial intelligence", 
                  "lumenisity", "malibu acquisition", "marketing pilot software", "metanautix", 
                  "metricshub", "minit", "mobile data labs", "mover", "opalis software", "peer5", 
                  "phonefactor", "playfab", "refirm labs", "semantic machines", "service scout", 
                  "spotfront", "storsimple", "sunrise atelier", "swing technologies", "takelessons", 
                  "talko", "the marsden", "two hat security", "undead labs", "videosurf", 
                  "wand labs", "6 wunderkinder", "adxstudio", "clipchamp", "maluuba", 
                 "minit", "nuance communications", "playground games"

)


sub <- paste0("\\b(", paste(subsidiarias, collapse = "|"), ")\\b")

# filtra
Subsidiarias_total <- tls201_207_206 %>%
  filter(str_detect(person_name, regex(sub, ignore_case = TRUE)))


Subsidiarias_unique <- Subsidiarias_total %>%
  distinct(person_id, .keep_all = TRUE)

write.xlsx(Subsidiarias_unique, file = "Subsidiarias_unique.xlsx")

# Olhei um por um e selecionei os que tenho certeza que são subsidiárias da Microsoft


ids <- c(
  82691518, 54627590, 54022372, 53496985, 69214529, 54455032, 56613342, 77408033,
  57892316, 56996721, 76746226, 57685123, 56809763, 88199609, 85988044, 74628840,
  76161012, 80661060, 85956510, 79512885, 87423090, 79509043, 81491116, 85988045,
  89030484, 84788582, 93284609, 92696129, 55234052, 87806388, 52986949, 52672470,
  55734761, 90889913, 57887930, 79505111, 72381734, 83325303, 79131591, 70033192,
  74215492, 70081179, 56965756, 58600671, 58051343, 65005397, 74570231, 72765935,
  76816098, 81625483, 80707920, 86083128, 86139610, 83491251, 82624233, 70682403,
  56141381, 57701616, 52221832, 55839254, 79098178, 83289915, 77835818, 52484428,
  53155308, 5988534, 59688902, 59512772, 72848797, 73603128, 59675140, 75228113,
  82979410, 61690432, 73290577, 78274984, 40347140, 7711512, 72374420, 54162661,
  12553137, 54341640, 57818408, 5232029, 110748, 54162660, 54572388, 79990756,
  90651898, 72433553, 72488355, 75842425, 70523679, 55519753, 57842027, 54726455,
  56002036, 70113668, 71931772, 76973877, 79400077, 74744660, 80016846, 48286195,
  55381822
)

ids <- as.character(ids)


Subsidiarias_final <- Subsidiarias_total[Subsidiarias_total$person_id %in% ids, ]

n_distinct(Subsidiarias_final$appln_id)
# 879

Microsoft_total <- rbind(Microsoft, Subsidiarias_final)

n_distinct(Microsoft_total$appln_id)
# 43.310




write_parquet(Microsoft_total, "Microsoft_pat.parquet")
write.csv2(Microsoft_total, "Microsoft_pat.csv")



tabela_appln_id <- as.data.frame(table(Microsoft_total$appln_id))

names(tabela_appln_id)[1] <- "appln_id"


Microsoft_total <- Microsoft_total %>%
  inner_join(tabela_appln_id, by = "appln_id")

Microsoft_total <- Microsoft_total %>%
  distinct(appln_id, .keep_all = TRUE)


# 2. Análises

# Conferindo

tabela_id <- as.data.frame(table(Microsoft_total$person_id))

tabela_names <- as.data.frame(table(Microsoft_total$person_name))

write.xlsx(tabela_names, file = "Freq_subsidiarias_Microsoft.xlsx")

# Número de aplicantes vs inventores

n_applicants <- as.data.frame(table(Microsoft_total$nb_applicants))
n_inventors <- as.data.frame(table(Microsoft_total$nb_inventors))

wb <- createWorkbook()

addWorksheet(wb, "Applicants")
writeData(wb, "Applicants", n_applicants)

addWorksheet(wb, "Inventors")
writeData(wb, "Inventors", n_inventors)

saveWorkbook(wb, file = "Freq_applic_invent.xlsx", overwrite = TRUE)



# Quem sãos os inventores?
 
appln_id_Microsoft <- Microsoft_total[, 1, drop = FALSE]

Microsoft_207 <- appln_id_Microsoft %>%
  inner_join(tls207, by = "appln_id")

invent <- Microsoft_207 %>%
  filter(invt_seq_nr > 0)


Invent_id_Microsoft <- invent[, 2, drop = FALSE]

Invent_Microsoft <- Invent_id_Microsoft %>%
  inner_join(tls206, by = "person_id")

Invent_Microsoft <- Invent_Microsoft %>%
  select(person_id, person_name, person_address, person_ctry_code, psn_name) %>%
  collect()

write.xlsx(Invent_Microsoft, file = "Nomes_inventores_Microsoft.xlsx")

# País de origem dos inventores

ctry <-  as.data.frame(table(Invent_Microsoft$person_ctry_code))

write.xlsx(ctry, file = "Freq_Ctry_inventores_Microsoft.xlsx")



# Áreas tecnológicas das patentes

tls230 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls230/tls230_total.parquet") %>%
  as.data.frame()

Microsoft_230 <- appln_id_Microsoft %>%
  inner_join(tls230, by = "appln_id")

n_distinct(Microsoft_230$appln_id)
# 41.452 - Perdemos 1.858 patentes


#Agrupando campo tec por frequência (quantidade de vezes que eles aparecem na base)
dist_techfield <- as.data.frame(table(Microsoft_230$techn_field_nr))

options(scipen = 999)
# Somar os valores de 'weight' por 'techn_field_nr'
weight_techfield <- Microsoft_230 %>%
  group_by(techn_field_nr) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE))


soma <- sum(weight_techfield$total_weight, na.rm = TRUE)

# Criar a nova coluna com percentual do peso do campo
weight_techfield$percent <- (weight_techfield$total_weight / soma)*100


wb <- createWorkbook()

addWorksheet(wb, "Dist")
writeData(wb, "Dist", dist_techfield)

addWorksheet(wb, "Weight")
writeData(wb, "Weight", weight_techfield)

saveWorkbook(wb, file = "Tech_Field_Microsoft.xlsx", overwrite = TRUE)



# As patentes estão concentradas em IA??


tls224 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls224/tls224_total.parquet") %>%
  as.data.frame()

#Códigos associados à IA: G06N3, G06N5, G06N7, G06N20, G06F40, G06F17/18, G06F3, G16H, G16B, A61B5/00, B25J9/16
# Códigos validados pela nossa lista de CPCs 4.0

subset_Microsoft <- Microsoft_total %>% 
  select(appln_id) %>% 
  distinct()


Microsoft_CPC <- subset_Microsoft %>%
  left_join(tls224, by = "appln_id")

Microsoft_CPC$cpc_class_symbol <- gsub(" ", "", Microsoft_CPC$cpc_class_symbol)


codigos_IA <- c("G06N3", "G06N5", "G06N7", "G06N20", 
                "G06F40", "G06F17/18", "G06F3", 
                "G16H", "G16B", "A61B5/00", "B25J9/16")

regex_IA <- paste0("^", codigos_IA, collapse = "|")

Microsoft_IA <- Microsoft_CPC %>%
  filter(grepl(regex_IA, cpc_class_symbol))


Microsoft_IA_unique <- Microsoft_IA %>%
  distinct(appln_id, .keep_all = TRUE)
# 17.295

wb <- createWorkbook()

addWorksheet(wb, "Microsoft_IA")
writeData(wb, "Microsoft_IA", Microsoft_IA)

addWorksheet(wb, "Unique")
writeData(wb, "Unique", Microsoft_IA_unique)

saveWorkbook(wb, file = "Microsoft_IA.xlsx", overwrite = TRUE)



# Quantas dessas patentes são 4.0?
tls224 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls224/tls224_total.parquet") %>%
  as.data.frame()


subset_Microsoft <- Microsoft_pat %>% 
  select(appln_id) %>% 
  distinct()


Microsoft_CPC <- subset_Microsoft %>%
  left_join(tls224, by = "appln_id")

Microsoft_CPC$cpc_class_symbol <- gsub(" ", "", Microsoft_CPC$cpc_class_symbol)

CPC_List <- read_excel("C:/Users/cveneo/Desktop/Financ_MI/Lista_CPC_2025_FINAL.xlsx", sheet = 'Final_List', col_names = FALSE)

names(CPC_List)[1] <- "cpc_class_symbol"


Microsoft_pat_4.0 <- Microsoft_CPC %>%
  filter(cpc_class_symbol %in% CPC_List$cpc_class_symbol)
# 214.204 obs

Microsoft_pat_4.0_unique <- Microsoft_pat_4.0 %>% 
  select(appln_id) %>% 
  distinct()
# 37.288 patentes 4.0

wb <- createWorkbook()

addWorksheet(wb, "Microsoft_4.0")
writeData(wb, "Microsoft_4.0", Microsoft_pat_4.0)

addWorksheet(wb, "Unique")
writeData(wb, "Unique", Microsoft_pat_4.0_unique)

saveWorkbook(wb, file = "Microsoft_4.0.xlsx", overwrite = TRUE)






# Importânica das patentes
Microsoft_pat <- read.csv2("C:/Users/cveneo/Desktop/Financ_MI/Microsoft_pat.csv")
#Preciso recuperar a DOCDB_FAMILY_SIZE da TLS201

tls201 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls201/tls201_total.parquet") %>%
  as.data.frame()

tls201_filtrada <- tls201 %>%
  filter(earliest_filing_year >= 2015,
         earliest_filing_year <= 2024)

tls201_filtrada <- tls201_filtrada %>%
  select(appln_id, 
         docdb_family_id,
         docdb_family_size,
         appln_auth) %>%
  collect()

Microsoft_pat$appln_id <- as.character(Microsoft_pat$appln_id)

Microsoft_pat_family <- tls201_filtrada %>%
  inner_join(Microsoft_pat, by = "appln_id")


#Frequencia do tamanho das docdb_family
Microsoft_pat_unique <- Microsoft_pat_family %>%
  distinct(docdb_family_id.x, .keep_all = TRUE)

family_size <- as.data.frame(table(Microsoft_pat_unique$docdb_family_size))

family_size$pct <- (family_size$Freq / (sum(family_size$Freq))) * 100


# Tamanho médio da família (simples)
Microsoft_pat_unique$docdb_family_size <- as.numeric(Microsoft_pat_unique$docdb_family_size)

media_familia <- mean(Microsoft_pat_unique$docdb_family_size)

# Proporção de famílias com tamanho > 1
proporcao_maior_1 <- sum(Microsoft_pat_unique$docdb_family_size > 1) / nrow(Microsoft_pat_unique)

cat("Tamanho médio da família DOCDB:", round(media_familia, 2), "\n")
cat("Proporção de famílias com tamanho > 1:", round(proporcao_maior_1 * 100, 2), "%\n")




#Comparando com o total das patentes do mesmo período
tls201_unique <- tls201_filtrada %>%
  distinct(docdb_family_id, .keep_all = TRUE)


family_size_201 <- as.data.frame(table(tls201_unique$docdb_family_size))

options(scipen = 999)
family_size_201$pct <- (family_size_201$Freq / (sum(family_size_201$Freq))) * 100



tls201_unique$docdb_family_size <- as.numeric(tls201_unique$docdb_family_size)

media_familia_201 <- mean(tls201_unique$docdb_family_size)


proporcao_maior_1_201 <- sum(tls201_unique$docdb_family_size > 1) / nrow(tls201_unique)

cat("Tamanho médio da família DOCDB:", round(media_familia_201, 2), "\n")
cat("Proporção de famílias com tamanho > 1:", round(proporcao_maior_1_201 * 100, 2), "%\n")


wb <- createWorkbook()

addWorksheet(wb, "Microsoft")
writeData(wb, "Microsoft", family_size)

addWorksheet(wb, "Total")
writeData(wb, "Total", family_size_201)

saveWorkbook(wb, file = "C:/Users/cveneo/Desktop/Financ_MI/Family_size.xlsx", overwrite = TRUE)




# País do depósito da patente

Microsoft_pat <- read.csv2("C:/Users/cveneo/Desktop/Financ_MI/Microsoft_pat.csv")
#Preciso recuperar a appln_auth da TLS201

tls201 <- read_parquet("C:/Users/cveneo/Desktop/Lendo_PATSTAT/tls201/tls201_total.parquet") %>%
  as.data.frame()

tls201_filtrada <- tls201 %>%
  filter(earliest_filing_year >= 2015,
         earliest_filing_year <= 2024)

tls201_filtrada <- tls201_filtrada %>%
  select(appln_id, appln_auth) %>%
  collect()

Microsoft_pat$appln_id <- as.character(Microsoft_pat$appln_id)

Microsoft_pat_office <- tls201_filtrada %>%
  inner_join(Microsoft_pat, by = "appln_id")

Freq_office_Microsoft <- as.data.frame(table(Microsoft_pat_office$appln_auth))

write.xlsx(Freq_office_Microsoft, file = "Freq_office_Microsoft.xlsx")


