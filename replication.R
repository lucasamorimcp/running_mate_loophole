#packages
library(data.table)
library(fixest)
library(ggplot2)
library(patchwork)
library(ragg)
library(HonestDiD)


#encoding
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United States.utf8")

#statutorydata
statutory_data <- fread("fefc_2020_statutory.csv")

#candidatedata
exec <- data.table()
council <- data.table()
runoff <- data.table()
for (ano in c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) {
  arquivo <- unzip(paste0("consulta_cand_", ano, ".zip"), files = paste0("consulta_cand_", ano, "_BRASIL.csv"), exdir = tempdir())
  cand <- fread(arquivo, sep = ";", encoding = "Latin-1", colClasses = "character", na.strings = c("NA", ""),
                select = c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "NR_TURNO", "SG_UF", "SG_UE", "NM_UE", "CD_CARGO", "SQ_CANDIDATO",
                           "NR_CANDIDATO", "NM_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "DT_NASCIMENTO", "DS_GENERO",
                           "DS_COR_RACA", "SG_PARTIDO", "NR_PARTIDO", "TP_AGREMIACAO", "SQ_COLIGACAO", "DS_COMPOSICAO_COLIGACAO",
                           "DS_SITUACAO_CANDIDATURA", "DS_SIT_TOT_TURNO"))
  unlink(arquivo)
  for (v in names(cand)) set(cand, j = v, value = trimws(enc2utf8(cand[[v]])))
  for (v in names(cand)) set(cand, i = which(cand[[v]] == ""), j = v, value = NA_character_)
  cand <- cand[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  exec <- rbind(exec, cand[NR_TURNO == "1" & CD_CARGO %chin% c("11", "12"),
                           .(ANO_ELEICAO, SG_UF, SG_UE, NM_UE, CD_CARGO, SQ_CANDIDATO, NR_CANDIDATO, NM_CANDIDATO,
                             NR_TITULO_ELEITORAL_CANDIDATO, DT_NASCIMENTO, DS_GENERO, DS_COR_RACA, SG_PARTIDO, NR_PARTIDO,
                             TP_AGREMIACAO, SQ_COLIGACAO, DS_COMPOSICAO_COLIGACAO, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO)])
  council <- rbind(council, cand[NR_TURNO == "1" & CD_CARGO == "13",
                                 .(ANO_ELEICAO, SG_UF, SG_UE, SQ_CANDIDATO, NM_CANDIDATO, NR_TITULO_ELEITORAL_CANDIDATO,
                                   DT_NASCIMENTO, DS_GENERO, SG_PARTIDO, NR_PARTIDO, DS_SIT_TOT_TURNO)])
  runoff <- rbind(runoff, cand[NR_TURNO == "2" & CD_CARGO == "11", .(ANO_ELEICAO, SG_UE, NR_CANDIDATO, DS_SIT_TOT_TURNO)])
  rm(cand)
  gc()
}

#financedata
arquivo <- unzip("prestacao_contas_final_2016.zip", files = c("receitas_partidos_prestacao_contas_final_2016_brasil.txt",
                                                              "despesas_partidos_prestacao_contas_final_2016_brasil.txt"), exdir = tempdir())
contas_partidos <- rbindlist(lapply(arquivo, fread, sep = ";", encoding = "Latin-1", colClasses = "character", na.strings = c("NA", "")), fill = TRUE)
unlink(arquivo)
setnames(contas_partidos, enc2utf8(names(contas_partidos)))
contas_partidos <- contas_partidos[, .(linha = .I, sigla = toupper(trimws(fcoalesce(`Sigla  Partido`, ""))),
                                       cnpj = trimws(fcoalesce(`CNPJ Prestador Conta`, "")), recibo = fcoalesce(`Número recibo eleitoral`, ""))]
contas_partidos[startsWith(cnpj, "#"), cnpj := ""]
numero_partido <- contas_partidos[substr(recibo, 1, 1) == "P" & grepl("^[0-9]+$", substr(recibo, 2, 3)),
                                  .(n = .N, primeira = min(linha)), by = .(sigla, numero = substr(recibo, 2, 3))]
numero_partido <- numero_partido[order(sigla, -n, primeira)][, .SD[1], by = sigla]
cnpj_partido <- contas_partidos[, .(n = .N, primeira = min(linha)), by = .(cnpj, sigla)][order(cnpj, -n, primeira)][, .SD[1], by = cnpj]
cnpj_partido <- merge(cnpj_partido, numero_partido[, .(sigla, numero)], by = "sigla", all.x = TRUE)[, .(cnpj, numero = fcoalesce(numero, ""))]
nomes_2016 <- data.table(chave = c("TRABALHISTA DO BRASIL", "HUMANISTA DA SOLIDARIEDADE", "SOCIAL DEMOCRATA CRISTAO", "REPUBLICANO PROGRESSISTA",
                                   "RENOVADOR TRABALHISTA", "TRABALHISTA CRISTAO", "TRABALHISTA NACIONAL", "TRABALHISTA BRASILEIRO",
                                   "DEMOCRATICO TRABALHISTA", "DOS TRABALHADORES", "MOVIMENTO DEMOCRATICO", "SOCIAL DEMOCRACIA",
                                   "POPULAR SOCIALISTA", "SOCIALISTA BRASILEIRO", "SOCIALISMO E LIBERDADE", "COMUNISTA DO BRASIL",
                                   "SOCIAL DEMOCRATICO", "REPUBLICANO BRASILEIRO", "ORDEM SOCIAL", "DA REPUBLICA", "PROGRESSISTA", "DEMOCRATAS",
                                   "VERDE", "SOCIAL CRISTAO", "SOCIAL LIBERAL", "ECOLOGICO NACIONAL", "MOBILIZACAO NACIONAL",
                                   "REDE SUSTENTABILIDADE", "PATRIA LIVRE", "MULHER BRASILEIRA", "SOLIDARIEDADE",
                                   "SOCIALISTA DOS TRABALHADORES UNIFICADO", "PARTIDO RENOVADOR TRABALHISTA BRASILEIRO",
                                   "PARTIDO LIBERAL", "DEMOCRACIA CRISTA", "COMUNISTA BRASILEIRO", "CAUSA OPERARIA"),
                         numero = c("70", "31", "27", "44", "28", "36", "19", "14", "12", "13", "15", "45", "23", "40", "50", "65", "55", "10",
                                    "90", "22", "11", "25", "43", "20", "17", "51", "33", "18", "54", "35", "77",
                                    "16", "28", "22", "27", "21", "29"))
rec <- data.table()
dp <- data.table()
council_transfers <- data.table()
inkind <- data.table()
tr <- data.table()
for (ano in c(2012, 2016, 2020, 2024)) {
  gc()
  zip <- c("2012" = "prestacao_final_2012.zip", "2016" = "prestacao_contas_final_2016.zip",
           "2020" = "prestacao_de_contas_eleitorais_candidatos_2020.zip", "2024" = "prestacao_de_contas_eleitorais_candidatos_2024.zip")[[as.character(ano)]]
  nome <- c("2012" = "receitas_candidatos_2012_brasil.txt", "2016" = "receitas_candidatos_prestacao_contas_final_2016_brasil.txt",
            "2020" = "receitas_candidatos_2020_BRASIL.csv", "2024" = "receitas_candidatos_2024_BRASIL.csv")[[as.character(ano)]]
  arquivo <- unzip(zip, files = nome, exdir = tempdir())
  colunas <- enc2utf8(names(fread(arquivo, sep = ";", encoding = "Latin-1", nrows = 0)))
  if (ano <= 2016) {
    usar <- c(sq = "Sequencial Candidato", office = "Cargo", origin = "Tipo receita", source = "Fonte recurso", kind = "Especie recurso",
              value = "Valor receita", donor_name = "Nome do doador", donor_id = "CPF/CNPJ do doador", rm_id = "CPF do vice/suplente",
              donor_party = "Número partido doador", donor_number = "Número candidato doador", ue = "Numero UE", donor_ue = "Sigla UE doador",
              cnpj_account = "CNPJ Prestador Conta", donor_name_rfb = "Nome do doador (Receita Federal)")
  } else {
    usar <- c(sq = "SQ_CANDIDATO", office = "DS_CARGO", origin = "DS_ORIGEM_RECEITA", source = "DS_FONTE_RECEITA", kind = "DS_ESPECIE_RECEITA",
              nature = "DS_NATUREZA_RECEITA", value = "VR_RECEITA", donor_name = "NM_DOADOR", donor_id = "NR_CPF_CNPJ_DOADOR",
              rm_id = "NR_CPF_VICE_CANDIDATO", donor_office = "DS_CARGO_CANDIDATO_DOADOR", donor_party = "NR_PARTIDO_DOADOR",
              donor_sq = "SQ_CANDIDATO_DOADOR", donor_name_rfb = "NM_DOADOR_RFB", description = "DS_NATUREZA_RECURSO_ESTIMAVEL")
  }
  usar <- usar[usar %in% colunas]
  r <- fread(arquivo, sep = ";", encoding = "Latin-1", colClasses = "character", na.strings = c("NA", ""), select = match(usar, colunas))
  unlink(arquivo)
  setnames(r, names(usar))
  for (v in setdiff(c("sq", "office", "origin", "source", "kind", "nature", "value", "donor_name", "donor_id", "rm_id", "donor_office",
                      "donor_party", "donor_number", "ue", "donor_ue", "donor_sq", "cnpj_account", "donor_name_rfb", "description"), names(r))) {
    set(r, j = v, value = NA_character_)
  }
  for (v in names(r)) set(r, j = v, value = enc2utf8(fcoalesce(r[[v]], "")))
  r[, linha := .I]
  r[, sq := trimws(sq)]
  r[, office := trimws(office)]
  r[, valor := trimws(value)]
  r[grepl(",", valor, fixed = TRUE), valor := gsub(",", ".", gsub(".", "", valor, fixed = TRUE), fixed = TRUE)]
  r[, v := suppressWarnings(as.numeric(valor))]
  r[is.na(v) | startsWith(valor, "#"), v := 0]
  r[, cash := !grepl("ESTIM", toupper(paste(kind, nature)), fixed = TRUE)]
  r[, fefc := grepl("ESPECIAL", toupper(source), fixed = TRUE)]
  r[, fp := grepl("PARTID", toupper(source), fixed = TRUE)]
  r[, party := grepl("PARTIDO", toupper(origin), fixed = TRUE) |
      (ano == 2012 & grepl("COMIT", toupper(origin), fixed = TRUE) & grepl("COMIT|DIRE", toupper(donor_name)))]
  if (ano >= 2020) {
    donor_names <- rbind(nomes_2016, data.table(
      chave = c("REPUBLICANOS", "PODEMOS", "CIDADANIA", "AVANTE", "AGIR", "UNIAO BRASIL", "RENOVACAO DEMOCRATICA",
                "RENOVADOR DEMOCRATICO", "POR MAIS BRASIL", "MOBILIZACAO NACIONAL", "MOBILIZA", "PATRIOTA", "PMDB", "NOVO", "UNIDADE POPULAR"),
      numero = c("10", "19", "23", "70", "36", "44", "25", "25", "35", "33", "33", "51", "15", "30", "80")))
    donor_identity <- unique(r[party & grepl("^[0-9]{14}$", donor_id), .(donor_id, donor_name_rfb)])
    donor_identity[, registered_name := iconv(chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇÑ", "AAAAAEEEEIIIIOOOOOUUUUCN",
      toupper(donor_name_rfb)), "UTF-8", "ASCII", sub = "")]
    donor_identity <- donor_identity[grepl("PARTIDO|DIRETOR|COMISS|^REPUBLICANOS|^PODEMOS|^CIDADANIA|^AVANTE|^AGIR|^UNIAO BRASIL|^MOBILIZA|^PATRIOTA|^PMDB|^PROGRESSISTAS|^REDE SUSTENTABILIDADE|^SOLIDARIEDADE|^DEMOCRATAS|^NOVO|^UNIDADE POPULAR|^DEMOCRACIA CRISTA", registered_name) &
      !grepl("^ELEICAO|FEDERAC", registered_name)]
    donor_identity[, numero := NA_character_]
    donor_names[, pattern := fifelse(grepl(" ", chave, fixed = TRUE), chave,
      paste0("(^|PARTIDO )", chave, "([ -]|$)"))]
    donor_names[chave == "PROGRESSISTA", pattern := "(^|PARTIDO )PROGRESSISTAS?([ -]|$)"]
    for (k in order(nchar(donor_names$chave))) donor_identity[grepl(donor_names$pattern[k], registered_name), numero := donor_names$numero[k]]
    donor_identity <- donor_identity[numero %chin% unique(exec[ANO_ELEICAO == as.character(ano), NR_PARTIDO])]
    donor_identity <- donor_identity[, if (uniqueN(numero) == 1L) .SD[1], by = donor_id]
    donor_corrections <- r[party == TRUE][donor_identity, on = "donor_id", nomatch = 0][donor_party != numero,
      .(records = .N, amount = sum(v), fefc = sum(v[fefc])), by = .(reported = donor_party, corrected = numero)]
    print(donor_corrections)
    r[donor_identity, on = "donor_id", donor_party := fifelse(party, numero, donor_party)]
  }
  r[, from_rm := grepl("VICE", toupper(donor_office), fixed = TRUE) | (!trimws(rm_id) %chin% c("", "-4", "#NULO", "#NULO#") & trimws(donor_id) == trimws(rm_id))]
  rec <- rbind(rec, r[, .(year = as.integer(ano), CARGO = office[1], total = round(sum(v), 2), party = round(sum(v * party), 2),
                          party_cash = round(sum(v * (party & cash)), 2), fefc = round(sum(v * fefc), 2), fp = round(sum(v * fp), 2),
                          fefc_party = round(sum(v * (fefc & party)), 2), fp_party = round(sum(v * (fp & party)), 2),
                          from_rm = round(sum(v * from_rm), 2)), by = .(SQ_CANDIDATO = sq)])
  r <- r[toupper(office) %chin% c("PREFEITO", "VEREADOR")]
  if (ano == 2016) {
    cnpj_prefeito <- r[toupper(office) == "PREFEITO", .(cnpj = fifelse(startsWith(trimws(cnpj_account), "#"), "", trimws(cnpj_account)), sq, linha)]
    cnpj_prefeito <- cnpj_prefeito[order(linha)][, .SD[.N], by = cnpj][, .(cnpj, sq_prefeito = sq)]
  }
  if (ano >= 2020) {
    council_transfers <- rbind(council_transfers,
      r[toupper(office) == "VEREADOR" & party & fefc,
        .(year = as.integer(ano), fefc = round(sum(v), 2)),
        by = .(SQ_CANDIDATO = sq, donor_party_number = trimws(donor_party))])
  }
  doa <- r[toupper(office) == "PREFEITO" & party == TRUE]
  if (ano == 2016) {
    doa[, cnpj := fifelse(startsWith(trimws(donor_id), "#"), "", trimws(donor_id))]
    doa <- merge(doa, cnpj_partido, by = "cnpj", all.x = TRUE, sort = FALSE)
    doa[, method := fifelse(!is.na(numero) & numero != "", "cnpj", "name")]
    doa[, nome_limpo := iconv(chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇÑ", "AAAAAEEEEIIIIOOOOOUUUUCN", toupper(donor_name_rfb)), "UTF-8", "ASCII", sub = "")]
    doa[, p := numero]
    name_patterns <- fifelse(grepl(" ", nomes_2016$chave, fixed = TRUE), nomes_2016$chave,
      paste0("(^|PARTIDO )", nomes_2016$chave, "([ -]|$)"))
    name_patterns[nomes_2016$chave == "PROGRESSISTA"] <- "(^|PARTIDO )PROGRESSISTAS?([ -]|$)"
    for (k in order(nchar(nomes_2016$chave))) doa[method == "name" & grepl(name_patterns[k], nome_limpo), p := nomes_2016$numero[k]]
    doa[method == "name" & !grepl(paste(name_patterns, collapse = "|"), nome_limpo), p := ""]
    doa[is.na(p), p := ""]
  } else {
    doa[, p := fifelse(startsWith(trimws(donor_party), "#"), "", trimws(donor_party))]
    doa[, method := "field"]
  }
  setorder(doa, linha)
  dp <- rbind(dp, doa[, .(year = as.integer(ano), party = round(sum(v), 2), fefc_party = round(sum(v * fefc), 2)), by = .(SQ_CANDIDATO = sq, NR_PARTIDO_DOADOR = p, method)])
  est <- doa[cash == FALSE]
  est[, x := iconv(chartr("áàâãäéèêëíìîïóòôõöúùûüçñ", "aaaaaeeeeiiiiooooouuuucn", tolower(fifelse(rep(ano >= 2020, .N), description, ""))), "UTF-8", "ASCII", sub = "")]
  est[, group := fcase(trimws(x) == "", "unclassified",
                       grepl("impresso|adesivo|placa|estandarte|faixa|jornais", x), "material",
                       grepl("radio|televis|video|jingle|impulsionamento|internet|carros de som", x), "media",
                       grepl("servico|pesquisa", x), "services",
                       default = "other")]
  est[, fonte := fifelse(fefc, "fefc", fifelse(fp, "party_fund", "other_source"))]
  inkind <- rbind(inkind, est[, .(year = as.integer(ano), value = round(sum(v), 2)), by = .(SQ_CANDIDATO = sq, group, source = fonte)])
  tra <- r[toupper(office) == "VEREADOR" & grepl("OUTROS CANDIDATOS", toupper(origin), fixed = TRUE)]
  if (ano >= 2020) {
    tra <- tra[toupper(trimws(donor_office)) == "PREFEITO"]
    tra[, chave := paste0("SQ:", fifelse(startsWith(trimws(donor_sq), "#"), "", trimws(donor_sq)))]
  } else if (ano == 2016) {
    tra[, chave := paste0("CNPJ:", fifelse(startsWith(trimws(donor_id), "#"), "", trimws(donor_id)))]
  } else {
    tra[, `:=`(num = trimws(donor_number), ue_c = trimws(ue), ue_d = trimws(donor_ue))]
    tra[startsWith(num, "#"), num := ""]
    tra[startsWith(ue_c, "#"), ue_c := ""]
    tra[startsWith(ue_d, "#"), ue_d := ""]
    tra <- tra[nchar(num) == 2 & ue_d != "" & ue_d == ue_c]
    tra[, chave := paste0("NR:", ue_d, ":", num)]
  }
  tra <- tra[, .(total = round(sum(v), 2), cash = round(sum(v * cash), 2), fefc = round(sum(v * fefc), 2), primeira = min(linha)), by = .(SQ_COUNCIL = sq, DONOR = chave)]
  if (ano == 2016) {
    tra[, cnpj := substring(DONOR, 6)]
    tra <- merge(tra, cnpj_prefeito, by = "cnpj")[order(primeira)]
    tra[, DONOR := paste0("SQ:", sq_prefeito)]
    tra <- tra[, .SD[.N], by = .(SQ_COUNCIL, DONOR)]
  }
  tr <- rbind(tr, tra[, .(year = as.integer(ano), SQ_COUNCIL, DONOR, total, cash, fefc)])
  rm(r, doa, est, tra)
}

#generaldata
cargos <- c("GOVERNADOR", "VICE-GOVERNADOR", "SENADOR", "1º SUPLENTE", "2º SUPLENTE", "1O SUPLENTE", "2O SUPLENTE", "PRESIDENTE", "VICE-PRESIDENTE")
g <- data.table()
for (ano in c(2002, 2006, 2010, 2014, 2018, 2022)) {
  zip <- paste0("consulta_cand_", ano, ".zip")
  nomes <- unzip(zip, list = TRUE)$Name
  nomes <- if (any(grepl("\\.CSV$", toupper(nomes)) & grepl("BRASIL", toupper(nomes)))) nomes[grepl("\\.CSV$", toupper(nomes)) & grepl("BRASIL", toupper(nomes))] else nomes[grepl("\\.(CSV|TXT)$", toupper(nomes))]
  arquivo <- unzip(zip, files = nomes, exdir = tempdir())
  gerais <- rbindlist(lapply(arquivo, fread, sep = ";", encoding = "Latin-1", colClasses = "character", na.strings = c("NA", "")), fill = TRUE)
  unlink(arquivo)
  for (v in setdiff(c("NR_TURNO", "NM_TIPO_ELEICAO", "DS_ELEICAO"), names(gerais))) set(gerais, j = v, value = NA_character_)
  for (v in names(gerais)) set(gerais, j = v, value = trimws(enc2utf8(gerais[[v]])))
  for (v in names(gerais)) set(gerais, i = which(gerais[[v]] == ""), j = v, value = NA_character_)
  gerais <- gerais[(is.na(NR_TURNO) | NR_TURNO == "1") & grepl("ORDIN", toupper(fcoalesce(NM_TIPO_ELEICAO, DS_ELEICAO, "ORDIN"))) &
                     toupper(trimws(fcoalesce(DS_CARGO, ""))) %chin% cargos]
  g <- rbind(g, gerais[, .(ANO_ELEICAO, SG_UF, DS_CARGO, SQ_CANDIDATO, NR_CANDIDATO, DS_GENERO, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO)])
  rm(gerais)
}

#registrydata
arquivo <- unzip("consulta_cand_2026.zip", files = "consulta_cand_2026_BRASIL.csv", exdir = tempdir())
reg <- fread(arquivo, sep = ";", encoding = "Latin-1", colClasses = "character", na.strings = c("NA", ""),
             select = c("NM_TIPO_ELEICAO", "SG_UF", "DS_CARGO", "SQ_CANDIDATO", "NR_CANDIDATO", "DS_GENERO"))
unlink(arquivo)
for (v in names(reg)) set(reg, j = v, value = trimws(enc2utf8(reg[[v]])))
for (v in names(reg)) set(reg, i = which(reg[[v]] == ""), j = v, value = NA_character_)
reg <- reg[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)) & toupper(trimws(fcoalesce(DS_CARGO, ""))) %chin% cargos, .(SG_UF, DS_CARGO, SQ_CANDIDATO, NR_CANDIDATO, DS_GENERO)]

#tickets
exec[, ANO := as.integer(ANO_ELEICAO)]
exec[, ID := fifelse(!is.na(NR_TITULO_ELEITORAL_CANDIDATO) & nchar(NR_TITULO_ELEITORAL_CANDIDATO) >= 8 & !grepl("^#|^-", NR_TITULO_ELEITORAL_CANDIDATO),
                     NR_TITULO_ELEITORAL_CANDIDATO, paste(toupper(NM_CANDIDATO), DT_NASCIMENTO, sep = "|"))]
heads <- exec[CD_CARGO == "11" & (DS_SIT_TOT_TURNO == "ELEITO" | grepl("^N.O ELEITO$", DS_SIT_TOT_TURNO) | grepl("TURNO", DS_SIT_TOT_TURNO))]
heads <- heads[order(ANO, SG_UE, NR_CANDIDATO, -as.numeric(SQ_CANDIDATO))][, .SD[1], by = .(ANO, SG_UE, NR_CANDIDATO)]
heads[, tkey := fifelse(ANO == 2000L & !is.na(SQ_COLIGACAO) & !(SQ_COLIGACAO %chin% c("-1", "", "#NULO#", "#NULO", "#NE")),
                        paste0("C", SQ_COLIGACAO), paste0("N", NR_CANDIDATO))]
rms <- exec[CD_CARGO == "12"]
rms[ANO == 2000L & nchar(NR_CANDIDATO) == 3L, NR_CANDIDATO := substr(NR_CANDIDATO, 1L, 2L)]
rms[, tkey := fifelse(ANO == 2000L & !is.na(SQ_COLIGACAO) & !(SQ_COLIGACAO %chin% c("-1", "", "#NULO#", "#NULO", "#NE")),
                      paste0("C", SQ_COLIGACAO), paste0("N", NR_CANDIDATO))]
rms[, priority := fcase(DS_SIT_TOT_TURNO == "ELEITO" | grepl("^N.O ELEITO$", DS_SIT_TOT_TURNO) | grepl("TURNO", DS_SIT_TOT_TURNO), 1L,
                        DS_SITUACAO_CANDIDATURA %chin% c("APTO", "DEFERIDO"), 2L,
                        DS_SITUACAO_CANDIDATURA %chin% c("#NE", "SUB JUDICE"), 3L,
                        default = 9L)]
rms <- rms[priority < 9][order(ANO, SG_UE, tkey, priority, -as.numeric(SQ_CANDIDATO))]
rms <- rms[, .SD[1], by = .(ANO, SG_UE, tkey)]
tk <- merge(heads[, .(ANO, SG_UF, SG_UE, NM_UE, tkey, NR_CANDIDATO, SQ_CANDIDATO, ID, DS_GENERO, SG_PARTIDO, NR_PARTIDO, TP_AGREMIACAO,
                      DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO)],
            rms[, .(ANO, SG_UE, tkey, SQ_CANDIDATO, ID, DS_GENERO, SG_PARTIDO)],
            by = c("ANO", "SG_UE", "tkey"), suffixes = c("_c", "_v"), all.x = TRUE)
setnames(tk, c("SG_UF", "NM_UE", "NR_PARTIDO", "TP_AGREMIACAO", "DS_COMPOSICAO_COLIGACAO", "DS_SIT_TOT_TURNO"),
         c("UF", "MUN", "NR_PARTIDO_c", "AGREMIACAO", "COLIGACAO", "RESULT_c"))
tk <- tk[DS_GENERO_c %chin% c("MASCULINO", "FEMININO") & DS_GENERO_v %chin% c("MASCULINO", "FEMININO")]
tk[, `:=`(head_female = as.integer(DS_GENERO_c == "FEMININO"), rm_female = as.integer(DS_GENERO_v == "FEMININO"))]
tk[, ticket_type := factor(paste0(fifelse(head_female == 1, "M", "H"), fifelse(rm_female == 1, "M", "H")), levels = c("HH", "HM", "MH", "MM"))]
runoff[, ANO := as.integer(ANO_ELEICAO)]
tk <- merge(tk, runoff[DS_SIT_TOT_TURNO == "ELEITO", .(ANO, SG_UE, NR_CANDIDATO, won_runoff = 1L)], by = c("ANO", "SG_UE", "NR_CANDIDATO"), all.x = TRUE)
tk[, won := as.integer(RESULT_c == "ELEITO" | (grepl("TURNO", RESULT_c) & !is.na(won_runoff)))]
tk[, won_runoff := NULL]
heads <- merge(heads, runoff[DS_SIT_TOT_TURNO == "ELEITO", .(ANO, SG_UE, NR_CANDIDATO, won_runoff = 1L)], by = c("ANO", "SG_UE", "NR_CANDIDATO"), all.x = TRUE)
heads[, won_head := as.integer(DS_SIT_TOT_TURNO == "ELEITO" | (grepl("TURNO", DS_SIT_TOT_TURNO) & !is.na(won_runoff)))]
tk[, n_tickets := .N, by = .(ANO, SG_UE)]
tk[, coalition_size := fifelse(AGREMIACAO == "PARTIDO ISOLADO", 1L, lengths(strsplit(gsub("\\(.*?\\)", "", COLIGACAO), "/")))]
family <- c(PPB = "PP", PMDB = "MDB", PFL = "DEM", PR = "PL", PPS = "CIDADANIA", PRB = "REPUBLICANOS", PTN = "PODE", PSDC = "DC",
            "PT DO B" = "AVANTE", PRN = "AGIR", PTC = "AGIR", PMN = "MOBILIZA", SD = "SOLIDARIEDADE", PEN = "PATRIOTA", "UNIÃO" = "UNIAO")
tk[, party_head := fcoalesce(unname(family[toupper(trimws(SG_PARTIDO_c))]), toupper(trimws(SG_PARTIDO_c)))]

#history
council[, ANO := as.integer(ANO_ELEICAO)]
council[, ID := fifelse(!is.na(NR_TITULO_ELEITORAL_CANDIDATO) & nchar(NR_TITULO_ELEITORAL_CANDIDATO) >= 8 & !grepl("^#|^-", NR_TITULO_ELEITORAL_CANDIDATO),
                        NR_TITULO_ELEITORAL_CANDIDATO, paste(toupper(NM_CANDIDATO), DT_NASCIMENTO, sep = "|"))]
history <- rbind(
  council[DS_SIT_TOT_TURNO %chin% c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "SUPLENTE", "MÉDIA") | grepl("^N.O ELEITO$", DS_SIT_TOT_TURNO),
          .(ANO, SG_UE, ID, office = "VEREADOR", won = as.integer(DS_SIT_TOT_TURNO %chin% c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA")))],
  heads[, .(ANO, SG_UE, ID, office = "PREFEITO", won = won_head == 1L)],
  tk[!is.na(ID_v), .(ANO, SG_UE, ID = ID_v, office = "VICE", won = won == 1L)]
)
history[, won := as.integer(won)]

#incumbency
tk <- merge(tk, history[office == "PREFEITO" & won == 1, .(ANO = ANO + 4L, SG_UE, ID_c = ID, incumbent_head = 1L)], by = c("ANO", "SG_UE", "ID_c"), all.x = TRUE)
tk[is.na(incumbent_head), incumbent_head := 0L]
tk[ANO == 2000, incumbent_head := NA_integer_]
prev <- unique(heads[won_head == 1, .(ANO, SG_UE, fam = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO))))])
prev[, ANO_next := ANO + 4L]
prev[, fam_next := fcase(ANO_next == 2020 & fam == "PHS", "PODE", ANO_next == 2020 & fam == "PRP", "PATRIOTA", ANO_next == 2020 & fam == "PPL", "PC DO B",
                         ANO_next == 2024 & fam %chin% c("DEM", "PSL"), "UNIAO", ANO_next == 2024 & fam %chin% c("PTB", "PATRIOTA"), "PRD",
                         ANO_next == 2024 & fam == "PSC", "PODE", ANO_next == 2024 & fam == "PROS", "SOLIDARIEDADE", default = fam)]
tk <- merge(tk, unique(prev[, .(ANO = ANO_next, SG_UE, party_head = fam_next, incumbent_party = 1L)]), by = c("ANO", "SG_UE", "party_head"), all.x = TRUE)
tk[is.na(incumbent_party), incumbent_party := 0L]
tk[ANO == 2000, incumbent_party := NA_integer_]
tk[, viable := as.integer(incumbent_head == 1L | incumbent_party == 1L)]
m <- merge(data.table(ID = tk$ID_c, ANO = tk$ANO, k = seq_len(nrow(tk))), history[won == 1, .(ID, ANO)], by = "ID",
           allow.cartesian = TRUE, suffixes = c("", "_h"))[ANO_h < ANO & ANO_h >= ANO - 8L]
tk[, experienced_head := 0L]
tk[unique(m$k), experienced_head := 1L]
m <- merge(data.table(ID = tk$ID_v, ANO = tk$ANO, k = seq_len(nrow(tk))), history[won == 1, .(ID, ANO)], by = "ID",
           allow.cartesian = TRUE, suffixes = c("", "_h"))[ANO_h < ANO & ANO_h >= ANO - 8L]
tk[, experienced_rm := fifelse(is.na(ID_v), NA_integer_, 0L)]
tk[unique(m$k), experienced_rm := 1L]
tk[ANO <= 2004, `:=`(experienced_head = NA_integer_, experienced_rm = NA_integer_)]

#municipalities
capitals <- data.table(UF = c("AC", "AL", "AP", "AM", "BA", "CE", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                              "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                       nome = c("RIO BRANCO", "MACEIO", "MACAPA", "MANAUS", "SALVADOR", "FORTALEZA", "VITORIA", "GOIANIA",
                                "SAO LUIS", "CUIABA", "CAMPO GRANDE", "BELO HORIZONTE", "BELEM", "JOAO PESSOA", "CURITIBA",
                                "RECIFE", "TERESINA", "RIO DE JANEIRO", "NATAL", "PORTO ALEGRE", "PORTO VELHO", "BOA VISTA",
                                "FLORIANOPOLIS", "SAO PAULO", "ARACAJU", "PALMAS"))
cap_ue <- unique(merge(tk[, .(UF, SG_UE, nome = toupper(iconv(MUN, "UTF-8", "ASCII//TRANSLIT")))], capitals, by = c("UF", "nome"))$SG_UE)
runoff_ue <- unique(runoff$SG_UE)

#identifiers
set.seed(20260926)
ids <- unique(c(tk$ID_c, tk$ID_v))
ids <- ids[!is.na(ids)]
code <- setNames(sample(length(ids)), ids)

#ticketfunds
tk <- merge(tk, rec[CARGO == "Prefeito", .(ANO = year, SQ_CANDIDATO_c = SQ_CANDIDATO, total, party, party_cash, fefc, fp, fefc_party, fp_party, from_rm)],
            by = c("ANO", "SQ_CANDIDATO_c"), all.x = TRUE)
endow <- rbind(tk[ANO >= 2020, .(ANO, SQ = SQ_CANDIDATO_c, fam = party_head)],
               council[ANO >= 2020, .(ANO, SQ = SQ_CANDIDATO, fam = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO))))])
endow <- merge(endow, rec[year >= 2020, .(ANO = year, SQ = SQ_CANDIDATO, fefc_party)], by = c("ANO", "SQ"))[, .(v = sum(fefc_party)), by = .(ANO, fam)][ANO == 2020L, .(fam, v)]
endow <- rbind(endow, data.table(fam = c("UNIAO", "PRD"), v = c(endow[fam %chin% c("DEM", "PSL"), sum(v)], endow[fam %chin% c("PTB", "PATRIOTA"), sum(v)])))
tk <- merge(tk, endow[, .(party_head = fam, endowment_2020 = v)], by = "party_head", all.x = TRUE)
statutory <- copy(statutory_data)[, .(fam = fcoalesce(unname(family[toupper(trimws(party_family))]), toupper(trimws(party_family))),
                                                                        v = fefc_2020_statutory_brl)]
statutory <- rbind(statutory, data.table(fam = c("UNIAO", "PRD"), v = c(statutory[fam %chin% c("DEM", "PSL"), sum(v)], statutory[fam %chin% c("PTB", "PATRIOTA"), sum(v)])))
tk <- merge(tk, statutory[, .(party_head = fam, statutory_2020 = v)], by = "party_head", all.x = TRUE)
exec[, black := fifelse(DS_COR_RACA %chin% c("PRETA", "PARDA"), 1L, fifelse(DS_COR_RACA == "BRANCA", 0L, NA_integer_))]
exec[ANO < 2016L, black := NA_integer_]
tk <- merge(tk, exec[ANO >= 2012L, .(ANO, SQ_CANDIDATO_c = SQ_CANDIDATO, head_black = black, nrp_c = NR_PARTIDO)], by = c("ANO", "SQ_CANDIDATO_c"), all.x = TRUE)
tk <- merge(tk, exec[ANO >= 2012L, .(ANO, SQ_CANDIDATO_v = SQ_CANDIDATO, rm_black = black, nrp_v = NR_PARTIDO)], by = c("ANO", "SQ_CANDIDATO_v"), all.x = TRUE)
tk <- merge(tk, council[grepl("^ELEITO", DS_SIT_TOT_TURNO) | DS_SIT_TOT_TURNO == "MÉDIA", .(seats = .N), by = .(ANO, SG_UE)], by = c("ANO", "SG_UE"), all.x = TRUE)
dp <- merge(dp[, .(ANO = year, SQ_CANDIDATO, NR_PARTIDO_DOADOR, method, party, fefc_party)], unique(tk[, .(ANO, SQ_CANDIDATO = SQ_CANDIDATO_c, nrp_c, nrp_v)]),
            by = c("ANO", "SQ_CANDIDATO"))
dp[, origin := fifelse(NR_PARTIDO_DOADOR == nrp_c, "own", fifelse(!is.na(nrp_v) & NR_PARTIDO_DOADOR == nrp_v & nrp_v != nrp_c, "rmp", "oth"))]
tk <- merge(tk, dcast(dp, ANO + SQ_CANDIDATO ~ origin, value.var = c("party", "fefc_party"), fun.aggregate = sum, fill = 0),
            by.x = c("ANO", "SQ_CANDIDATO_c"), by.y = c("ANO", "SQ_CANDIDATO"), all.x = TRUE)
tk <- merge(tk, dp[origin == "rmp" & method == "name", .(party_rmp_name = sum(party)), by = .(ANO, SQ_CANDIDATO)],
            by.x = c("ANO", "SQ_CANDIDATO_c"), by.y = c("ANO", "SQ_CANDIDATO"), all.x = TRUE)
tk <- merge(tk, merge(dcast(inkind, year + SQ_CANDIDATO ~ paste0("inkind_", group), value.var = "value", fun.aggregate = sum, fill = 0),
                      dcast(inkind, year + SQ_CANDIDATO ~ paste0("inkind_", source), value.var = "value", fun.aggregate = sum, fill = 0),
                      by = c("year", "SQ_CANDIDATO")),
            by.x = c("ANO", "SQ_CANDIDATO_c"), by.y = c("year", "SQ_CANDIDATO"), all.x = TRUE)
tr <- merge(tr[, .(ANO = year, SQ_COUNCIL, DONOR, total, cash, fefc)], council[ANO >= 2012L, .(ANO, SQ_COUNCIL = SQ_CANDIDATO, fem = DS_GENERO == "FEMININO")],
            by = c("ANO", "SQ_COUNCIL"))
tr[startsWith(DONOR, "SQ:"), sq_c := substring(DONOR, 4)]
tr12 <- tr[startsWith(DONOR, "NR:")]
tr12[, c("ue", "nr") := tstrsplit(substring(DONOR, 4), ":")]
tr12[, ue := sprintf("%05d", as.integer(ue))]
tr12 <- merge(tr12, unique(tk[ANO == 2012L, .(ue = SG_UE, nr = NR_CANDIDATO, sq_h = SQ_CANDIDATO_c)]), by = c("ue", "nr"))
tr <- rbind(tr[!startsWith(DONOR, "NR:"), .(ANO, sq_c, fem, total, cash, fefc)], tr12[, .(ANO, sq_c = sq_h, fem, total, cash, fefc)])
tk <- merge(tk, tr[!is.na(sq_c), .(to_male_council = sum(total[!fem]), to_female_council = sum(total[fem]),
                                   to_male_council_cash = sum(cash[!fem]), to_female_council_cash = sum(cash[fem]),
                                   fefc_to_male_council = sum(fefc[!fem]), fefc_to_female_council = sum(fefc[fem])), by = .(ANO, sq_c)],
            by.x = c("ANO", "SQ_CANDIDATO_c"), by.y = c("ANO", "sq_c"), all.x = TRUE)

#ticketdata
region <- c(AC = "North", AM = "North", AP = "North", PA = "North", RO = "North", RR = "North", TO = "North",
            AL = "Northeast", BA = "Northeast", CE = "Northeast", MA = "Northeast", PB = "Northeast", PE = "Northeast",
            PI = "Northeast", RN = "Northeast", SE = "Northeast", GO = "Center-West", MS = "Center-West", MT = "Center-West",
            ES = "Southeast", MG = "Southeast", RJ = "Southeast", SP = "Southeast", PR = "South", RS = "South", SC = "South")
tickets <- tk[, .(
  year = ANO, state = UF, region = unname(region[UF]), muni = SG_UE,
  capital = as.integer(SG_UE %chin% cap_ue), runoff_muni = as.integer(SG_UE %chin% runoff_ue),
  n_tickets, council_seats = seats,
  party_head, party_rm = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO_v))]), toupper(trimws(SG_PARTIDO_v))), rm_other_party = as.integer(nrp_v != nrp_c),
  head_female, rm_female, head_black, rm_black, ticket_type = as.character(ticket_type), won,
  incumbent_head, incumbent_party, viable, experienced_head, experienced_rm, coalition_size,
  id_head = unname(code[ID_c]), id_rm = unname(code[ID_v]),
  has_receipts = as.integer(!is.na(total) & total > 0),
  receipts_total = total, party_funds = party, party_funds_cash = party_cash,
  fefc, party_fund_fp = fp, fefc_from_party = fefc_party, fp_from_party = fp_party, from_rm,
  party_funds_own = party_own, party_funds_rm_party = party_rmp, party_funds_other = party_oth, party_funds_rm_party_name = party_rmp_name,
  inkind_material, inkind_media, inkind_services, inkind_other, inkind_unclassified, inkind_fefc, inkind_party_fund, inkind_other_source,
  fefc_own = fefc_party_own, fefc_rm_party = fefc_party_rmp, fefc_other = fefc_party_oth,
  to_male_council, to_female_council, to_male_council_cash, to_female_council_cash, fefc_to_male_council, fefc_to_female_council,
  endowment_2020, statutory_2020)]
fin <- c("receipts_total", "party_funds", "party_funds_cash", "fefc", "party_fund_fp", "fefc_from_party", "fp_from_party", "from_rm",
         "party_funds_own", "party_funds_rm_party", "party_funds_other", "fefc_own", "fefc_rm_party", "fefc_other", "party_funds_rm_party_name",
         "inkind_material", "inkind_media", "inkind_services", "inkind_other", "inkind_unclassified", "inkind_fefc", "inkind_party_fund", "inkind_other_source",
         "to_male_council", "to_female_council", "to_male_council_cash", "to_female_council_cash", "fefc_to_male_council", "fefc_to_female_council")
for (v in fin) set(tickets, which(tickets$year < 2012L), v, NA_real_)
for (v in fin) set(tickets, which(tickets$year >= 2012L & is.na(tickets[[v]])), v, 0)
setorder(tickets, year, state, muni)
tickets[, ticket_id := .I]

#generalslots
g[, year := as.integer(ANO_ELEICAO)]
g[, office := fcase(DS_CARGO %chin% c("GOVERNADOR", "VICE-GOVERNADOR"), "governor", DS_CARGO %chin% c("SENADOR", "1º SUPLENTE", "2º SUPLENTE"), "senate",
                    DS_CARGO %chin% c("PRESIDENTE", "VICE-PRESIDENTE"), "president")]
g[, is_head := DS_CARGO %chin% c("GOVERNADOR", "SENADOR", "PRESIDENTE")]
g[, female := fifelse(DS_GENERO == "FEMININO", 1L, fifelse(DS_GENERO == "MASCULINO", 0L, NA_integer_))]
g[, uf := fifelse(office == "president", "BR", SG_UF)]
hd <- unique(g[is_head == TRUE & DS_SIT_TOT_TURNO %chin% c("ELEITO", "NÃO ELEITO", "2º TURNO") & !is.na(female),
               .(year, uf, office, nr = NR_CANDIDATO, head_female = female)], by = c("year", "uf", "office", "nr"))
gr <- g[is_head == FALSE & !is.na(female)]
gr[, pri := fifelse(DS_SITUACAO_CANDIDATURA == "APTO" & !startsWith(DS_SIT_TOT_TURNO, "#"), 1L, fifelse(DS_SITUACAO_CANDIDATURA == "APTO", 2L, 3L))]
setorder(gr, year, uf, office, NR_CANDIDATO, DS_CARGO, pri, -SQ_CANDIDATO)
gr <- gr[, .SD[1], by = .(year, uf, office, nr = NR_CANDIDATO, DS_CARGO)]
pairs <- merge(hd, gr[, .(year, uf, office, nr, rm_female = female)], by = c("year", "uf", "office", "nr"))
general_slots <- rbind(hd[, .(year, state = uf, office, female = head_female, rm_slot = 0L)],
                       pairs[head_female == 0, .(year, state = uf, office, female = rm_female, rm_slot = 1L)])
setorder(general_slots, office, year, state, rm_slot)

#currentregistry
reg[, office := fcase(DS_CARGO %chin% c("GOVERNADOR", "VICE-GOVERNADOR"), "governor", DS_CARGO %chin% c("SENADOR", "1º SUPLENTE", "2º SUPLENTE"), "senate",
                      DS_CARGO %chin% c("PRESIDENTE", "VICE-PRESIDENTE"), "president")]
reg[, uf := fifelse(office == "president", "BR", SG_UF)]
setorder(reg, uf, office, NR_CANDIDATO, DS_CARGO, -SQ_CANDIDATO)
reg <- reg[, .SD[1], by = .(uf, office, NR_CANDIDATO, DS_CARGO)][DS_GENERO %chin% c("MASCULINO", "FEMININO")]
reg[, female := as.integer(DS_GENERO == "FEMININO")]
reg <- merge(reg, reg[DS_CARGO %chin% c("GOVERNADOR", "SENADOR", "PRESIDENTE"), .(uf, office, NR_CANDIDATO, head_female = female)],
             by = c("uf", "office", "NR_CANDIDATO"), all.x = TRUE)
registry_2026 <- reg[, .(state = uf, office, rm_slot = as.integer(!DS_CARGO %chin% c("GOVERNADOR", "SENADOR", "PRESIDENTE")),
                         alternate = fcase(DS_CARGO == "1º SUPLENTE", 1L, DS_CARGO == "2º SUPLENTE", 2L), female, head_female)]
setorder(registry_2026, office, state, rm_slot)


#introduction
print(registry_2026[office == "governor", .(women = mean(female)), by = .(slot = fifelse(rm_slot == 1, "vice-governor", "governor"))])
print(registry_2026[office == "governor" & rm_slot == 1 & head_female == 0, .(female_running_mates_of_men = mean(female))])
print(general_slots[office == "governor" & rm_slot == 1 & year == 2014, .(female_running_mates_of_men = mean(female))])
print(tickets[year %in% c(2016, 2024), .(women_heading = mean(head_female), female_running_mates_of_men = mean(rm_female[head_female == 0])), by = year])
print(rec[year == 2024, .(fefc_declared_by_candidates_billion = sum(fefc) / 1e9)])
print(tickets[year == 2024 & won == 1, .(mayoralties = .N, won_by_women = mean(head_female))])
print(council[ANO == 2024 & (grepl("^ELEITO", DS_SIT_TOT_TURNO) | DS_SIT_TOT_TURNO == "MÉDIA"), .(council_seats = .N, won_by_women = mean(DS_GENERO == "FEMININO"))])

#data
print(tickets[, .(municipalities = uniqueN(muni), tickets = .N), by = year])
print(nrow(tickets))
print(rec[year == 2020, .(candidacies_with_receipts = .N), by = CARGO])
print(tickets[year >= 2012, .(matched_to_receipts = mean(has_receipts)), by = year])

#hypothesis1
tickets[, post := as.integer(year >= 2020)]
print(tickets[, .(female_running_mates_of_men = mean(rm_female[head_female == 0]), women_heading = mean(head_female)), by = year])
slots <- rbind(
  tickets[head_female == 0, .(year, state, region, muni, runoff_muni, capital, n_tickets, council_seats, post, female = rm_female,
                              black_woman = as.integer(rm_female == 1 & rm_black == 1), white_woman = as.integer(rm_female == 1 & rm_black == 0),
                              black_man = as.integer(rm_female == 0 & rm_black == 1), race_known = !is.na(rm_black), rm_slot = 1L)],
  tickets[, .(year, state, region, muni, runoff_muni, capital, n_tickets, council_seats, post, female = head_female,
              black_woman = as.integer(head_female == 1 & head_black == 1), white_woman = as.integer(head_female == 1 & head_black == 0),
              black_man = as.integer(head_female == 0 & head_black == 1), race_known = !is.na(head_black), rm_slot = 0L)]
)
h1_es <- feols(female ~ rm_slot + i(year, rm_slot, ref = 2016) | muni^year, slots, cluster = ~muni)
print(wald(h1_es, keep = "^year::(2000|2004|2008|2012):rm_slot$"))
print(data.table(term = names(coef(h1_es)), pp = 100 * coef(h1_es), low = 100 * (coef(h1_es) - 1.96 * se(h1_es)), high = 100 * (coef(h1_es) + 1.96 * se(h1_es))))
print(tickets[head_female == 0 & year >= 2020, .(male_headed_tickets = .N), by = year][, additional_female_running_mates := male_headed_tickets * coef(h1_es)[paste0("year::", year, ":rm_slot")]][])
h1_dd <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots, cluster = ~muni)
print(data.table(pp = 100 * coef(h1_dd)[["rm_slot:post"]], low = 100 * (coef(h1_dd)[["rm_slot:post"]] - 1.96 * se(h1_dd)[["rm_slot:post"]]),
           high = 100 * (coef(h1_dd)[["rm_slot:post"]] + 1.96 * se(h1_dd)[["rm_slot:post"]]), p = pvalue(h1_dd)[["rm_slot:post"]]))

#governors
general_slots[, post := as.integer(year >= 2018)]
general_slots[, race := paste(year, state, office)]
general_slots[, rm_post := rm_slot * post]
general_slots[, `:=`(rm_2002 = rm_slot * (year == 2002), rm_2006 = rm_slot * (year == 2006), rm_2010 = rm_slot * (year == 2010),
                     rm_2018 = rm_slot * (year == 2018), rm_2022 = rm_slot * (year == 2022))]
governor <- general_slots[office == "governor"]
senate <- general_slots[office == "senate"]
print(general_slots[office != "president", .(women = mean(female)), by = .(office, slot = fifelse(rm_slot == 1, "running mates of male heads", "heads"), year)][order(office, slot, year)])
print(uniqueN(governor$state))
h1g_es <- feols(female ~ rm_slot + i(year, rm_slot, ref = 2014) | race, governor, cluster = ~state)
print(wald(h1g_es, keep = "^year::(2002|2006|2010):rm_slot$"))
print(100 * coef(h1g_es))
h1g_dd <- feols(female ~ rm_slot + rm_slot:post | race, governor, cluster = ~state)
print(100 * coef(h1g_dd))
h1s_es <- feols(female ~ rm_slot + i(year, rm_slot, ref = 2014) | race, senate, cluster = ~state)
print(100 * coef(h1s_es))
n_bootstrap <- 9999
webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
bootstrap <- data.table()
bootstrap_cases <- list(
  list(name = "H1 gubernatorial tickets, 2018 and 2022", data = governor, y = "female", x = c("rm_slot", "rm_post"), fe = list("race"), w = NULL,
       cluster = "state", test = c(rm_post = 1)),
  list(name = "H1 gubernatorial tickets, 2018 coefficient of the event study", data = governor, y = "female",
       x = c("rm_slot", "rm_2002", "rm_2006", "rm_2010", "rm_2018", "rm_2022"), fe = list("race"), w = NULL, cluster = "state", test = c(rm_2018 = 1))
)
for (case in bootstrap_cases) {
  set.seed(20260927)
  d <- case$data[complete.cases(case$data[, c(case$y, case$x), with = FALSE])]
  weights_b <- if (is.null(case$w)) rep(1, nrow(d)) else d[[case$w]]
  factors <- list()
  for (v in case$fe) factors[[length(factors) + 1]] <- interaction(d[, v, with = FALSE], drop = TRUE)
  z <- demean(as.matrix(d[, c(case$y, case$x), with = FALSE]), factors, weights = weights_b)
  y_d <- z[, 1]
  x_d <- z[, -1, drop = FALSE]
  x_d <- x_d[, colSums(x_d^2) > 1e-10, drop = FALSE]
  groups <- as.integer(factor(d[[case$cluster]]))
  n_groups <- max(groups)
  r_vec <- setNames(rep(0, ncol(x_d)), colnames(x_d))
  r_vec[names(case$test)] <- case$test
  bread <- solve(crossprod(x_d * sqrt(weights_b)))
  b_hat <- drop(bread %*% crossprod(x_d * weights_b, y_d))
  meat <- crossprod(rowsum(x_d * (weights_b * (y_d - drop(x_d %*% b_hat))), groups))
  scale_b <- n_groups / (n_groups - 1) * (nrow(x_d) - 1) / (nrow(x_d) - ncol(x_d))
  t_hat <- sum(r_vec * b_hat) / sqrt(scale_b * drop(t(r_vec) %*% bread %*% meat %*% bread %*% r_vec))
  null_space <- qr.Q(qr(cbind(r_vec, diag(ncol(x_d)))))[, 2:ncol(x_d), drop = FALSE]
  x_r <- x_d %*% null_space
  fit_r <- drop(x_r %*% solve(crossprod(x_r * sqrt(weights_b)), crossprod(x_r * weights_b, y_d)))
  resid_r <- y_d - fit_r
  bootstrap_residuals <- matrix(0, nrow(x_d), n_groups)
  for (bootstrap_group in seq_len(n_groups)) bootstrap_residuals[groups == bootstrap_group, bootstrap_group] <- resid_r[groups == bootstrap_group]
  bootstrap_residuals <- demean(bootstrap_residuals, factors, weights = weights_b)
  bootstrap_coefficients <- bread %*% crossprod(x_d * weights_b, bootstrap_residuals)
  bootstrap_residuals <- bootstrap_residuals - x_d %*% bootstrap_coefficients
  bootstrap_scores <- rowsum(bootstrap_residuals * drop(weights_b * (x_d %*% bread %*% r_vec)), groups)
  bootstrap_numerator <- drop(t(r_vec) %*% bootstrap_coefficients)
  t_star <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    multipliers <- sample(webb, n_groups, replace = TRUE)
    t_star[i] <- sum(bootstrap_numerator * multipliers) / sqrt(scale_b * sum((bootstrap_scores %*% multipliers)^2))
  }
  rm(bootstrap_residuals, bootstrap_coefficients, bootstrap_scores, bootstrap_numerator)
  bootstrap <- rbind(bootstrap, data.table(case = case$name, estimate = sum(r_vec * b_hat), t = t_hat, p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = n_groups))
}
print(bootstrap)

#figure1
theme_set(theme_bw(base_size = 10, base_family = "Times New Roman") +
            theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
                  axis.title = element_text(size = 9.5), legend.text = element_text(size = 9.5)))
raw_h1 <- rbind(
  tickets[head_female == 0, .(share = mean(rm_female)), by = year][, series := "Female running mates on male-headed tickets"],
  tickets[, .(share = mean(head_female)), by = year][, series := "Women heading tickets"]
)
es_h1 <- as.data.table(coeftable(h1_es), keep.rownames = "term")[grepl("^year::", term)]
es_h1[, year := as.integer(sub("^year::(\\d{4}).*", "\\1", term))]
es_h1 <- rbind(es_h1[, .(year, est = Estimate, se = `Std. Error`)], data.table(year = 2016L, est = 0, se = 0))
raw_g <- governor[, .(share = mean(female)), by = .(year, series = fifelse(rm_slot == 1, "Female running mates on male-headed tickets", "Women heading tickets"))]
es_g <- as.data.table(coeftable(h1g_es), keep.rownames = "term")[grepl("^year::", term)]
es_g[, year := as.integer(sub("^year::(\\d{4}).*", "\\1", term))]
es_g <- rbind(es_g[, .(year, est = Estimate, se = `Std. Error`)], data.table(year = 2014L, est = 0, se = 0))
fig1a <- ggplot(raw_h1, aes(year, 100 * share, linetype = series, shape = series)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2000, 2024, 8)) + scale_y_continuous(limits = c(0, 45), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Share of slots held by women (%)")
fig1b <- ggplot(es_h1, aes(year, 100 * est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = 100 * (est - 1.96 * se), ymax = 100 * (est + 1.96 * se)), width = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2000, 2024, 8)) +
  labs(x = NULL, y = "Running-mate slot minus head slot,\nrelative to the reference (pp)")
fig1c <- ggplot(raw_g, aes(year, 100 * share, linetype = series, shape = series)) +
  geom_vline(xintercept = 2016, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2002, 2022, 4)) + scale_y_continuous(limits = c(0, 45), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Share of slots held by women (%)")
fig1d <- ggplot(es_g, aes(year, 100 * est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 2016, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = 100 * (est - 1.96 * se), ymax = 100 * (est + 1.96 * se)), width = 1.2) + geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2002, 2022, 4)) +
  labs(x = NULL, y = "Running-mate slot minus head slot,\nrelative to the reference (pp)")
figure_1 <- (fig1a + fig1b)/(fig1c + fig1d) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
if (interactive()) print(figure_1)
ggsave("figure_1.png", figure_1, width = 6.3, height = 6.4, dpi = 300, device = agg_png, bg = "white")

#otherreforms
constant <- dcast(unique(tickets[year %in% c(2016, 2020, 2024), .(muni, year, n_tickets)]), muni ~ year, value.var = "n_tickets")
constant <- constant[`2016` == `2020` & `2016` == `2024`, muni]
print(length(constant))
h1_constant <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots[muni %in% constant], cluster = ~muni)
print(data.table(pp = 100 * coef(h1_constant)[["rm_slot:post"]], low = 100 * (coef(h1_constant)[["rm_slot:post"]] - 1.96 * se(h1_constant)[["rm_slot:post"]]),
           high = 100 * (coef(h1_constant)[["rm_slot:post"]] + 1.96 * se(h1_constant)[["rm_slot:post"]])))
male_headed <- tickets[head_female == 0]
male_headed_1624 <- male_headed[year >= 2016 & !is.na(rm_other_party)]
male_headed_1624[, rm_other_post := rm_other_party * post]
h1_origin <- feols(rm_female ~ rm_other_party + rm_other_post | muni^year, male_headed_1624, cluster = ~muni)
print(coeftable(h1_origin))
origin_shares <- male_headed_1624[, .(tickets = .N, female = mean(rm_female)), by = .(year, rm_other_party)]
origin_shares[, weight := tickets / sum(tickets), by = year]
print(origin_shares[order(year, rm_other_party)])
for (target in c(2020L, 2024L)) {
  a <- origin_shares[year == 2016][order(rm_other_party)]
  b <- origin_shares[year == target][order(rm_other_party)]
  print(data.table(target = target, total_change = sum(b$weight * b$female) - sum(a$weight * a$female),
                   between = sum((b$weight - a$weight) * a$female), within = sum(b$weight * (b$female - a$female))))
}
slots[, small_muni := as.integer(council_seats <= 9)]
h1_small <- feols(female ~ rm_slot + rm_slot:post + rm_slot:small_muni + rm_slot:post:small_muni | muni^year, slots[!is.na(small_muni)], cluster = ~muni)
print(coeftable(h1_small))
print(100 * c(larger_municipalities = coef(h1_small)[["rm_slot:post"]], nine_seats = coef(h1_small)[["rm_slot:post"]] + coef(h1_small)[["rm_slot:post:small_muni"]]))

#race
race_slots <- slots[year >= 2016 & race_known == TRUE]
h1_black_women <- feols(black_woman ~ rm_slot + rm_slot:post | muni^year, race_slots, cluster = ~muni)
h1_white_women <- feols(white_woman ~ rm_slot + rm_slot:post | muni^year, race_slots, cluster = ~muni)
h1_black_men <- feols(black_man ~ rm_slot + rm_slot:post | muni^year, race_slots, cluster = ~muni)
print(100 * c(black_women = coef(h1_black_women)[["rm_slot:post"]], white_women = coef(h1_white_women)[["rm_slot:post"]], black_men = coef(h1_black_men)[["rm_slot:post"]]))
print(male_headed[year >= 2016 & !is.na(rm_black), .(black_women = mean(rm_female == 1 & rm_black == 1), white_women = mean(rm_female == 1 & rm_black == 0),
                                              black_men = mean(rm_female == 0 & rm_black == 1), white_men = mean(rm_female == 0 & rm_black == 0)), by = year][order(year)])

#placebo
placebo <- data.table()
for (fake in c(2008L, 2012L, 2016L)) {
  data_fake <- slots[year <= 2016]
  data_fake[, fake_post := as.integer(year >= fake)]
  m <- feols(female ~ rm_slot + rm_slot:fake_post | muni^year, data_fake, cluster = ~muni)
  placebo <- rbind(placebo, data.table(test = paste0("Running-mate vs. head slot, placebo ", fake), unit = "pp", est = coef(m)[["rm_slot:fake_post"]], se = se(m)[["rm_slot:fake_post"]]))
}
print(placebo[, .(test, pp = 100 * est, p = 2 * pnorm(-abs(est / se)))])

#hypothesis2
previous <- tickets[, .(year = year + 4L, muni, id_head, prev_head_female = head_female, prev_rm_female = rm_female, prev_id_rm = id_rm)]
transitions <- merge(tickets, previous, by = c("year", "muni", "id_head"))
transitions <- transitions[head_female == 0 & prev_head_female == 0 & prev_rm_female == 0 & year >= 2008]
transitions[, replaced := as.integer(is.na(id_rm) | is.na(prev_id_rm) | id_rm != prev_id_rm)]
print(transitions[, .(new_running_mate_is_a_woman = mean(rm_female), transitions = .N), by = year][order(year)])
h2_muni <- feols(rm_female ~ i(year, ref = 2016) + incumbent_head | muni, transitions, cluster = ~muni)
print(100 * coef(h2_muni))
print(wald(h2_muni, keep = "^year::(2008|2012)$"))
h2_nonincumbent <- feols(rm_female ~ i(year, ref = 2016) | muni, transitions[incumbent_head == 0], cluster = ~muni)
print(100 * coef(h2_nonincumbent))
h2_contrasts <- data.table()
for (model_name in c("h2_muni", "h2_nonincumbent")) {
  b <- coef(get(model_name))
  v <- vcov(get(model_name))
  for (target in c("year::2020", "year::2024")) {
    k <- c(target, "year::2008", "year::2012")
    l <- c(1, -1 / 3, -1 / 3)
    h2_contrasts <- rbind(h2_contrasts, data.table(model = model_name, transition = target, est = sum(l * b[k]), se = sqrt(drop(t(l) %*% v[k, k] %*% l))))
  }
}
h2_contrasts[, p := 2 * pnorm(-abs(est / se))]
print(h2_contrasts[, .(model, transition, pp = 100 * est, low = 100 * (est - 1.96 * se), high = 100 * (est + 1.96 * se), p)])

#runningmates
rm_tk <- tk[head_female == 0 & !is.na(ID_v) & ANO >= 2008, .(ANO, UF, SG_UE, party_head, ID_v, rm_female, won)]
hh <- unique(history[, .(ID, ANO_h = ANO, office, won_h = won)])
m <- merge(rm_tk[, .(k = .I, ID = ID_v, ANO)], hh, by = "ID", allow.cartesian = TRUE)[ANO_h < ANO & ANO_h >= ANO - 8L]
rm_tk[, `:=`(prior_elected = 0L, prior_ran = 0L)]
rm_tk[unique(m[won_h == 1]$k), prior_elected := 1L]
rm_tk[unique(m$k), prior_ran := 1L]
m <- merge(rm_tk[, .(k = .I, ID = ID_v, ANO)], hh, by = "ID", allow.cartesian = TRUE)[ANO_h == ANO + 4L]
rm_tk[, `:=`(heads_next = NA_integer_, runs_next = NA_integer_, elected_next = NA_integer_)]
rm_tk[ANO <= 2020, `:=`(heads_next = 0L, runs_next = 0L, elected_next = 0L)]
rm_tk[unique(m[office == "PREFEITO"]$k), heads_next := 1L]
rm_tk[unique(m$k), runs_next := 1L]
rm_tk[unique(m[won_h == 1]$k), elected_next := 1L]
rm_tk <- merge(rm_tk, unique(tk[!is.na(ID_c), .(ANO = ANO - 4L, ID_v = ID_c, headed_viable = viable)], by = c("ANO", "ID_v")), by = c("ANO", "ID_v"), all.x = TRUE)
rm_tk[, heads_viable_next := fifelse(ANO <= 2020, as.integer(heads_next == 1L & headed_viable %in% 1L), NA_integer_)]
rm_tk <- merge(rm_tk, unique(tk[, .(ANO = ANO - 4L, SG_UE, n_tickets_next = n_tickets)]), by = c("ANO", "SG_UE"), all.x = TRUE)
running_mates <- rm_tk[, .(year = ANO, state = UF, muni = SG_UE, party_head, id_rm = unname(code[ID_v]), rm_female, won,
                           prior_elected, prior_ran, heads_next, heads_viable_next, runs_next, elected_next, n_tickets_next)]
setorder(running_mates, year, state, muni)

#displacedmen
displaced <- transitions[year >= 2012 & !is.na(prev_id_rm) & !is.na(id_rm) & id_rm != prev_id_rm]
displaced <- merge(displaced, running_mates[, .(year = year + 4L, prev_id_rm = id_rm, runs_next, elected_next, heads_next)], by = c("year", "prev_id_rm"))
displaced[, replaced_by_woman := rm_female]
print(displaced[, .(displaced = .N, ran = mean(runs_next), won = mean(elected_next)), by = .(post, replaced_by_woman)][order(post, replaced_by_woman)])
displaced_runs <- feols(runs_next ~ replaced_by_woman + replaced_by_woman:post | year + state, displaced, cluster = ~muni)
print(data.table(pp = 100 * coef(displaced_runs)[["replaced_by_woman:post"]], low = 100 * (coef(displaced_runs)[["replaced_by_woman:post"]] - 1.96 * se(displaced_runs)[["replaced_by_woman:post"]]),
           high = 100 * (coef(displaced_runs)[["replaced_by_woman:post"]] + 1.96 * se(displaced_runs)[["replaced_by_woman:post"]])))

#hypothesis3
finance <- tickets[year >= 2012 & has_receipts == 1]
finance[, HM := as.integer(ticket_type == "HM")]
finance[, FH := as.integer(head_female == 1)]
finance_male <- finance[head_female == 0]
controls <- "incumbent_head + incumbent_party + experienced_head + coalition_size"
print(finance[, .(median_party_funds = median(party_funds)), by = .(year, type = fifelse(FH == 1, "FH", ticket_type))][order(year, type)])
h3_ppml <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(data.table(percent = 100 * (exp(coef(h3_ppml)[["HM:post"]]) - 1), low = 100 * (exp(coef(h3_ppml)[["HM:post"]] - 1.96 * se(h3_ppml)[["HM:post"]]) - 1),
           high = 100 * (exp(coef(h3_ppml)[["HM:post"]] + 1.96 * se(h3_ppml)[["HM:post"]]) - 1), p = pvalue(h3_ppml)[["HM:post"]]))
h3_ppml_es <- fepois(as.formula(paste("party_funds ~ HM + i(year, HM, ref = 2016) +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(data.table(term = names(coef(h3_ppml_es)), percent = 100 * (exp(coef(h3_ppml_es)) - 1), p = pvalue(h3_ppml_es)))
set.seed(20260928)
score <- data.table()
for (sample_name in c("2012-2024", "2016-2024")) {
  d <- finance_male[year >= fifelse(sample_name == "2012-2024", 2012, 2016)]
  d <- d[complete.cases(d[, .(party_funds, HM, incumbent_head, incumbent_party, experienced_head, coalition_size)])]
  d[, HM_post := HM * post]
  restricted <- fepois(as.formula(paste("party_funds ~ HM +", controls, "| muni^year + party_head^year")), d)
  d <- d[obs(restricted)]
  d[, mu := fitted(restricted)]
  d[, x_tilde := resid(feols(as.formula(paste("HM_post ~ HM +", controls, "| muni^year + party_head^year")), d, weights = ~mu))]
  for (cl in c("party_head", "muni")) {
    s <- rowsum(d$x_tilde * (d$party_funds - d$mu), as.integer(factor(d[[cl]])))[, 1]
    t_hat <- sum(s) / sqrt(sum(s^2))
    t_star <- numeric(n_bootstrap)
    for (i in seq_len(n_bootstrap)) {
      w <- sample(webb, length(s), replace = TRUE)
      t_star[i] <- sum(w * s) / sqrt(sum((w * s)^2))
    }
    score <- rbind(score, data.table(sample = sample_name, cluster = cl, t = t_hat, p_analytic = 2 * pnorm(-abs(t_hat)), p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = length(s)))
  }
}
print(score)
finance_male[, inkind := party_funds - party_funds_cash]
print(finance_male[, .(inkind_share_of_party_funds = sum(inkind) / sum(party_funds)), by = .(year, HM)][order(year, HM)])
h3_ppml_2016 <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male[year >= 2016], cluster = ~muni)
print(data.table(percent = 100 * (exp(coef(h3_ppml_2016)[["HM:post"]]) - 1), p = pvalue(h3_ppml_2016)[["HM:post"]]))

#note3
samples <- data.table()
m <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male[runoff_muni == 0], cluster = ~muni)
samples <- rbind(samples, data.table(hypothesis = "H3", sample = "Excluding municipalities with runoffs", est = coef(m)[["HM:post"]], se = se(m)[["HM:post"]]))
m <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male[capital == 0], cluster = ~muni)
samples <- rbind(samples, data.table(hypothesis = "H3", sample = "Excluding state capitals", est = coef(m)[["HM:post"]], se = se(m)[["HM:post"]]))
for (rg in c("North", "Northeast", "Center-West", "Southeast", "South")) {
  m <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male[region == rg], cluster = ~muni)
  samples <- rbind(samples, data.table(hypothesis = "H3", sample = paste("Region:", rg), est = coef(m)[["HM:post"]], se = se(m)[["HM:post"]]))
}
print(samples[, .(sample, percent = 100 * (exp(est) - 1), p = 2 * pnorm(-abs(est / se)))])
loo_parties <- data.table()
for (pp in finance_male[, .N, by = party_head][N >= 500, party_head]) {
  m <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male[party_head != pp], cluster = ~muni)
  loo_parties <- rbind(loo_parties, data.table(excluded = pp, est = coef(m)[["HM:post"]], se = se(m)[["HM:post"]]))
}
print(loo_parties[order(est), .(excluded, percent = 100 * (exp(est) - 1), p = 2 * pnorm(-abs(est / se)))])
n_permutations <- 1000
set.seed(20260926)
permutations <- numeric(n_permutations)
permuted <- copy(finance_male)
for (b in 1:n_permutations) {
  permuted[, HM_perm := sample(HM), by = .(muni, year)]
  permutations[b] <- coef(fepois(as.formula(paste("party_funds ~ HM_perm + HM_perm:post +", controls, "| muni^year + party_head^year")),
                                 permuted, notes = FALSE, vcov = "iid"))[["HM_perm:post"]]
}
rm(permuted)
print(mean(abs(permutations) >= abs(coef(h3_ppml)[["HM:post"]])))
honest_keep <- c("year::2012:HM", "year::2020:HM", "year::2024:HM")
honest_original <- constructOriginalCS(betahat = coef(h3_ppml_es)[honest_keep], sigma = vcov(h3_ppml_es)[honest_keep, honest_keep],
                                       numPrePeriods = 1, numPostPeriods = 2, l_vec = c(0.5, 0.5))
honest_rm <- createSensitivityResults_relativeMagnitudes(betahat = coef(h3_ppml_es)[honest_keep], sigma = vcov(h3_ppml_es)[honest_keep, honest_keep],
                                                        numPrePeriods = 1, numPostPeriods = 2, Mbarvec = seq(0.25, 2, 0.25), l_vec = c(0.5, 0.5))
honest_plot <- rbind(data.table(Mbar = 0, lb = as.numeric(honest_original$lb), ub = as.numeric(honest_original$ub)), as.data.table(honest_rm)[, .(Mbar, lb, ub)])
print(honest_plot[, .(Mbar, low_percent = 100 * (exp(lb) - 1), high_percent = 100 * (exp(ub) - 1))])
h3_ppml_cash <- fepois(as.formula(paste("party_funds_cash ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(data.table(percent = 100 * (exp(coef(h3_ppml_cash)[["HM:post"]]) - 1), p = pvalue(h3_ppml_cash)[["HM:post"]]))
h3_cash_es <- fepois(as.formula(paste("party_funds_cash ~ HM + i(year, HM, ref = 2016) +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(data.table(term = names(coef(h3_cash_es)), percent = 100 * (exp(coef(h3_cash_es)) - 1), p = pvalue(h3_cash_es)))

#crossparty
allied <- finance_male[rm_other_party == 1 & year >= 2016]
allied[, receives_rm_party := as.integer(party_funds_rm_party > 0)]
print(allied[, .(tickets = .N, receive_funds_from_running_mate_party = mean(receives_rm_party)), by = .(year, HM)][order(year, HM)])
cross_party_twoway <- feols(as.formula(paste("receives_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni + party_rm)
print(data.table(pp = 100 * coef(cross_party_twoway)[["HM:post"]], low = 100 * (coef(cross_party_twoway)[["HM:post"]] - 1.96 * se(cross_party_twoway)[["HM:post"]]),
           high = 100 * (coef(cross_party_twoway)[["HM:post"]] + 1.96 * se(cross_party_twoway)[["HM:post"]]), p = pvalue(cross_party_twoway)[["HM:post"]]))
allied[, HM_post := HM * post]
bootstrap_cases <- list(
  list(name = "H3 funds from the running mate's party, clustered by that party", data = allied, y = "receives_rm_party",
       x = c("HM", "HM_post", "incumbent_head", "incumbent_party", "experienced_head", "coalition_size"), fe = list(c("muni", "year"), c("party_head", "year")),
       w = NULL, cluster = "party_rm", test = c(HM_post = 1))
)
for (case in bootstrap_cases) {
  set.seed(20260927)
  d <- case$data[complete.cases(case$data[, c(case$y, case$x), with = FALSE])]
  weights_b <- if (is.null(case$w)) rep(1, nrow(d)) else d[[case$w]]
  factors <- list()
  for (v in case$fe) factors[[length(factors) + 1]] <- interaction(d[, v, with = FALSE], drop = TRUE)
  z <- demean(as.matrix(d[, c(case$y, case$x), with = FALSE]), factors, weights = weights_b)
  y_d <- z[, 1]
  x_d <- z[, -1, drop = FALSE]
  x_d <- x_d[, colSums(x_d^2) > 1e-10, drop = FALSE]
  groups <- as.integer(factor(d[[case$cluster]]))
  n_groups <- max(groups)
  r_vec <- setNames(rep(0, ncol(x_d)), colnames(x_d))
  r_vec[names(case$test)] <- case$test
  bread <- solve(crossprod(x_d * sqrt(weights_b)))
  b_hat <- drop(bread %*% crossprod(x_d * weights_b, y_d))
  meat <- crossprod(rowsum(x_d * (weights_b * (y_d - drop(x_d %*% b_hat))), groups))
  scale_b <- n_groups / (n_groups - 1) * (nrow(x_d) - 1) / (nrow(x_d) - ncol(x_d))
  t_hat <- sum(r_vec * b_hat) / sqrt(scale_b * drop(t(r_vec) %*% bread %*% meat %*% bread %*% r_vec))
  null_space <- qr.Q(qr(cbind(r_vec, diag(ncol(x_d)))))[, 2:ncol(x_d), drop = FALSE]
  x_r <- x_d %*% null_space
  fit_r <- drop(x_r %*% solve(crossprod(x_r * sqrt(weights_b)), crossprod(x_r * weights_b, y_d)))
  resid_r <- y_d - fit_r
  bootstrap_residuals <- matrix(0, nrow(x_d), n_groups)
  for (bootstrap_group in seq_len(n_groups)) bootstrap_residuals[groups == bootstrap_group, bootstrap_group] <- resid_r[groups == bootstrap_group]
  bootstrap_residuals <- demean(bootstrap_residuals, factors, weights = weights_b)
  bootstrap_coefficients <- bread %*% crossprod(x_d * weights_b, bootstrap_residuals)
  bootstrap_residuals <- bootstrap_residuals - x_d %*% bootstrap_coefficients
  bootstrap_scores <- rowsum(bootstrap_residuals * drop(weights_b * (x_d %*% bread %*% r_vec)), groups)
  bootstrap_numerator <- drop(t(r_vec) %*% bootstrap_coefficients)
  t_star <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    multipliers <- sample(webb, n_groups, replace = TRUE)
    t_star[i] <- sum(bootstrap_numerator * multipliers) / sqrt(scale_b * sum((bootstrap_scores %*% multipliers)^2))
  }
  rm(bootstrap_residuals, bootstrap_coefficients, bootstrap_scores, bootstrap_numerator)
  bootstrap <- rbind(bootstrap, data.table(case = case$name, estimate = sum(r_vec * b_hat), t = t_hat, p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = n_groups))
}
print(bootstrap)
stable_munis <- allied[, .(before = any(year == 2016 & HM == 1) & any(year == 2016 & HM == 0), after = any(year >= 2020 & HM == 1) & any(year >= 2020 & HM == 0)), by = muni][before & after, muni]
print(length(stable_munis))
cross_party_stable <- feols(as.formula(paste("receives_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied[muni %in% stable_munis], cluster = ~muni + party_rm)
cross_party_pair <- feols(as.formula(paste("receives_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year + party_head^party_rm")), allied, cluster = ~muni + party_rm)
allied[, receives_rm_party_tax_id := as.integer(party_funds_rm_party - party_funds_rm_party_name > 0)]
cross_party_tax_id <- feols(as.formula(paste("receives_rm_party_tax_id ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni + party_rm)
print(data.table(model = c("stable municipalities", "pair of parties", "donors identified by tax number"),
           pp = 100 * c(coef(cross_party_stable)[["HM:post"]], coef(cross_party_pair)[["HM:post"]], coef(cross_party_tax_id)[["HM:post"]]),
           p = c(pvalue(cross_party_stable)[["HM:post"]], pvalue(cross_party_pair)[["HM:post"]], pvalue(cross_party_tax_id)[["HM:post"]])))
counterfactual <- dcast(allied[, .(share = mean(receives_rm_party)), by = .(year, HM)], year ~ HM, value.var = "share")
setnames(counterfactual, c("0", "1"), c("all_male", "mixed"))
counterfactual[, ratio := mixed / all_male]
counterfactual[, proportional := mixed[year == 2016] * all_male / all_male[year == 2016]]
counterfactual[, additive := all_male + mixed[year == 2016] - all_male[year == 2016]]
print(counterfactual[, `:=`(excess_proportional = mixed - proportional, excess_additive = mixed - additive)][])
cross_party_poisson <- fepois(as.formula(paste("receives_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni + party_rm)
print(data.table(ratio = exp(coef(cross_party_poisson)[["HM:post"]]), p = pvalue(cross_party_poisson)[["HM:post"]]))

#figure2
es_h3 <- as.data.table(coeftable(h3_ppml_es), keep.rownames = "term")[grepl("^year::", term)]
es_h3[, year := as.integer(sub("^year::(\\d{4}).*", "\\1", term))]
es_h3 <- rbind(es_h3[, .(year, est = Estimate, se = `Std. Error`)], data.table(year = 2016L, est = 0, se = 0))
cross_raw <- allied[, .(share = mean(receives_rm_party)), by = .(year, type = fifelse(HM == 1, "Mixed tickets", "All-male tickets"))]
fig2a <- ggplot(es_h3, aes(year, 100 * (exp(est) - 1))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = 100 * (exp(est - 1.96 * se) - 1), ymax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  labs(x = NULL, y = "Party funds, mixed vs. all-male tickets,\nrelative to 2016 (%)")
fig2b <- ggplot(cross_raw, aes(year, 100 * share, linetype = type, shape = type)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(21, 16)) + scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = c(2016, 2020, 2024)) + scale_y_continuous(limits = c(0, 60), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Allied tickets receiving funds from\nthe running mate's party (%)")
figure_2 <- fig2a + fig2b + plot_layout(widths = c(1, 1))
if (interactive()) print(figure_2)
ggsave("figure_2.png", figure_2, width = 6.3, height = 3.4, dpi = 300, device = agg_png, bg = "white")

#saopaulo
print(tickets[year == 2024 & muni == "71072", .(party_head, party_rm, head_female, rm_female, fefc_from_head_party_million = fefc_own / 1e6, fefc_from_running_mate_party_million = fefc_rm_party / 1e6)])

#transfers
finance_male[, share_to_male_council := to_male_council / receipts_total]
dest_share_male <- feols(as.formula(paste("share_to_male_council ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(data.table(pp = 100 * coef(dest_share_male)[["HM:post"]], p = pvalue(dest_share_male)[["HM:post"]]))

#fefcdonors
fd <- merge(dp[ANO >= 2020L & fefc_party > 0],
            unique(exec[ANO >= 2020L, .(ANO, NR_PARTIDO, fam = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO))))]),
            by.x = c("ANO", "NR_PARTIDO_DOADOR"), by.y = c("ANO", "NR_PARTIDO"))
fd <- merge(fd, unique(tk[ANO >= 2020L, .(ANO, SQ_CANDIDATO = SQ_CANDIDATO_c, ID_c, SG_UE, ticket_type, head_female, viable)]), by = c("ANO", "SQ_CANDIDATO"))
fefc_donor <- fd[, .(year = ANO, donor_party = fam, id_head = unname(code[ID_c]), capital = as.integer(SG_UE %chin% cap_ue),
                     viable, ticket_type = fifelse(head_female == 1, "FH", as.character(ticket_type)), fefc = fefc_party)]
fefc_donor <- fefc_donor[, .(fefc = sum(fefc)), by = .(year, donor_party, id_head, capital, viable, ticket_type)]
setorder(fefc_donor, year, donor_party, -fefc)

#fundingdestinations
tickets[, ln_size := log(.N), by = .(year, party_head)]
composition <- tickets[head_female == 0 & year >= 2008]
viability_model <- feglm(won ~ incumbent_head + incumbent_party + experienced_head + coalition_size + ln_size + log(n_tickets),
                         tickets[head_female == 0 & year %in% c(2008, 2012, 2016)], family = binomial)
composition[, predicted_win := predict(viability_model, newdata = composition)]
composition[, viable_predicted := as.integer(predicted_win >= quantile(predicted_win, 0.75, na.rm = TRUE)), by = year]
finance_male <- merge(finance_male, composition[, .(ticket_id, viable_predicted)], by = "ticket_id", all.x = TRUE)
mixed_post <- finance_male[post == 1 & HM == 1]
print(mixed_post[, .(tickets = .N, funds = sum(party_funds)), by = viable][, `:=`(share_tickets = tickets / sum(tickets), share_funds = funds / sum(funds))][])
print(mixed_post[, .(tickets = .N, funds = sum(party_funds)), by = viable_predicted][, `:=`(share_tickets = tickets / sum(tickets), share_funds = funds / sum(funds))][])
print(finance_male[post == 1, .(won = mean(won), share_of_funds_to_tickets_that_lost = sum(party_funds[won == 0]) / sum(party_funds)), by = HM])
print(mixed_post[, .(tickets = .N, funds = sum(party_funds)), by = .(large = runoff_muni == 1 | capital == 1)][, `:=`(share_tickets = tickets / sum(tickets), share_funds = funds / sum(funds))][])
print(fefc_donor[ticket_type == "HM", .(fefc = sum(fefc)), by = .(year, viable)][, share := fefc / sum(fefc), by = year][order(year, viable)])
print(mixed_post[, .(party_funds_to_nonviable_tickets_that_lost = sum(party_funds[viable == 0 & won == 0]) / sum(party_funds),
               fefc_to_nonviable_tickets_that_lost = sum(fefc_from_party[viable == 0 & won == 0]) / sum(fefc_from_party))])
print(mixed_post[, .(party_funds_to_nonviable_tickets_that_lost = sum(party_funds[viable == 0 & won == 0]) / sum(party_funds),
               fefc_to_nonviable_tickets_that_lost = sum(fefc_from_party[viable == 0 & won == 0]) / sum(fefc_from_party)), by = year])

print(finance_male[post == 1, .(
  nonviable_tickets = mean(viable == 0),
  funds_to_nonviable = sum(party_funds[viable == 0]) / sum(party_funds),
  predicted_nonviable_tickets = mean(viable_predicted == 0),
  funds_to_predicted_nonviable = sum(party_funds[viable_predicted == 0]) / sum(party_funds),
  losing_tickets = mean(won == 0),
  funds_to_losing_tickets = sum(party_funds[won == 0]) / sum(party_funds)),
  by = .(ticket_type)])

#hypothesis4
h4a_comp <- feols(rm_female ~ viable + viable:post + experienced_head + coalition_size | muni^year + party_head^year, composition, cluster = ~muni)
print(data.table(pp = 100 * coef(h4a_comp)[["viable:post"]], low = 100 * (coef(h4a_comp)[["viable:post"]] - 1.96 * se(h4a_comp)[["viable:post"]]),
           high = 100 * (coef(h4a_comp)[["viable:post"]] + 1.96 * se(h4a_comp)[["viable:post"]]), p = pvalue(h4a_comp)[["viable:post"]]))
h4a_comp_pred <- feols(rm_female ~ viable_predicted + viable_predicted:post + experienced_head + coalition_size | muni^year + party_head^year, composition, cluster = ~muni)
print(100 * coef(h4a_comp_pred)[["viable_predicted:post"]])
print(composition[year %in% c(2016, 2024), .(female_running_mates = mean(rm_female)), by = .(year, viable)][order(viable, year)])
h4a_money <- fepois(party_funds ~ HM * post * viable + experienced_head + coalition_size | muni^year + party_head^year, finance_male, cluster = ~muni)
print(data.table(term = c("post:viable", "HM:post:viable"), percent = 100 * (exp(coef(h4a_money)[c("post:viable", "HM:post:viable")]) - 1),
           low = 100 * (exp(coef(h4a_money)[c("post:viable", "HM:post:viable")] - 1.96 * se(h4a_money)[c("post:viable", "HM:post:viable")]) - 1),
           high = 100 * (exp(coef(h4a_money)[c("post:viable", "HM:post:viable")] + 1.96 * se(h4a_money)[c("post:viable", "HM:post:viable")]) - 1),
           p = pvalue(h4a_money)[c("post:viable", "HM:post:viable")]))
composition[, endow_statutory := log(statutory_2020)]
h4b_comp <- feols(as.formula(paste("rm_female ~ endow_statutory:post +", controls, "| muni^year + party_head")), composition[!is.na(endow_statutory)], cluster = ~muni + party_head)
print(data.table(pp_for_twice_the_endowment = 100 * log(2) * coef(h4b_comp)[["endow_statutory:post"]], p = pvalue(h4b_comp)[["endow_statutory:post"]]))
h4b_data <- composition[!is.na(endow_statutory)]
h4b_data[, endow_statutory_post := endow_statutory * post]
bootstrap_cases <- list(
  list(name = "H4b female running mate, per log point of statutory endowment", data = h4b_data, y = "rm_female",
       x = c("endow_statutory_post", "incumbent_head", "incumbent_party", "experienced_head", "coalition_size"), fe = list(c("muni", "year"), "party_head"),
       w = NULL, cluster = "party_head", test = c(endow_statutory_post = 1))
)
for (case in bootstrap_cases) {
  set.seed(20260927)
  d <- case$data[complete.cases(case$data[, c(case$y, case$x), with = FALSE])]
  weights_b <- if (is.null(case$w)) rep(1, nrow(d)) else d[[case$w]]
  factors <- list()
  for (v in case$fe) factors[[length(factors) + 1]] <- interaction(d[, v, with = FALSE], drop = TRUE)
  z <- demean(as.matrix(d[, c(case$y, case$x), with = FALSE]), factors, weights = weights_b)
  y_d <- z[, 1]
  x_d <- z[, -1, drop = FALSE]
  x_d <- x_d[, colSums(x_d^2) > 1e-10, drop = FALSE]
  groups <- as.integer(factor(d[[case$cluster]]))
  n_groups <- max(groups)
  r_vec <- setNames(rep(0, ncol(x_d)), colnames(x_d))
  r_vec[names(case$test)] <- case$test
  bread <- solve(crossprod(x_d * sqrt(weights_b)))
  b_hat <- drop(bread %*% crossprod(x_d * weights_b, y_d))
  meat <- crossprod(rowsum(x_d * (weights_b * (y_d - drop(x_d %*% b_hat))), groups))
  scale_b <- n_groups / (n_groups - 1) * (nrow(x_d) - 1) / (nrow(x_d) - ncol(x_d))
  t_hat <- sum(r_vec * b_hat) / sqrt(scale_b * drop(t(r_vec) %*% bread %*% meat %*% bread %*% r_vec))
  null_space <- qr.Q(qr(cbind(r_vec, diag(ncol(x_d)))))[, 2:ncol(x_d), drop = FALSE]
  x_r <- x_d %*% null_space
  fit_r <- drop(x_r %*% solve(crossprod(x_r * sqrt(weights_b)), crossprod(x_r * weights_b, y_d)))
  resid_r <- y_d - fit_r
  bootstrap_residuals <- matrix(0, nrow(x_d), n_groups)
  for (bootstrap_group in seq_len(n_groups)) bootstrap_residuals[groups == bootstrap_group, bootstrap_group] <- resid_r[groups == bootstrap_group]
  bootstrap_residuals <- demean(bootstrap_residuals, factors, weights = weights_b)
  bootstrap_coefficients <- bread %*% crossprod(x_d * weights_b, bootstrap_residuals)
  bootstrap_residuals <- bootstrap_residuals - x_d %*% bootstrap_coefficients
  bootstrap_scores <- rowsum(bootstrap_residuals * drop(weights_b * (x_d %*% bread %*% r_vec)), groups)
  bootstrap_numerator <- drop(t(r_vec) %*% bootstrap_coefficients)
  t_star <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    multipliers <- sample(webb, n_groups, replace = TRUE)
    t_star[i] <- sum(bootstrap_numerator * multipliers) / sqrt(scale_b * sum((bootstrap_scores %*% multipliers)^2))
  }
  rm(bootstrap_residuals, bootstrap_coefficients, bootstrap_scores, bootstrap_numerator)
  bootstrap <- rbind(bootstrap, data.table(case = case$name, estimate = sum(r_vec * b_hat), t = t_hat, p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = n_groups))
}
print(bootstrap)
h4b_comp_es <- feols(as.formula(paste("rm_female ~ i(year, endow_statutory, ref = 2016) +", controls, "| muni^year + party_head")), composition[!is.na(endow_statutory)], cluster = ~muni + party_head)
print(wald(h4b_comp_es, keep = "^year::(2008|2012):endow_statutory$"))
finance_male[, endow_statutory := log(statutory_2020)]
h4b_money <- fepois(as.formula(paste("party_funds ~ HM + HM:post + HM:endow_statutory + HM:post:endow_statutory +", controls, "| muni^year + party_head^year")),
                    finance_male[!is.na(endow_statutory)], cluster = ~muni + party_head)
print(pvalue(h4b_money)[["HM:post:endow_statutory"]])

#note4
h4a_comp_es <- feols(rm_female ~ viable + i(year, viable, ref = 2016) + experienced_head + coalition_size | muni^year + party_head^year, composition, cluster = ~muni)
print(100 * coef(h4a_comp_es))
print(wald(h4a_comp_es, keep = "^year::(2008|2012):viable$"))
print(100 * c(gap_2020_minus_gap_2008_2012 = coef(h4a_comp_es)[["year::2020:viable"]] - mean(coef(h4a_comp_es)[c("year::2008:viable", "year::2012:viable")]),
        gap_2024_minus_gap_2008_2012 = coef(h4a_comp_es)[["year::2024:viable"]] - mean(coef(h4a_comp_es)[c("year::2008:viable", "year::2012:viable")])))

#figure3
h4a_money_pred <- fepois(party_funds ~ HM * post * viable_predicted + experienced_head + coalition_size | muni^year + party_head^year, finance_male, cluster = ~muni)
fig3_comp <- data.table(
  factor = c("Viable ticket\n(incumbent or incumbent party)", "Top quartile of\npredicted viability", "Party with twice the\nstatutory FEFC endowment"),
  est = c(100 * coef(h4a_comp)[["viable:post"]], 100 * coef(h4a_comp_pred)[["viable_predicted:post"]], 100 * log(2) * coef(h4b_comp)[["endow_statutory:post"]]),
  se = c(100 * se(h4a_comp)[["viable:post"]], 100 * se(h4a_comp_pred)[["viable_predicted:post"]], 100 * log(2) * se(h4b_comp)[["endow_statutory:post"]])
)
fig3_money <- data.table(
  factor = fig3_comp$factor,
  est = c(coef(h4a_money)[["HM:post:viable"]], coef(h4a_money_pred)[["HM:post:viable_predicted"]], log(2) * coef(h4b_money)[["HM:post:endow_statutory"]]),
  se = c(se(h4a_money)[["HM:post:viable"]], se(h4a_money_pred)[["HM:post:viable_predicted"]], log(2) * se(h4b_money)[["HM:post:endow_statutory"]])
)
fig3_comp[, factor := factor(factor, levels = rev(factor))]
fig3_money[, factor := factor(factor, levels = rev(factor))]
fig3a <- ggplot(fig3_comp, aes(est, factor)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), width = 0.15, orientation = "y") + geom_point(size = 2.2) +
  labs(x = "Post-earmark change in the probability of\na female running mate (percentage points)", y = NULL)
fig3b <- ggplot(fig3_money, aes(100 * (exp(est) - 1), factor)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.15, orientation = "y") +
  geom_point(size = 2.2) + labs(x = "Post-earmark change in the\nmixed-ticket premium (%)", y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
figure_3 <- fig3a + fig3b
if (interactive()) print(figure_3)
ggsave("figure_3.png", figure_3, width = 6.3, height = 2.9, dpi = 300, device = agg_png, bg = "white")

#rival
composition[, left := as.integer(party_head %in% c("PT", "PSOL", "PC DO B", "PSB", "PDT", "REDE", "PV", "PCB", "PSTU", "PCO", "UP", "PPL"))]
rival_pre <- feols(rm_female ~ ln_size + left + viable | muni^year, composition[post == 0], cluster = ~muni + party_head)
rival_post <- feols(rm_female ~ ln_size + left + viable + ln_size:post + left:post + viable:post | muni^year, composition, cluster = ~muni + party_head)
size_iqr <- diff(quantile(composition[post == 0]$ln_size, c(0.25, 0.75)))[[1]]
print(100 * c(large_minus_small_party = size_iqr * coef(rival_pre)[["ln_size"]], left_leaning_party = coef(rival_pre)[["left"]], viable_ticket = coef(rival_pre)[["viable"]]))
rival_data <- composition[!is.na(viable)]
rival_data[, `:=`(ln_size_post = ln_size * post, left_post = left * post, viable_post = viable * post)]
bootstrap_cases <- list(
  list(name = "Rival hypothesis, log party size x post", data = rival_data, y = "rm_female", x = c("ln_size", "left", "viable", "ln_size_post", "left_post", "viable_post"),
       fe = list(c("muni", "year")), w = NULL, cluster = "party_head", test = c(ln_size_post = 1)),
  list(name = "Rival hypothesis, left-leaning party x post", data = rival_data, y = "rm_female", x = c("ln_size", "left", "viable", "ln_size_post", "left_post", "viable_post"),
       fe = list(c("muni", "year")), w = NULL, cluster = "party_head", test = c(left_post = 1)),
  list(name = "Rival hypothesis, viable ticket x post", data = rival_data, y = "rm_female", x = c("ln_size", "left", "viable", "ln_size_post", "left_post", "viable_post"),
       fe = list(c("muni", "year")), w = NULL, cluster = "party_head", test = c(viable_post = 1))
)
for (case in bootstrap_cases) {
  set.seed(20260927)
  d <- case$data[complete.cases(case$data[, c(case$y, case$x), with = FALSE])]
  weights_b <- if (is.null(case$w)) rep(1, nrow(d)) else d[[case$w]]
  factors <- list()
  for (v in case$fe) factors[[length(factors) + 1]] <- interaction(d[, v, with = FALSE], drop = TRUE)
  z <- demean(as.matrix(d[, c(case$y, case$x), with = FALSE]), factors, weights = weights_b)
  y_d <- z[, 1]
  x_d <- z[, -1, drop = FALSE]
  x_d <- x_d[, colSums(x_d^2) > 1e-10, drop = FALSE]
  groups <- as.integer(factor(d[[case$cluster]]))
  n_groups <- max(groups)
  r_vec <- setNames(rep(0, ncol(x_d)), colnames(x_d))
  r_vec[names(case$test)] <- case$test
  bread <- solve(crossprod(x_d * sqrt(weights_b)))
  b_hat <- drop(bread %*% crossprod(x_d * weights_b, y_d))
  meat <- crossprod(rowsum(x_d * (weights_b * (y_d - drop(x_d %*% b_hat))), groups))
  scale_b <- n_groups / (n_groups - 1) * (nrow(x_d) - 1) / (nrow(x_d) - ncol(x_d))
  t_hat <- sum(r_vec * b_hat) / sqrt(scale_b * drop(t(r_vec) %*% bread %*% meat %*% bread %*% r_vec))
  null_space <- qr.Q(qr(cbind(r_vec, diag(ncol(x_d)))))[, 2:ncol(x_d), drop = FALSE]
  x_r <- x_d %*% null_space
  fit_r <- drop(x_r %*% solve(crossprod(x_r * sqrt(weights_b)), crossprod(x_r * weights_b, y_d)))
  resid_r <- y_d - fit_r
  bootstrap_residuals <- matrix(0, nrow(x_d), n_groups)
  for (bootstrap_group in seq_len(n_groups)) bootstrap_residuals[groups == bootstrap_group, bootstrap_group] <- resid_r[groups == bootstrap_group]
  bootstrap_residuals <- demean(bootstrap_residuals, factors, weights = weights_b)
  bootstrap_coefficients <- bread %*% crossprod(x_d * weights_b, bootstrap_residuals)
  bootstrap_residuals <- bootstrap_residuals - x_d %*% bootstrap_coefficients
  bootstrap_scores <- rowsum(bootstrap_residuals * drop(weights_b * (x_d %*% bread %*% r_vec)), groups)
  bootstrap_numerator <- drop(t(r_vec) %*% bootstrap_coefficients)
  t_star <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    multipliers <- sample(webb, n_groups, replace = TRUE)
    t_star[i] <- sum(bootstrap_numerator * multipliers) / sqrt(scale_b * sum((bootstrap_scores %*% multipliers)^2))
  }
  rm(bootstrap_residuals, bootstrap_coefficients, bootstrap_scores, bootstrap_numerator)
  bootstrap <- rbind(bootstrap, data.table(case = case$name, estimate = sum(r_vec * b_hat), t = t_hat, p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = n_groups))
}
print(bootstrap)

#hypothesis5
h5_ppml <- fepois(as.formula(paste("party_funds ~ HM + FH + HM:post + FH:post +", controls, "| muni^year + party_head^year")), finance, cluster = ~muni)
print(data.table(percent = 100 * (exp(coef(h5_ppml)[["FH:post"]]) - 1), low = 100 * (exp(coef(h5_ppml)[["FH:post"]] - 1.96 * se(h5_ppml)[["FH:post"]]) - 1),
           high = 100 * (exp(coef(h5_ppml)[["FH:post"]] + 1.96 * se(h5_ppml)[["FH:post"]]) - 1), p = pvalue(h5_ppml)[["FH:post"]]))
print(finance[FH == 1 & post == 1, .(median_party_funds = median(party_funds)), by = year])
h5_ppml_es <- fepois(as.formula(paste("party_funds ~ HM + FH + HM:post + i(year, FH, ref = 2016) +", controls, "| muni^year + party_head^year")), finance, cluster = ~muni)
print(data.table(term = names(coef(h5_ppml_es)), percent = 100 * (exp(coef(h5_ppml_es)) - 1), p = pvalue(h5_ppml_es)))
h5_ppml_2016 <- fepois(as.formula(paste("party_funds ~ HM + FH + HM:post + FH:post +", controls, "| muni^year + party_head^year")), finance[year >= 2016], cluster = ~muni)
print(data.table(percent = 100 * (exp(coef(h5_ppml_2016)[["FH:post"]]) - 1), p = pvalue(h5_ppml_2016)[["FH:post"]]))
h5_ppml_incumbent <- fepois(party_funds ~ FH * post * incumbent_head + HM + HM:post + incumbent_party + experienced_head + coalition_size | muni^year + party_head^year, finance, cluster = ~muni)
h5_b <- coef(h5_ppml_incumbent)
h5_v <- vcov(h5_ppml_incumbent)
gains_h5 <- data.table(
  group = factor(c("Women who were\nnot incumbents", "Women who were\nincumbent mayors"), levels = c("Women who were\nincumbent mayors", "Women who were\nnot incumbents")),
  est = c(h5_b[["FH:post"]], h5_b[["FH:post"]] + h5_b[["FH:post:incumbent_head"]]),
  se = c(sqrt(h5_v["FH:post", "FH:post"]), sqrt(h5_v["FH:post", "FH:post"] + h5_v["FH:post:incumbent_head", "FH:post:incumbent_head"] + 2 * h5_v["FH:post", "FH:post:incumbent_head"]))
)
print(gains_h5[, .(group, percent = 100 * (exp(est) - 1), low = 100 * (exp(est - 1.96 * se) - 1), high = 100 * (exp(est + 1.96 * se) - 1))])
print(pvalue(h5_ppml_incumbent)[["FH:post:incumbent_head"]])

#partycells
verp <- council[ANO >= 2012L & DS_GENERO %chin% c("MASCULINO", "FEMININO"), .(SQ_CANDIDATO, SG_UF, SG_PARTIDO, DS_GENERO, ANO)]
verp <- merge(verp, rec[CARGO == "Vereador", .(ANO = year, SQ_CANDIDATO, party = party_cash)], by = c("ANO", "SQ_CANDIDATO"), all.x = TRUE)
verp[is.na(party), party := 0]
verp[, fam := fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO)))]
pc <- dcast(rbind(verp[, .(v = sum(party)), by = .(ANO, UF = SG_UF, fam, g = fifelse(DS_GENERO == "FEMININO", "funds_female_council", "funds_male_council"))],
                  tk[ANO >= 2012 & !is.na(total) & total > 0, .(v = sum(party_cash)),
                     by = .(ANO, UF, fam = party_head, g = fcase(head_female == 1, "funds_fh", ticket_type == "HM", "funds_hm", default = "funds_hh"))]),
            ANO + UF + fam ~ g, value.var = "v", fun.aggregate = sum, fill = 0)
pc <- merge(pc, verp[, .(share_female_council_cands = mean(DS_GENERO == "FEMININO")), by = .(ANO, UF = SG_UF, fam)], by = c("ANO", "UF", "fam"))
party_cells <- pc[, .(year = ANO, state = UF, party = fam, funds_fh, funds_female_council, funds_hm, funds_hh, funds_male_council, share_female_council_cands)]
setorder(party_cells, year, party, state)

#fefccells
ver <- council[ANO %in% c(2020L, 2024L) & DS_GENERO %chin% c("MASCULINO", "FEMININO"),
               .(ANO, SQ_CANDIDATO, SG_UF, fam = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO))), DS_GENERO)]
ver <- merge(ver, rec[CARGO == "Vereador" & year >= 2020, .(ANO = year, SQ_CANDIDATO, fefc_party)], by = c("ANO", "SQ_CANDIDATO"), all.x = TRUE)
ver[is.na(fefc_party), fefc_party := 0]
cells <- dcast(rbind(ver[, .(fefc = sum(fefc_party)), by = .(ANO, UF = SG_UF, fam, group = fifelse(DS_GENERO == "FEMININO", "fefc_female_council", "fefc_male_council"))],
                     tk[ANO >= 2020 & !is.na(total) & total > 0, .(fefc = sum(fefc_party)),
                        by = .(ANO, UF, fam = party_head, group = fcase(ticket_type == "HM", "fefc_hm", ticket_type == "HH", "fefc_hh", default = "fefc_fh"))]),
               ANO + UF + fam ~ group, value.var = "fefc", fun.aggregate = sum, fill = 0)
cells <- merge(cells, ver[, .(share_female_council_cands = mean(DS_GENERO == "FEMININO")), by = .(ANO, UF = SG_UF, fam)], by = c("ANO", "UF", "fam"))
fefc_cells <- cells[, .(year = ANO, state = UF, party = fam, fefc_fh, fefc_female_council, fefc_hh, fefc_hm, fefc_male_council, share_female_council_cands)]
setorder(fefc_cells, year, party, state)
cand <- unique(rbind(
  exec[ANO >= 2020L & DS_GENERO %chin% c("MASCULINO", "FEMININO"),
    .(ANO, SQ_CANDIDATO, fam = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]),
      toupper(trimws(SG_PARTIDO))), female = DS_GENERO == "FEMININO")],
  council[ANO >= 2020L & DS_GENERO %chin% c("MASCULINO", "FEMININO"),
    .(ANO, SQ_CANDIDATO, fam = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]),
      toupper(trimws(SG_PARTIDO))), female = DS_GENERO == "FEMININO")]),
  by = c("ANO", "SQ_CANDIDATO"))
party_floor <- cand[, .(share_female_candidacies = mean(female), n_candidacies = .N), by = .(year = ANO, party = fam)]
setorder(party_floor, year, party)

#hypothesis6
party_cells[, total := funds_fh + funds_female_council + funds_hm + funds_hh + funds_male_council]
party_cells[, women_share := (funds_fh + funds_female_council) / total]
funds_shares <- party_cells[, .(women_own = sum(funds_fh + funds_female_council) / sum(total), mixed = sum(funds_hm) / sum(total),
                                all_male = sum(funds_hh) / sum(total), male_council = sum(funds_male_council) / sum(total)), by = year][order(year)]
print(funds_shares)
by_donor <- dcast(fefc_donor, year + donor_party ~ ticket_type, value.var = "fefc", fun.aggregate = sum, fill = 0)
setnames(by_donor, c("donor_party", "FH", "HH", "HM"), c("party", "fh", "hh", "hm"))
party_numbers <- unique(rbind(
  exec[ANO >= 2020L, .(year = ANO, donor_party_number = NR_PARTIDO,
    party = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO))))],
  council[ANO >= 2020L, .(year = ANO, donor_party_number = NR_PARTIDO,
    party = fcoalesce(unname(family[toupper(trimws(SG_PARTIDO))]), toupper(trimws(SG_PARTIDO))))]))
council_donors <- merge(council_transfers,
  council[ANO >= 2020L & DS_GENERO %chin% c("MASCULINO", "FEMININO"),
    .(year = ANO, SQ_CANDIDATO, female = DS_GENERO == "FEMININO")],
  by = c("year", "SQ_CANDIDATO"))
council_donors <- merge(council_donors, party_numbers,
  by = c("year", "donor_party_number"), all.x = TRUE)
donor_coverage <- council_donors[, .(total = sum(fefc), unidentified = sum(fefc[is.na(party)])), by = year]
donor_coverage[, coverage := 1 - unidentified / total]
print(donor_coverage)
council_donors <- council_donors[!is.na(party)]
council_fefc <- council_donors[, .(female_council = sum(fefc[female]),
  male_council = sum(fefc[!female])), by = .(year, party)]
accounting <- merge(by_donor, council_fefc, by = c("year", "party"), all = TRUE)
for (v in c("fh", "hh", "hm", "female_council", "male_council")) set(accounting, which(is.na(accounting[[v]])), v, 0)
accounting <- merge(accounting, party_floor, by = c("year", "party"))
accounting[, total := fh + hh + hm + female_council + male_council]
accounting <- accounting[total > 0]
accounting[, legal_floor := pmax(0.30, share_female_candidacies)]
accounting[, own_share := (fh + female_council) / total]
accounting[, share_with_mixed := (fh + female_council + hm) / total]
accounting[, status := fcase(own_share >= legal_floor, "own", share_with_mixed >= legal_floor, "only_with_mixed", default = "fails")]
print(accounting[, .(parties = .N, fefc = sum(total)), by = .(year, status)][, fefc_share := fefc / sum(fefc), by = year][order(year, status)])
exposure <- tickets[year == 2016 & head_female == 0, .(exposure = mean(ticket_type == "HM"), tickets_2016 = .N), by = .(state, party = party_head)]
h6_cells <- merge(party_cells[total > 0], exposure, by = c("state", "party"))[tickets_2016 >= 3]
h6_cells[, post := as.integer(year >= 2020)]
h6_cells[, exposure_post := exposure * post]
h6_dd <- feols(women_share ~ exposure_post + share_female_council_cands | state^party + party^year + state^year, h6_cells, weights = ~total, cluster = ~party)
print(coeftable(h6_dd))
h6_es <- feols(women_share ~ i(year, exposure, ref = 2016) + share_female_council_cands | state^party + party^year + state^year, h6_cells, weights = ~total, cluster = ~party)
print(coeftable(h6_es))
fefc_cells[, total := fefc_fh + fefc_female_council + fefc_hh + fefc_hm + fefc_male_council]
cells <- fefc_cells[total > 0]
cells[, women_share := (fefc_fh + fefc_female_council) / total]
cells[, share_hm := fefc_hm / total]
cells[, share_hh := fefc_hh / total]
h6_cross <- feols(women_share ~ share_hm + share_hh + share_female_council_cands | party^year + state^year, cells, weights = ~total, cluster = ~party)
h6_b <- coef(h6_cross)
h6_v <- vcov(h6_cross)
h6_diff <- data.table(est = h6_b[["share_hm"]] - h6_b[["share_hh"]], se = sqrt(h6_v["share_hm", "share_hm"] + h6_v["share_hh", "share_hh"] - 2 * h6_v["share_hm", "share_hh"]))
h6_diff[, p := 2 * pnorm(-abs(est / se))]
print(h6_diff[, .(points_per_10_points = 10 * est, p)])

#note5
accounting[, status_flat := fcase(own_share >= 0.30, "own", share_with_mixed >= 0.30, "only_with_mixed", default = "fails")]
top_mixed <- fefc_donor[ticket_type == "HM", .(top_hm = max(fefc)), by = .(year, party = donor_party)]
accounting <- merge(accounting, top_mixed, by = c("year", "party"), all.x = TRUE)
accounting[is.na(top_hm), top_hm := 0]
accounting[, status_without_top := fcase((fh + female_council) / (total - top_hm) >= legal_floor, "own",
                                         (fh + female_council + hm - top_hm) / (total - top_hm) >= legal_floor, "only_with_mixed", default = "fails")]
capital_mixed <- fefc_donor[ticket_type == "HM" & capital == 1, .(capital_hm = sum(fefc)), by = .(year, party = donor_party)]
accounting <- merge(accounting, capital_mixed, by = c("year", "party"), all.x = TRUE)
accounting[is.na(capital_hm), capital_hm := 0]
accounting[, status_without_capitals := fcase((fh + female_council) / (total - capital_hm) >= legal_floor, "own",
                                              (fh + female_council + hm - capital_hm) / (total - capital_hm) >= legal_floor, "only_with_mixed", default = "fails")]
print(accounting[, .(parties = .N), by = .(year, status_flat)][order(year, status_flat)])
print(accounting[, .(parties = .N), by = .(year, status_without_top)][order(year, status_without_top)])
print(accounting[, .(parties = .N), by = .(year, status_without_capitals)][order(year, status_without_capitals)])

#figure4
shares_long <- melt(funds_shares, id.vars = "year", variable.name = "group", value.name = "share")
shares_long[, group := factor(group, levels = c("women_own", "mixed", "all_male", "male_council"),
                              labels = c("Women's own candidacies", "Mixed tickets", "All-male tickets", "Male council candidates"))]
fig4a <- ggplot(shares_long, aes(year, 100 * share, linetype = group, shape = group)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 17, 21, 22)) + scale_linetype_manual(values = c("solid", "longdash", "dashed", "dotdash")) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  guides(linetype = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) +
  labs(x = NULL, y = "Share of cash party funds received\nby municipal candidates (%)")
fig4_acc <- accounting[, .N, by = .(year, status)]
fig4_acc[, status := factor(status, levels = c("fails", "only_with_mixed", "own"),
                            labels = c("Would not reach the floor\neven counting mixed tickets", "Would reach the floor only\nif mixed tickets counted", "Would reach the floor through\nwomen's own candidacies"))]
fig4b <- ggplot(fig4_acc, aes(factor(year), N, fill = status)) +
  geom_col(width = 0.55, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = N, colour = status), position = position_stack(vjust = 0.5), size = 3.3, family = "Times New Roman", show.legend = FALSE) +
  scale_fill_manual(values = c("black", "grey55", "white"), drop = FALSE) + scale_colour_manual(values = c("white", "white", "black"), drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) + guides(fill = guide_legend(ncol = 1)) +
  labs(x = NULL, y = "Number of parties") + theme(legend.key.size = unit(10, "pt"))
figure_4 <- fig4a + fig4b + plot_layout(widths = c(1.2, 1))
if (interactive()) print(figure_4)
ggsave("figure_4.png", figure_4, width = 6.3, height = 3.9, dpi = 300, device = agg_png, bg = "white")

#hypothesis7
running_mates[, post := as.integer(year >= 2020)]
h7_prior <- feols(prior_elected ~ rm_female + rm_female:post | muni^year + party_head^year, running_mates, cluster = ~muni)
print(data.table(term = names(coef(h7_prior)), pp = 100 * coef(h7_prior), low = 100 * (coef(h7_prior) - 1.96 * se(h7_prior)), high = 100 * (coef(h7_prior) + 1.96 * se(h7_prior))))
raw_h7 <- running_mates[, .(prior_elected = mean(prior_elected), heads_next = mean(heads_next, na.rm = TRUE), heads_viable_next = mean(heads_viable_next, na.rm = TRUE)), by = .(year, rm_female)]
print(raw_h7[order(rm_female, year)])
h7_heads <- feols(heads_next ~ rm_female + rm_female:post | muni^year + party_head^year, running_mates[year <= 2020], cluster = ~muni)
print(data.table(pp = 100 * coef(h7_heads)[["rm_female:post"]], low = 100 * (coef(h7_heads)[["rm_female:post"]] - 1.96 * se(h7_heads)[["rm_female:post"]]),
           high = 100 * (coef(h7_heads)[["rm_female:post"]] + 1.96 * se(h7_heads)[["rm_female:post"]]), p = pvalue(h7_heads)[["rm_female:post"]]))
h7_heads_viable <- feols(heads_viable_next ~ rm_female + rm_female:post | muni^year + party_head^year, running_mates[year <= 2020], cluster = ~muni)
h7_elected <- feols(elected_next ~ rm_female + rm_female:post | muni^year + party_head^year, running_mates[year <= 2020], cluster = ~muni)
print(data.table(outcome = c("headed a viable ticket", "won municipal office"), pp = 100 * c(coef(h7_heads_viable)[["rm_female:post"]], coef(h7_elected)[["rm_female:post"]]),
           p = c(pvalue(h7_heads_viable)[["rm_female:post"]], pvalue(h7_elected)[["rm_female:post"]])))

#figure5
raw_h7[, rm := factor(rm_female, levels = c(1, 0), labels = c("Female running mates", "Male running mates"))]
fig5a <- ggplot(raw_h7, aes(year, 100 * prior_elected, linetype = rm, shape = rm)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2008, 2024, 8)) + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "Previously elected to\nmunicipal office (%)")
fig5b <- ggplot(raw_h7[year <= 2020], aes(year, 100 * heads_next, linetype = rm, shape = rm)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2008, 2020, 4), limits = c(2007, 2021)) + scale_y_continuous(limits = c(0, 12), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Headed a ticket in the\nfollowing election (%)")
fig5c <- ggplot(raw_h7[year <= 2020], aes(year, 100 * heads_viable_next, linetype = rm, shape = rm)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2008, 2020, 4), limits = c(2007, 2021)) + scale_y_continuous(limits = c(0, 12), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Headed a viable ticket in the\nfollowing election (%)")
figure_5 <- fig5a + fig5b + fig5c + plot_layout(guides = "collect") & theme(legend.position = "bottom")
if (interactive()) print(figure_5)
ggsave("figure_5.png", figure_5, width = 6.3, height = 3.3, dpi = 300, device = agg_png, bg = "white")

#sectionS1
print(exec[, .(valid_voter_registration_number = mean(!is.na(NR_TITULO_ELEITORAL_CANDIDATO) & nchar(NR_TITULO_ELEITORAL_CANDIDATO) >= 8 &
                                                 !grepl("^#|^-", NR_TITULO_ELEITORAL_CANDIDATO))), by = ANO])
print(dp[ANO == 2016, .(party_funds_of_donors_identified_by_tax_number = sum(party[method == "cnpj"]) / sum(party))])
print(tickets[year == 2016 & head_female == 0, .(own = sum(party_funds_own), running_mate_party = sum(party_funds_rm_party), other = sum(party_funds_other))][
  , .(head_party = own / (own + running_mate_party + other), running_mate_party = running_mate_party / (own + running_mate_party + other), other = other / (own + running_mate_party + other))])
print(allied[year == 2016, .(from_running_mate_party_million = sum(party_funds_rm_party) / 1e6, donors_identified_by_name_million = sum(party_funds_rm_party_name) / 1e6)])
print(rec[CARGO == "Vereador", .(inkind_from_parties_billion = sum(party - party_cash) / 1e9, cash_from_parties_billion = sum(party_cash) / 1e9), by = year])

#tableS2
print(tickets[head_female == 0, .(running_mate_from_head_party = mean(party_rm == party_head, na.rm = TRUE)), by = year])
sample_sizes <- tickets[, .(municipalities = uniqueN(muni), tickets = .N, male_headed = sum(head_female == 0),
                            mixed = sum(ticket_type == "HM"), all_male = sum(ticket_type == "HH"), female_headed = sum(head_female == 1),
                            with_receipts = sum(has_receipts == 1 & year >= 2012)), by = year]
sample_sizes <- merge(sample_sizes, transitions[, .(transitions = .N), by = year], by = "year", all.x = TRUE)
sample_sizes <- merge(sample_sizes, running_mates[, .(running_mates = .N), by = year], by = "year", all.x = TRUE)
sample_sizes <- merge(sample_sizes, party_cells[total > 0, .(party_state_cells = .N), by = year], by = "year", all.x = TRUE)
print(sample_sizes)
print(dcast(general_slots[office != "president", .N, by = .(year, office, slot = fifelse(rm_slot == 1, "running_mate", "head"))], year ~ office + slot, value.var = "N", fill = 0))

#tableS5
finance[, share_party := party_funds / receipts_total]
print(finance[, .(tickets = .N, incumbent_head = mean(incumbent_head), incumbent_party = mean(incumbent_party), viable = mean(viable),
            experienced_head = mean(experienced_head), coalition_size = mean(coalition_size), any_party_funds = mean(party_funds > 0),
            median_party_funds = median(party_funds), share_party = mean(share_party)), by = .(year, type = fifelse(FH == 1, "FH", ticket_type))][order(year, type)])
print(finance[, .(fefc_share_of_receipts = sum(fefc) / sum(receipts_total)), by = year][order(year)])

#figureS1
print(finance_male[, .(any_party_funds = mean(party_funds > 0)), by = year][order(year)])
funds_by_type <- finance[, .(any_funds = mean(party_funds > 0), median_recipients = median(party_funds[party_funds > 0])),
                         by = .(year, type = fifelse(FH == 1, "Female-headed (FH)", fifelse(ticket_type == "HM", "Mixed (HM)", "All-male (HH)")))]
print(funds_by_type[order(year, type)])
figS1a <- ggplot(funds_by_type, aes(year, 100 * any_funds, linetype = type, shape = type)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(21, 17, 16)) + scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) + scale_y_continuous(limits = c(0, 100)) +
  labs(x = NULL, y = "Tickets receiving any party funds (%)")
figS1b <- ggplot(funds_by_type, aes(year, median_recipients / 1000, linetype = type, shape = type)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(21, 17, 16)) + scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) + scale_y_log10() +
  labs(x = NULL, y = "Median party funds among recipients\n(thousands of reais, log scale)")
figure_S1 <- figS1a + figS1b + plot_layout(guides = "collect") & theme(legend.position = "bottom")
if (interactive()) print(figure_S1)
ggsave("figure_S1.png", figure_S1, width = 6.3, height = 3.4, dpi = 300, device = agg_png, bg = "white")

#sectionS3
print(nrow(transitions))
print(finance_male[, .(before = any(post == 0), after = any(post == 1), mixed = any(HM == 1), all_male = any(HM == 0)), by = id_head][before & after & mixed & all_male, .N])
print(finance_male[, .(both = any(HM == 1) & any(HM == 0)), by = .(muni, year)][, .(races = .N, with_both = sum(both)), by = year][order(year)])
print(finance_male[, .(both = any(HM == 1) & any(HM == 0)), by = .(muni, year)][, .(races = .N, with_both = sum(both))])
print(finance[FH == 1 & incumbent_head == 1, .N, by = year][order(year)])
print(nrow(running_mates))

#tableS8
setFixest_dict(c(female = "Slot held by a woman", rm_slot = "Running-mate slot", post = "Post-earmark", year = "Election",
                 cycle = "Linear trend (election cycles)", d2020 = "Election 2020", d2024 = "Election 2024",
                 rm_female = "Female running mate", incumbent_head = "Head is incumbent mayor",
                 incumbent_party = "Head's party holds mayoralty", experienced_head = "Head previously elected",
                 coalition_size = "Parties in coalition", ln_party = "Log party funds", public_funds = "Public funds",
                 share_party = "Party share of receipts", party_funds_cash = "Party funds (cash)", party_funds = "Party funds",
                 HM = "Mixed ticket", FH = "Female-headed ticket", viable = "Viable ticket", viable_predicted = "Top quartile of predicted viability",
                 endow = "Log FEFC receipts (observed)", endow_statutory = "Log FEFC endowment (statutory)",
                 ln_size = "Log party size", left = "Left-leaning party", women_share = "Share reaching women's own candidacies",
                 share_hm = "Share to mixed tickets", share_hh = "Share to all-male tickets", exposure_post = "Mixed-ticket exposure in 2016 x Post-earmark",
                 share_female_council_cands = "Share of women among council candidates", prior_elected = "Previously elected",
                 prior_ran = "Previously ran", heads_next = "Heads a ticket in the following election", heads_viable_next = "Heads a viable ticket in the following election",
                 runs_next = "Runs in the following election", elected_next = "Elected in the following election", muni = "Municipality", state = "State",
                 party_head = "Party", party = "Party", id_head = "Head", race = "Race", party_funds_rm_party = "Funds from the running mate's party",
                 receives_rm_party = "Receives funds from the running mate's party", party_funds_own = "Funds from the head's party",
                 to_male_council = "Transfers to male council candidates", to_female_council = "Transfers to female council candidates",
                 share_to_male_council = "Share of receipts transferred to male council candidates",
                 fefc_to_male_council = "FEFC transferred to male council candidates", ln_receipts = "Log receipts", fefc_from_party = "FEFC from parties",
                 black_woman = "Slot held by a Black woman", white_woman = "Slot held by a white woman", black_man = "Slot held by a Black man",
                 HM_black = "Mixed ticket, Black running mate", HM_white = "Mixed ticket, white running mate", tight = "Party short of the floor in 2020",
                 small_muni = "Municipality with nine council seats", rm_financing_high = "High running-mate financing, 2012-2016",
                 rm_other_party = "Running mate from another party", rm_other_post = "Running mate from another party x Post-earmark",
                 exposure = "Mixed-ticket exposure in 2016", exposure_1216 = "Mixed-ticket exposure in 2012-2016",
                 replaced_by_woman = "Replaced by a woman", inkind = "In-kind party funds", inkind_material = "In-kind funds: campaign material",
                 inkind_media = "In-kind funds: media and advertising", inkind_services = "In-kind funds: services and polls",
                 fp_from_party = "Party Assistance Fund money from parties", party_rm = "Running mate's party",
                 receives_rm_party_tax_id = "Receives funds from the running mate's party (donors identified by tax number)",
                 rm_endow = "Log statutory FEFC endowment of the running mate's party"))
slots[, `:=`(cycle = (year - 2000) / 4, d2020 = as.integer(year == 2020), d2024 = as.integer(year == 2024))]
male_headed[, `:=`(cycle = (year - 2000) / 4, d2020 = as.integer(year == 2020), d2024 = as.integer(year == 2024))]
h1_trend <- feols(female ~ rm_slot + rm_slot:cycle + rm_slot:d2020 + rm_slot:d2024 | muni^year, slots, cluster = ~muni)
h1_its <- feols(rm_female ~ cycle + d2020 + d2024 | muni, male_headed, cluster = ~muni)
h1_its_party <- feols(rm_female ~ cycle + d2020 + d2024 | muni^party_head, male_headed, cluster = ~muni)
h1_its_0816 <- feols(rm_female ~ cycle + d2020 + d2024 | muni, male_headed[year >= 2008], cluster = ~muni)
print(etable(h1_dd, h1_es, h1_trend, h1_its, h1_its_party, h1_its_0816, se.below = TRUE, digits = 3, fitstat = ~ n + r2))

#tableS9
h1g_2018 <- feols(female ~ rm_slot + rm_slot:post | race, governor[year <= 2018], cluster = ~state)
h1s_dd <- feols(female ~ rm_slot + rm_slot:post | race, senate, cluster = ~state)
print(etable(h1g_es, h1g_dd, h1g_2018, h1s_dd, h1s_es, se.below = TRUE, digits = 3, fitstat = ~ n + r2))

#tableS10
slots[, race_weight := 1 / .N, by = .(muni, year)]
rm_financing <- tickets[year %in% c(2012, 2016) & head_female == 0 & receipts_total > 0, .(rm_financing = mean(from_rm / receipts_total)), by = muni]
rm_financing[, rm_financing_high := as.integer(rm_financing > median(rm_financing))]
slots <- merge(slots, rm_financing, by = "muni", all.x = TRUE)
h1_weighted <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots, weights = ~race_weight, cluster = ~muni)
h1_rm_financing <- feols(female ~ rm_slot + rm_slot:post + rm_slot:rm_financing_high + rm_slot:post:rm_financing_high | muni^year, slots[!is.na(rm_financing_high)], cluster = ~muni)
print(etable(h1_constant, h1_weighted, h1_small, h1_rm_financing, h1_origin, h1_black_women, h1_white_women, h1_black_men, se.below = TRUE, digits = 3, fitstat = ~ n + r2))

#tableS11
h2_pooled <- feols(rm_female ~ i(year, ref = 2016) + incumbent_head, transitions, cluster = ~muni)
h2_party <- feols(rm_female ~ i(year, ref = 2016) + incumbent_head | muni + party_head, transitions, cluster = ~muni)
h2_replaced <- feols(rm_female ~ i(year, ref = 2016) + incumbent_head | muni, transitions[replaced == 1], cluster = ~muni)
print(etable(h2_pooled, h2_muni, h2_party, h2_replaced, h2_nonincumbent, se.below = TRUE, digits = 3, fitstat = ~ n + r2))
b <- coef(h2_replaced)
v <- vcov(h2_replaced)
for (target in c("year::2020", "year::2024")) {
  k <- c(target, "year::2008", "year::2012")
  l <- c(1, -1 / 3, -1 / 3)
  h2_contrasts <- rbind(h2_contrasts, data.table(model = "h2_replaced", transition = target, est = sum(l * b[k]), se = sqrt(drop(t(l) %*% v[k, k] %*% l))), fill = TRUE)
}
h2_contrasts[, p := 2 * pnorm(-abs(est / se))]
print(h2_contrasts[, .(model, transition, pp = 100 * est, low = 100 * (est - 1.96 * se), high = 100 * (est + 1.96 * se), p)])

#tableS12
displaced_elected <- feols(elected_next ~ replaced_by_woman + replaced_by_woman:post | year + state, displaced, cluster = ~muni)
displaced_runs_muni <- feols(runs_next ~ replaced_by_woman + replaced_by_woman:post | muni + year, displaced, cluster = ~muni)
print(etable(displaced_runs, displaced_elected, displaced_runs_muni, se.below = TRUE, digits = 3, fitstat = ~ n + r2))

#tableS13
finance_male[, `:=`(ln_party = log1p(party_funds), public_funds = fefc + party_fund_fp, share_party = party_funds / receipts_total)]
h3_ppml_public <- fepois(as.formula(paste("public_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
h3_dd <- feols(as.formula(paste("ln_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
h3_head <- feols(as.formula(paste("ln_party ~ HM + HM:post +", controls, "| id_head + muni^year + party_head^year")), finance_male, cluster = ~muni)
h3_share <- feols(as.formula(paste("share_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(etable(h3_ppml, h3_ppml_es, h3_ppml_2016, h3_ppml_public, h3_ppml_cash, h3_dd, h3_head, h3_share, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + pr2))

#tableS14
h3_inkind <- fepois(as.formula(paste("inkind ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
h3_inkind_es <- fepois(as.formula(paste("inkind ~ HM + i(year, HM, ref = 2016) +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
inkind_material_post <- fepois(as.formula(paste("inkind_material ~ HM +", controls, "| muni^year + party_head^year")), finance_male[post == 1], cluster = ~muni)
inkind_media_post <- fepois(as.formula(paste("inkind_media ~ HM +", controls, "| muni^year + party_head^year")), finance_male[post == 1], cluster = ~muni)
inkind_services_post <- fepois(as.formula(paste("inkind_services ~ HM +", controls, "| muni^year + party_head^year")), finance_male[post == 1], cluster = ~muni)
fefc_post <- fepois(as.formula(paste("fefc_from_party ~ HM +", controls, "| muni^year + party_head^year")), finance_male[post == 1], cluster = ~muni)
fp_dd <- fepois(as.formula(paste("fp_from_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
print(etable(h3_cash_es, h3_inkind, h3_inkind_es, fp_dd, inkind_material_post, inkind_media_post, inkind_services_post, fefc_post, se.below = TRUE, digits = 3, fitstat = ~ n + pr2))

#tableS15
cross_party_lpm <- feols(as.formula(paste("receives_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni)
cross_party_ppml <- fepois(as.formula(paste("party_funds_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni)
h3_allied <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni)
h3_same <- fepois(as.formula(paste("party_funds ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male[rm_other_party == 0 & year >= 2016], cluster = ~muni)
h3_own <- fepois(as.formula(paste("party_funds_own ~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
finance_race <- finance_male[year >= 2016]
finance_race[, `:=`(HM_black = as.integer(HM == 1 & rm_black %in% 1L), HM_white = as.integer(HM == 1 & rm_black %in% 0L))]
h3_race <- fepois(as.formula(paste("party_funds ~ HM_black + HM_white + HM_black:post + HM_white:post +", controls, "| muni^year + party_head^year")), finance_race, cluster = ~muni)
finance_male[, ln_receipts := log1p(receipts_total)]
dest_male <- fepois(as.formula(paste("to_male_council ~ HM + HM:post + ln_receipts +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
dest_female <- fepois(as.formula(paste("to_female_council ~ HM + HM:post + ln_receipts +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
dest_fefc_male <- fepois(as.formula(paste("fefc_to_male_council ~ HM + log1p(fefc_from_party) +", controls, "| muni^year + party_head^year")), finance_male[post == 1], cluster = ~muni)
print(etable(cross_party_lpm, cross_party_ppml, h3_allied, h3_same, h3_own, h3_race, dest_male, dest_female, dest_share_male, dest_fefc_male,
       se.below = TRUE, digits = 3, fitstat = ~ n + r2 + pr2))

#tableS16
cross_party_rm <- feols(as.formula(paste("receives_rm_party ~ HM + HM:post +", controls, "| muni^year + party_head^year")), allied, cluster = ~party_rm)
allied <- merge(allied, unique(tickets[!is.na(statutory_2020), .(party_rm = party_head, rm_endow = log(statutory_2020))]), by = "party_rm", all.x = TRUE)
cross_party_endow <- feols(as.formula(paste("receives_rm_party ~ HM + HM:post + rm_endow:post +", controls, "| muni^year + party_head^year")),
                           allied[!is.na(rm_endow)], cluster = ~muni + party_rm)
print(etable(cross_party_twoway, cross_party_rm, cross_party_poisson, cross_party_tax_id, cross_party_stable, cross_party_pair, cross_party_endow,
       se.below = TRUE, digits = 3, fitstat = ~ n + r2 + pr2))

#tableS17
composition[, endow := log1p(endowment_2020)]
finance_male[, endow := log1p(endowment_2020)]
h4b_comp_distributed <- feols(as.formula(paste("rm_female ~ endow:post +", controls, "| muni^year + party_head")), composition[!is.na(endow)], cluster = ~muni + party_head)
h4b_money_distributed <- fepois(as.formula(paste("party_funds ~ HM + HM:post + HM:endow + HM:post:endow +", controls, "| muni^year + party_head^year")),
                                finance_male[!is.na(endow)], cluster = ~muni + party_head)
slack <- accounting[year == 2020, .(party_head = party, slack = own_share - legal_floor)]
finance_slack <- merge(finance_male, slack, by = "party_head")
finance_slack[, tight := as.integer(slack < 0)]
slack_ppml <- fepois(as.formula(paste("party_funds ~ HM + HM:post + HM:tight + HM:post:tight +", controls, "| muni^year + party_head^year")), finance_slack, cluster = ~muni + party_head)
print(etable(h4a_comp, h4a_comp_pred, h4a_money, h4a_money_pred, h4b_comp, h4b_comp_distributed, h4b_money, h4b_money_distributed, slack_ppml,
       se.below = TRUE, digits = 3, fitstat = ~ n + r2 + pr2))

#tableS18
print(etable(rival_pre, rival_post, se.below = TRUE, digits = 3, fitstat = ~ n + r2))
print(exp(quantile(composition[post == 0]$ln_size, c(0.25, 0.75))))
print(size_iqr)

#tableS19
finance[, ln_party := log1p(party_funds)]
h5_dd <- feols(as.formula(paste("ln_party ~ HM + FH + HM:post + FH:post +", controls, "| muni^year + party_head^year")), finance, cluster = ~muni)
print(etable(h5_ppml, h5_ppml_es, h5_ppml_2016, h5_ppml_incumbent, h5_dd, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + pr2))

#tableS20
exposure_1216 <- tickets[year %in% c(2012, 2016) & head_female == 0, .(exposure_1216 = mean(ticket_type == "HM")), by = .(state, party = party_head)]
h6_cells <- merge(h6_cells, exposure_1216, by = c("state", "party"))
h6_dd_unweighted <- feols(women_share ~ exposure_post + share_female_council_cands | state^party + party^year + state^year, h6_cells, cluster = ~party)
h6_dd_1216 <- feols(women_share ~ exposure_1216:post + share_female_council_cands | state^party + party^year + state^year, h6_cells, weights = ~total, cluster = ~party)
print(etable(h6_dd, h6_dd_unweighted, h6_dd_1216, h6_es, h6_cross, se.below = TRUE, digits = 3, fitstat = ~ n + r2))
print(h6_diff)

#tableS21
h7_ran <- feols(prior_ran ~ rm_female + rm_female:post | muni^year + party_head^year, running_mates, cluster = ~muni)
h7_heads_tickets <- feols(heads_next ~ rm_female + rm_female:post + rm_female:log(n_tickets_next) | muni^year + party_head^year, running_mates[year <= 2020], cluster = ~muni)
h7_runs <- feols(runs_next ~ rm_female + rm_female:post | muni^year + party_head^year, running_mates[year <= 2020], cluster = ~muni)
h7_heads_won <- feols(heads_next ~ rm_female + rm_female:post | state^year + party_head^year, running_mates[year <= 2020 & won == 1], cluster = ~muni)
print(etable(h7_prior, h7_ran, h7_heads, h7_heads_tickets, h7_heads_viable, h7_runs, h7_elected, h7_heads_won, se.below = TRUE, digits = 3, fitstat = ~ n + r2))

#figureS2
raw_general <- general_slots[office != "president", .(share = mean(female), slots = .N),
                             by = .(office, year, slot = fifelse(rm_slot == 1, "Running mates or alternates on male-headed tickets", "Heads of tickets"))]
raw_general[, office := factor(office, levels = c("governor", "senate"), labels = c("(a) Governor and vice-governor", "(b) Senator and alternates"))]
print(raw_general[order(office, slot, year)])
figS2 <- ggplot(raw_general, aes(year, 100 * share, linetype = slot, shape = slot)) + facet_wrap(~office, ncol = 2) +
  geom_vline(xintercept = 2016, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(21, 16)) + scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = seq(2002, 2022, 4)) + scale_y_continuous(limits = c(0, 60)) +
  guides(shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Share of slots held by women (%)") + theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))
figure_S2 <- figS2
if (interactive()) print(figure_S2)
ggsave("figure_S2.png", figure_S2, width = 6.3, height = 3.4, dpi = 300, device = agg_png, bg = "white")
bootstrap_cases <- list(
  list(name = "H1 gubernatorial tickets, 2002-2018 sample", data = governor[year <= 2018], y = "female", x = c("rm_slot", "rm_post"), fe = list("race"), w = NULL,
       cluster = "state", test = c(rm_post = 1)),
  list(name = "H1 Senate alternates, 2018 and 2022", data = senate, y = "female", x = c("rm_slot", "rm_post"), fe = list("race"), w = NULL,
       cluster = "state", test = c(rm_post = 1))
)
for (case in bootstrap_cases) {
  set.seed(20260927)
  d <- case$data[complete.cases(case$data[, c(case$y, case$x), with = FALSE])]
  weights_b <- if (is.null(case$w)) rep(1, nrow(d)) else d[[case$w]]
  factors <- list()
  for (v in case$fe) factors[[length(factors) + 1]] <- interaction(d[, v, with = FALSE], drop = TRUE)
  z <- demean(as.matrix(d[, c(case$y, case$x), with = FALSE]), factors, weights = weights_b)
  y_d <- z[, 1]
  x_d <- z[, -1, drop = FALSE]
  x_d <- x_d[, colSums(x_d^2) > 1e-10, drop = FALSE]
  groups <- as.integer(factor(d[[case$cluster]]))
  n_groups <- max(groups)
  r_vec <- setNames(rep(0, ncol(x_d)), colnames(x_d))
  r_vec[names(case$test)] <- case$test
  bread <- solve(crossprod(x_d * sqrt(weights_b)))
  b_hat <- drop(bread %*% crossprod(x_d * weights_b, y_d))
  meat <- crossprod(rowsum(x_d * (weights_b * (y_d - drop(x_d %*% b_hat))), groups))
  scale_b <- n_groups / (n_groups - 1) * (nrow(x_d) - 1) / (nrow(x_d) - ncol(x_d))
  t_hat <- sum(r_vec * b_hat) / sqrt(scale_b * drop(t(r_vec) %*% bread %*% meat %*% bread %*% r_vec))
  null_space <- qr.Q(qr(cbind(r_vec, diag(ncol(x_d)))))[, 2:ncol(x_d), drop = FALSE]
  x_r <- x_d %*% null_space
  fit_r <- drop(x_r %*% solve(crossprod(x_r * sqrt(weights_b)), crossprod(x_r * weights_b, y_d)))
  resid_r <- y_d - fit_r
  bootstrap_residuals <- matrix(0, nrow(x_d), n_groups)
  for (bootstrap_group in seq_len(n_groups)) bootstrap_residuals[groups == bootstrap_group, bootstrap_group] <- resid_r[groups == bootstrap_group]
  bootstrap_residuals <- demean(bootstrap_residuals, factors, weights = weights_b)
  bootstrap_coefficients <- bread %*% crossprod(x_d * weights_b, bootstrap_residuals)
  bootstrap_residuals <- bootstrap_residuals - x_d %*% bootstrap_coefficients
  bootstrap_scores <- rowsum(bootstrap_residuals * drop(weights_b * (x_d %*% bread %*% r_vec)), groups)
  bootstrap_numerator <- drop(t(r_vec) %*% bootstrap_coefficients)
  t_star <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    multipliers <- sample(webb, n_groups, replace = TRUE)
    t_star[i] <- sum(bootstrap_numerator * multipliers) / sqrt(scale_b * sum((bootstrap_scores %*% multipliers)^2))
  }
  rm(bootstrap_residuals, bootstrap_coefficients, bootstrap_scores, bootstrap_numerator)
  bootstrap <- rbind(bootstrap, data.table(case = case$name, estimate = sum(r_vec * b_hat), t = t_hat, p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = n_groups))
}
print(bootstrap)

#figureS3
reforms <- rbind(
  data.table(test = "Baseline", est = coef(h1_dd)[["rm_slot:post"]], se = se(h1_dd)[["rm_slot:post"]]),
  data.table(test = "Same number of tickets in 2016, 2020, and 2024", est = coef(h1_constant)[["rm_slot:post"]], se = se(h1_constant)[["rm_slot:post"]]),
  data.table(test = "Each race weighted equally", est = coef(h1_weighted)[["rm_slot:post"]], se = se(h1_weighted)[["rm_slot:post"]]),
  data.table(test = "Municipalities with more than nine seats", est = coef(h1_small)[["rm_slot:post"]], se = se(h1_small)[["rm_slot:post"]]),
  data.table(test = "Additional change, municipalities with nine seats", est = coef(h1_small)[["rm_slot:post:small_muni"]], se = se(h1_small)[["rm_slot:post:small_muni"]]),
  data.table(test = "Additional change where running mates\nfinanced tickets more, 2012-2016", est = coef(h1_rm_financing)[["rm_slot:post:rm_financing_high"]],
             se = se(h1_rm_financing)[["rm_slot:post:rm_financing_high"]]),
  data.table(test = "Running mate from another party vs.\nfrom the head's party, 2016-2024", est = coef(h1_origin)[["rm_other_post"]], se = se(h1_origin)[["rm_other_post"]])
)
reforms[, test := factor(test, levels = rev(test))]
print(reforms[, .(test, pp = 100 * est, low = 100 * (est - 1.96 * se), high = 100 * (est + 1.96 * se), p = 2 * pnorm(-abs(est / se)))])
figS3 <- ggplot(reforms, aes(100 * est, test)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Post-earmark change (percentage points)", y = NULL)
figure_S3 <- figS3
if (interactive()) print(figure_S3)
ggsave("figure_S3.png", figure_S3, width = 6.3, height = 3.6, dpi = 300, device = agg_png, bg = "white")

#figureS4
race_rates <- male_headed[year >= 2016 & !is.na(rm_black), .(`Black women` = mean(rm_female == 1 & rm_black == 1), `White women` = mean(rm_female == 1 & rm_black == 0),
                                                            `Black men` = mean(rm_female == 0 & rm_black == 1), `White men` = mean(rm_female == 0 & rm_black == 0)), by = year]
race_rates <- melt(race_rates, id.vars = "year", variable.name = "group", value.name = "share")
race_dd <- data.table(group = c("Black women", "White women", "Black men"),
                      est = c(coef(h1_black_women)[["rm_slot:post"]], coef(h1_white_women)[["rm_slot:post"]], coef(h1_black_men)[["rm_slot:post"]]),
                      se = c(se(h1_black_women)[["rm_slot:post"]], se(h1_white_women)[["rm_slot:post"]], se(h1_black_men)[["rm_slot:post"]]))
race_dd[, group := factor(group, levels = rev(group))]
figS4a <- ggplot(race_rates, aes(factor(year), 100 * share, fill = group)) + geom_col(position = "dodge", width = 0.7, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("black", "grey45", "grey75", "white")) + guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Share of running mates on\nmale-headed tickets (%)")
figS4b <- ggplot(race_dd, aes(100 * est, group)) + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Running-mate slot minus head slot,\n2020-2024 vs. 2016 (pp)", y = NULL)
figure_S4 <- figS4a + figS4b + plot_layout(widths = c(1.3, 1))
if (interactive()) print(figure_S4)
ggsave("figure_S4.png", figure_S4, width = 6.3, height = 3.4, dpi = 300, device = agg_png, bg = "white")
race_terms <- c("HM_black:post", "HM_white:post")
print(data.table(term = race_terms, percent = 100 * (exp(coef(h3_race)[race_terms]) - 1), p = pvalue(h3_race)[race_terms]))
race_vcov <- vcov(h3_race)[race_terms, race_terms]
print(2 * pnorm(-abs(diff(coef(h3_race)[race_terms]) / sqrt(race_vcov[1, 1] + race_vcov[2, 2] - 2 * race_vcov[1, 2]))))

#figureS5
rates_h2 <- rbind(
  transitions[, .(share = mean(rm_female), n = .N), by = year][, group := "All heads who had a male running mate"],
  transitions[replaced == 1, .(share = mean(rm_female), n = .N), by = year][, group := "Heads who changed running mates"]
)
rates_h2[, se := sqrt(share * (1 - share) / n)]
rates_h2[, pair := factor(paste0(year - 4, "–", substr(year, 3, 4)))]
print(rates_h2[order(group, year)])
figS5 <- ggplot(rates_h2, aes(pair, 100 * share, shape = group)) +
  geom_vline(xintercept = 3.5, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = 100 * (share - 1.96 * se), ymax = 100 * (share + 1.96 * se)), width = 0.15, position = position_dodge(0.45)) +
  geom_point(size = 2.2, fill = "white", position = position_dodge(0.45)) + scale_shape_manual(values = c(16, 24)) +
  labs(x = "Pair of consecutive elections", y = "New running mate is a woman (%)")
figure_S5 <- figS5
if (interactive()) print(figure_S5)
ggsave("figure_S5.png", figure_S5, width = 6, height = 3.3, dpi = 300, device = agg_png, bg = "white")

#figureS6
displaced_rates <- displaced[, .(ran = mean(runs_next), elected = mean(elected_next), n = .N), by = .(post, replaced_by_woman)]
displaced_rates <- melt(displaced_rates, id.vars = c("post", "replaced_by_woman", "n"), variable.name = "outcome", value.name = "share")
displaced_rates[, se := sqrt(share * (1 - share) / n)]
displaced_rates[, period := factor(post, levels = c(0, 1), labels = c("Replaced in\n2012 or 2016", "Replaced in\n2020 or 2024"))]
displaced_rates[, replaced := factor(replaced_by_woman, levels = c(1, 0), labels = c("Replaced by a woman", "Replaced by a man"))]
displaced_rates[, outcome := factor(outcome, levels = c("ran", "elected"), labels = c("(a) Ran for municipal office", "(b) Won municipal office"))]
print(displaced_rates[order(outcome, post, replaced)])
print(data.table(model = c("ran", "won", "ran, municipality fixed effects"),
           pp = 100 * c(coef(displaced_runs)[["replaced_by_woman:post"]], coef(displaced_elected)[["replaced_by_woman:post"]], coef(displaced_runs_muni)[["replaced_by_woman:post"]]),
           p = c(pvalue(displaced_runs)[["replaced_by_woman:post"]], pvalue(displaced_elected)[["replaced_by_woman:post"]], pvalue(displaced_runs_muni)[["replaced_by_woman:post"]])))
figS6 <- ggplot(displaced_rates, aes(period, 100 * share, shape = replaced)) + facet_wrap(~outcome, scales = "free_y") +
  geom_errorbar(aes(ymin = 100 * (share - 1.96 * se), ymax = 100 * (share + 1.96 * se)), width = 0.15, position = position_dodge(0.4)) +
  geom_point(size = 2.2, fill = "white", position = position_dodge(0.4)) + scale_shape_manual(values = c(16, 21)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "Displaced male running mates, in the\nelection in which they were replaced (%)") +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))
figure_S6 <- figS6
if (interactive()) print(figure_S6)
ggsave("figure_S6.png", figure_S6, width = 6.3, height = 3.4, dpi = 300, device = agg_png, bg = "white")

#figureS7
inkind_shares <- finance_male[, .(share = sum(inkind) / sum(party_funds)), by = .(year, type = fifelse(HM == 1, "Mixed tickets", "All-male tickets"))]
print(inkind_shares[order(type, year)])
inkind_nature <- finance_male[post == 1, .(material = sum(inkind_material), media = sum(inkind_media), services = sum(inkind_services), other = sum(inkind_other)), by = HM]
inkind_nature[, total := material + media + services + other]
print(inkind_nature[, .(HM, material = material / total, media = media / total, services = services / total, other = other / total)])
components <- rbind(
  data.table(test = "Cash transfers, difference-in-differences", est = coef(h3_ppml_cash)[["HM:post"]], se = se(h3_ppml_cash)[["HM:post"]]),
  data.table(test = "In-kind contributions, difference-in-differences", est = coef(h3_inkind)[["HM:post"]], se = se(h3_inkind)[["HM:post"]]),
  data.table(test = "Party Assistance Fund money, difference-in-differences", est = coef(fp_dd)[["HM:post"]], se = se(fp_dd)[["HM:post"]]),
  data.table(test = "FEFC money, 2020-2024", est = coef(fefc_post)[["HM"]], se = se(fefc_post)[["HM"]]),
  data.table(test = "In-kind services and polls, 2020-2024", est = coef(inkind_services_post)[["HM"]], se = se(inkind_services_post)[["HM"]]),
  data.table(test = "In-kind campaign material, 2020-2024", est = coef(inkind_material_post)[["HM"]], se = se(inkind_material_post)[["HM"]]),
  data.table(test = "In-kind media and advertising, 2020-2024", est = coef(inkind_media_post)[["HM"]], se = se(inkind_media_post)[["HM"]])
)
components[, test := factor(test, levels = rev(test))]
print(components[, .(test, percent = 100 * (exp(est) - 1), p = 2 * pnorm(-abs(est / se)))])
figS7a <- ggplot(inkind_shares, aes(year, 100 * share, linetype = type, shape = type)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(21, 16)) + scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  guides(linetype = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "In-kind share of party funds (%)")
figS7b <- ggplot(components, aes(100 * (exp(est) - 1), test)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Mixed relative to all-male tickets (%)", y = NULL)
figure_S7 <- figS7a + figS7b + plot_layout(widths = c(0.8, 1.2))
if (interactive()) print(figure_S7)
ggsave("figure_S7.png", figure_S7, width = 6.8, height = 3.6, dpi = 300, device = agg_png, bg = "white")

#figureS8
print(data.table(term = c("HM", "HM:post"), pp = 100 * coef(cross_party_lpm)[c("HM", "HM:post")], low = 100 * (coef(cross_party_lpm)[c("HM", "HM:post")] - 1.96 * se(cross_party_lpm)[c("HM", "HM:post")]),
           high = 100 * (coef(cross_party_lpm)[c("HM", "HM:post")] + 1.96 * se(cross_party_lpm)[c("HM", "HM:post")]), p = pvalue(cross_party_lpm)[c("HM", "HM:post")]))
cross_party_es <- feols(as.formula(paste("receives_rm_party ~ HM + i(year, HM, ref = 2016) +", controls, "| muni^year + party_head^year")), allied, cluster = ~muni)
print(100 * coef(cross_party_es))
print(allied[, .(funds_from_running_mate_party_million = sum(party_funds_rm_party) / 1e6), by = .(year, HM)][order(HM, year)])
print(data.table(percent = 100 * (exp(coef(cross_party_ppml)[["HM:post"]]) - 1), low = 100 * (exp(coef(cross_party_ppml)[["HM:post"]] - 1.96 * se(cross_party_ppml)[["HM:post"]]) - 1),
           high = 100 * (exp(coef(cross_party_ppml)[["HM:post"]] + 1.96 * se(cross_party_ppml)[["HM:post"]]) - 1)))
mechanism <- rbind(
  data.table(test = "Funds from the running mate's party,\nallied tickets, 2016-2024 (Poisson)", unit = "pct", est = coef(cross_party_ppml)[["HM:post"]], se = se(cross_party_ppml)[["HM:post"]]),
  data.table(test = "Total party funds, allied tickets,\n2016-2024 (Poisson)", unit = "pct", est = coef(h3_allied)[["HM:post"]], se = se(h3_allied)[["HM:post"]]),
  data.table(test = "Total party funds, same-party tickets,\n2016-2024 (Poisson)", unit = "pct", est = coef(h3_same)[["HM:post"]], se = se(h3_same)[["HM:post"]]),
  data.table(test = "Funds from the head's party (Poisson)", unit = "pct", est = coef(h3_own)[["HM:post"]], se = se(h3_own)[["HM:post"]]),
  data.table(test = "Transfers to male council candidates (Poisson)", unit = "pct", est = coef(dest_male)[["HM:post"]], se = se(dest_male)[["HM:post"]]),
  data.table(test = "Transfers to female council candidates (Poisson)", unit = "pct", est = coef(dest_female)[["HM:post"]], se = se(dest_female)[["HM:post"]])
)
mechanism[, test := factor(test, levels = rev(test))]
print(mechanism[, .(test, percent = 100 * (exp(est) - 1), p = 2 * pnorm(-abs(est / se)))])
print(data.table(percent = 100 * (exp(coef(dest_fefc_male)[["HM"]]) - 1), p = pvalue(dest_fefc_male)[["HM"]]))
figS8 <- ggplot(mechanism, aes(100 * (exp(est) - 1), test)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Mixed relative to all-male tickets (%)", y = NULL)
figure_S8 <- figS8
if (interactive()) print(figure_S8)
ggsave("figure_S8.png", figure_S8, width = 6.3, height = 3.8, dpi = 300, device = agg_png, bg = "white")

#figureS9
cross_robust <- rbind(
  data.table(test = "Baseline, clustered by municipality", est = coef(cross_party_lpm)[["HM:post"]], se = se(cross_party_lpm)[["HM:post"]]),
  data.table(test = "Clustered by municipality and by\nthe running mate's party", est = coef(cross_party_twoway)[["HM:post"]], se = se(cross_party_twoway)[["HM:post"]]),
  data.table(test = "Clustered by the running mate's party", est = coef(cross_party_rm)[["HM:post"]], se = se(cross_party_rm)[["HM:post"]]),
  data.table(test = "Donor parties identified by tax number only", est = coef(cross_party_tax_id)[["HM:post"]], se = se(cross_party_tax_id)[["HM:post"]]),
  data.table(test = "Municipalities with both kinds of allied\nticket before and after the earmark", est = coef(cross_party_stable)[["HM:post"]], se = se(cross_party_stable)[["HM:post"]]),
  data.table(test = "Fixed effects for each pair of parties", est = coef(cross_party_pair)[["HM:post"]], se = se(cross_party_pair)[["HM:post"]]),
  data.table(test = "Controlling for the FEFC endowment\nof the running mate's party", est = coef(cross_party_endow)[["HM:post"]], se = se(cross_party_endow)[["HM:post"]])
)
cross_robust[, test := factor(test, levels = rev(test))]
print(cross_robust[, .(test, pp = 100 * est, low = 100 * (est - 1.96 * se), high = 100 * (est + 1.96 * se), p = 2 * pnorm(-abs(est / se)))])
figS9a <- ggplot(cross_robust, aes(100 * est, test)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  scale_x_continuous(limits = c(0, NA)) + labs(x = "Post-earmark change, mixed vs. all-male\nallied tickets (percentage points)", y = NULL)
counterfactual_long <- rbind(
  counterfactual[, .(year, series = "Mixed tickets, observed", share = mixed)],
  counterfactual[, .(year, series = "All-male tickets, observed", share = all_male)],
  counterfactual[year >= 2016, .(year, series = "Mixed tickets had the\n2016 ratio held", share = proportional)],
  counterfactual[year >= 2016, .(year, series = "Mixed tickets had the\n2016 gap held", share = additive)]
)
counterfactual_long[, series := factor(series, levels = c("Mixed tickets, observed", "All-male tickets, observed", "Mixed tickets had the\n2016 ratio held", "Mixed tickets had the\n2016 gap held"))]
figS9b <- ggplot(counterfactual_long, aes(year, 100 * share, linetype = series, shape = series)) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 2, fill = "white") +
  scale_shape_manual(values = c(16, 21, 24, 22)) + scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  scale_x_continuous(breaks = c(2016, 2020, 2024)) + scale_y_continuous(limits = c(0, 60), expand = expansion(mult = c(0, 0.02))) +
  guides(linetype = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) +
  labs(x = NULL, y = "Allied tickets receiving funds from\nthe running mate's party (%)")
figure_S9 <- figS9a + figS9b + plot_layout(widths = c(1.2, 1))
if (interactive()) print(figure_S9)
ggsave("figure_S9.png", figure_S9, width = 6.8, height = 3.9, dpi = 300, device = agg_png, bg = "white")

#figureS10
print(finance_male[post == 1 & HM == 0, .(tickets = .N, funds = sum(party_funds)), by = viable][, `:=`(share_tickets = tickets / sum(tickets), share_funds = funds / sum(funds))][])
money_where <- rbind(
  mixed_post[, .(group = "Nonviable ticket\n(no incumbency)", tickets = mean(viable == 0), funds = sum(party_funds[viable == 0]) / sum(party_funds))],
  mixed_post[, .(group = "Nonviable ticket\n(predicted)", tickets = mean(viable_predicted == 0), funds = sum(party_funds[viable_predicted == 0]) / sum(party_funds))],
  mixed_post[, .(group = "Ticket lost\nthe election", tickets = mean(won == 0), funds = sum(party_funds[won == 0]) / sum(party_funds))],
  mixed_post[, .(group = "Runoff municipality\nor state capital", tickets = mean(runoff_muni == 1 | capital == 1), funds = sum(party_funds[runoff_muni == 1 | capital == 1]) / sum(party_funds))]
)
money_where <- melt(money_where, id.vars = "group", variable.name = "what", value.name = "share")
money_where[, group := factor(group, levels = unique(group))]
money_where[, what := factor(what, levels = c("tickets", "funds"), labels = c("Share of mixed tickets", "Share of their party funds"))]
print(money_where)
figS10a <- ggplot(money_where, aes(group, 100 * share, fill = what)) +
  geom_col(position = position_dodge(0.7), width = 0.65, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("white", "grey45")) + scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  guides(fill = guide_legend(nrow = 2)) + labs(x = NULL, y = "Mixed tickets, 2020 and 2024 (%)") + theme(axis.text.x = element_text(size = 7.5))
new_female_rms <- composition[year %in% c(2016, 2020, 2024), .(female_rm = sum(rm_female)), by = .(year, viable)]
new_female_rms[, viable := factor(viable, levels = c(0, 1), labels = c("Nonviable tickets", "Viable tickets"))]
print(new_female_rms[order(viable, year)])
figS10b <- ggplot(new_female_rms, aes(factor(year), female_rm, fill = viable)) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) + scale_fill_manual(values = c("white", "grey45")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) + guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Female running mates on\nmale-headed tickets (number)")
figure_S10 <- figS10a + figS10b + plot_layout(widths = c(1.5, 1))
if (interactive()) print(figure_S10)
ggsave("figure_S10.png", figure_S10, width = 6.8, height = 3.8, dpi = 300, device = agg_png, bg = "white")

#sectionS5_9
print(fitstat(viability_model, "pr2"))
print(data.table(pp = 100 * coef(h4a_comp_pred)[["viable_predicted:post"]], low = 100 * (coef(h4a_comp_pred)[["viable_predicted:post"]] - 1.96 * se(h4a_comp_pred)[["viable_predicted:post"]]),
           high = 100 * (coef(h4a_comp_pred)[["viable_predicted:post"]] + 1.96 * se(h4a_comp_pred)[["viable_predicted:post"]])))
print(data.table(model = c("viable", "top quartile of predicted viability"),
           percent = 100 * (exp(c(coef(h4a_money)[["post:viable"]], coef(h4a_money_pred)[["post:viable_predicted"]])) - 1),
           p = c(pvalue(h4a_money)[["post:viable"]], pvalue(h4a_money_pred)[["post:viable_predicted"]])))
print(data.table(pp_for_twice_the_distributed_endowment = 100 * log(2) * coef(h4b_comp_distributed)[["endow:post"]], p = pvalue(h4b_comp_distributed)[["endow:post"]],
           p_premium = pvalue(h4b_money_distributed)[["HM:post:endow"]]))
print(slack[, .(parties = .N, short_of_the_floor = sum(slack < 0))])
print(data.table(percent = 100 * (exp(coef(slack_ppml)[["HM:post:tight"]]) - 1), p = pvalue(slack_ppml)[["HM:post:tight"]]))

#figureS11
print(composition[post == 0, .(female_running_mates = mean(rm_female))])
rival_plot <- rbind(
  data.table(factor = "Larger party (75th vs.\n25th percentile of size)", when = "Association before the earmark (2008–2016)",
             est = size_iqr * coef(rival_pre)[["ln_size"]], se = size_iqr * se(rival_pre)[["ln_size"]]),
  data.table(factor = "Left-leaning party", when = "Association before the earmark (2008–2016)", est = coef(rival_pre)[["left"]], se = se(rival_pre)[["left"]]),
  data.table(factor = "Viable ticket", when = "Association before the earmark (2008–2016)", est = coef(rival_pre)[["viable"]], se = se(rival_pre)[["viable"]]),
  data.table(factor = "Larger party (75th vs.\n25th percentile of size)", when = "Post-earmark change in the association",
             est = size_iqr * coef(rival_post)[["ln_size:post"]], se = size_iqr * se(rival_post)[["ln_size:post"]]),
  data.table(factor = "Left-leaning party", when = "Post-earmark change in the association", est = coef(rival_post)[["left:post"]], se = se(rival_post)[["left:post"]]),
  data.table(factor = "Viable ticket", when = "Post-earmark change in the association", est = coef(rival_post)[["viable:post"]], se = se(rival_post)[["viable:post"]])
)
rival_plot[, factor := factor(factor, levels = rev(c("Larger party (75th vs.\n25th percentile of size)", "Left-leaning party", "Viable ticket")))]
print(rival_plot[, .(factor, when, pp = 100 * est, p = 2 * pnorm(-abs(est / se)))])
print(pvalue(rival_post)[c("ln_size:post", "left:post", "viable:post")])
figS11 <- ggplot(rival_plot, aes(100 * est, factor, shape = when)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.15, orientation = "y", position = position_dodge(0.5)) +
  geom_point(size = 2.2, fill = "white", position = position_dodge(0.5)) + scale_shape_manual(values = c(16, 24)) +
  guides(shape = guide_legend(nrow = 2)) + labs(x = "Probability of a female running mate (percentage points)", y = NULL)
figure_S11 <- figS11
if (interactive()) print(figure_S11)
ggsave("figure_S11.png", figure_S11, width = 6, height = 3.4, dpi = 300, device = agg_png, bg = "white")

#figureS12
print(data.table(period = c("before the earmark", "under the earmark"), percent = 100 * (exp(c(coef(h5_ppml)[["FH"]], coef(h5_ppml)[["FH"]] + coef(h5_ppml)[["FH:post"]])) - 1),
           p_before = pvalue(h5_ppml)[["FH"]]))
es_h5 <- as.data.table(coeftable(h5_ppml_es), keep.rownames = "term")[grepl("^year::\\d{4}:FH$", term)]
es_h5[, year := as.integer(sub("^year::(\\d{4}).*", "\\1", term))]
es_h5 <- rbind(es_h5[, .(year, est = Estimate, se = `Std. Error`)], data.table(year = 2016L, est = 0, se = 0))
print(es_h5[, .(year, percent = 100 * (exp(est) - 1), p = 2 * pnorm(-abs(est / se)))])
print(data.table(percent = 100 * (exp(coef(h5_dd)[["FH:post"]]) - 1), p = pvalue(h5_dd)[["FH:post"]]))
print(finance[FH == 1 & incumbent_head == 1 & post == 1, .N])
print(data.table(percent = 100 * (exp(coef(h5_ppml_incumbent)[["post:incumbent_head"]]) - 1), p = pvalue(h5_ppml_incumbent)[["post:incumbent_head"]]))
figS12a <- ggplot(es_h5, aes(year, 100 * (exp(est) - 1))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = 100 * (exp(est - 1.96 * se) - 1), ymax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  labs(x = NULL, y = "Party funds, female-headed vs. all-male\ntickets, relative to 2016 (%)")
figS12b <- ggplot(gains_h5, aes(100 * (exp(est) - 1), group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.15, orientation = "y") +
  geom_point(size = 2.2) + labs(x = "Post-earmark growth in the advantage\nof female-headed tickets (%)", y = NULL)
figure_S12 <- figS12a + figS12b + plot_layout(widths = c(1, 1.05))
if (interactive()) print(figure_S12)
ggsave("figure_S12.png", figure_S12, width = 6.3, height = 3.3, dpi = 300, device = agg_png, bg = "white")

#figureS13
es_h6 <- as.data.table(coeftable(h6_es), keep.rownames = "term")[grepl("^year::", term)]
es_h6[, year := as.integer(sub("^year::(\\d{4}).*", "\\1", term))]
es_h6 <- rbind(es_h6[, .(year, est = Estimate, se = `Std. Error`)], data.table(year = 2016L, est = 0, se = 0))
print(data.table(term = names(coef(h6_es)), points_per_10_points_of_exposure = 10 * coef(h6_es), p = pvalue(h6_es)))
print(10 * coef(h6_dd)[["exposure_post"]])
print(10 * c(share_hm = h6_b[["share_hm"]], share_hh = h6_b[["share_hh"]]))
bootstrap_cases <- list(
  list(name = "H6 difference-in-differences", data = h6_cells, y = "women_share", x = c("exposure_post", "share_female_council_cands"),
       fe = list(c("state", "party"), c("party", "year"), c("state", "year")), w = "total", cluster = "party", test = c(exposure_post = 1)),
  list(name = "H6 cross-section, mixed minus all-male tickets", data = cells, y = "women_share", x = c("share_hm", "share_hh", "share_female_council_cands"),
       fe = list(c("party", "year"), c("state", "year")), w = "total", cluster = "party", test = c(share_hm = 1, share_hh = -1))
)
for (case in bootstrap_cases) {
  set.seed(20260927)
  d <- case$data[complete.cases(case$data[, c(case$y, case$x), with = FALSE])]
  weights_b <- if (is.null(case$w)) rep(1, nrow(d)) else d[[case$w]]
  factors <- list()
  for (v in case$fe) factors[[length(factors) + 1]] <- interaction(d[, v, with = FALSE], drop = TRUE)
  z <- demean(as.matrix(d[, c(case$y, case$x), with = FALSE]), factors, weights = weights_b)
  y_d <- z[, 1]
  x_d <- z[, -1, drop = FALSE]
  x_d <- x_d[, colSums(x_d^2) > 1e-10, drop = FALSE]
  groups <- as.integer(factor(d[[case$cluster]]))
  n_groups <- max(groups)
  r_vec <- setNames(rep(0, ncol(x_d)), colnames(x_d))
  r_vec[names(case$test)] <- case$test
  bread <- solve(crossprod(x_d * sqrt(weights_b)))
  b_hat <- drop(bread %*% crossprod(x_d * weights_b, y_d))
  meat <- crossprod(rowsum(x_d * (weights_b * (y_d - drop(x_d %*% b_hat))), groups))
  scale_b <- n_groups / (n_groups - 1) * (nrow(x_d) - 1) / (nrow(x_d) - ncol(x_d))
  t_hat <- sum(r_vec * b_hat) / sqrt(scale_b * drop(t(r_vec) %*% bread %*% meat %*% bread %*% r_vec))
  null_space <- qr.Q(qr(cbind(r_vec, diag(ncol(x_d)))))[, 2:ncol(x_d), drop = FALSE]
  x_r <- x_d %*% null_space
  fit_r <- drop(x_r %*% solve(crossprod(x_r * sqrt(weights_b)), crossprod(x_r * weights_b, y_d)))
  resid_r <- y_d - fit_r
  bootstrap_residuals <- matrix(0, nrow(x_d), n_groups)
  for (bootstrap_group in seq_len(n_groups)) bootstrap_residuals[groups == bootstrap_group, bootstrap_group] <- resid_r[groups == bootstrap_group]
  bootstrap_residuals <- demean(bootstrap_residuals, factors, weights = weights_b)
  bootstrap_coefficients <- bread %*% crossprod(x_d * weights_b, bootstrap_residuals)
  bootstrap_residuals <- bootstrap_residuals - x_d %*% bootstrap_coefficients
  bootstrap_scores <- rowsum(bootstrap_residuals * drop(weights_b * (x_d %*% bread %*% r_vec)), groups)
  bootstrap_numerator <- drop(t(r_vec) %*% bootstrap_coefficients)
  t_star <- numeric(n_bootstrap)
  for (i in seq_len(n_bootstrap)) {
    multipliers <- sample(webb, n_groups, replace = TRUE)
    t_star[i] <- sum(bootstrap_numerator * multipliers) / sqrt(scale_b * sum((bootstrap_scores %*% multipliers)^2))
  }
  rm(bootstrap_residuals, bootstrap_coefficients, bootstrap_scores, bootstrap_numerator)
  bootstrap <- rbind(bootstrap, data.table(case = case$name, estimate = sum(r_vec * b_hat), t = t_hat, p_bootstrap = mean(abs(t_star) >= abs(t_hat)), clusters = n_groups))
}
print(bootstrap)
figS13a <- ggplot(es_h6, aes(year, 100 * est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = 100 * (est - 1.96 * se), ymax = 100 * (est + 1.96 * se)), width = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) +
  labs(x = NULL, y = "Share reaching women's own candidacies,\nper unit of 2016 exposure, relative to 2016 (pp)")
partial <- data.table()
for (focal in c("share_hm", "share_hh")) {
  other <- setdiff(c("share_hm", "share_hh"), focal)
  y_res <- resid(feols(as.formula(paste("women_share ~", other, "+ share_female_council_cands | party^year + state^year")), cells, weights = ~total), na.rm = FALSE)
  x_res <- resid(feols(as.formula(paste(focal, "~", other, "+ share_female_council_cands | party^year + state^year")), cells, weights = ~total), na.rm = FALSE)
  binned <- data.table(x = x_res, y = y_res, w = cells$total)[!is.na(x) & !is.na(y)][order(x)]
  binned[, bin := pmin(20L, 1L + floor(20 * (cumsum(w) - w / 2) / sum(w)))]
  binned <- binned[, .(x = weighted.mean(x, w), y = weighted.mean(y, w)), by = bin]
  partial <- rbind(partial, binned[, focal := fifelse(focal == "share_hm", "To mixed tickets", "To all-male tickets")])
}
slopes <- data.table(focal = c("To mixed tickets", "To all-male tickets"), slope = c(h6_b[["share_hm"]], h6_b[["share_hh"]]))
figS13b <- ggplot(partial, aes(100 * x, 100 * y, shape = focal)) +
  geom_abline(data = slopes, aes(slope = slope, intercept = 0, linetype = focal)) +
  geom_point(size = 2, fill = "white") + scale_shape_manual(values = c(21, 16)) + scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = "Share of FEFC receipts by recipient party,\nresidualized (pp)", y = "Share reaching women's own\ncandidacies, residualized (pp)")
figure_S13 <- figS13a + figS13b + plot_layout(widths = c(1, 1.1))
if (interactive()) print(figure_S13)
ggsave("figure_S13.png", figure_S13, width = 6.3, height = 3.6, dpi = 300, device = agg_png, bg = "white")

#tableS22
print(accounting[order(year, -total), .(year, party, fefc_million = round(total / 1e6, 2), own_share = round(100 * own_share, 1),
                                   share_with_mixed = round(100 * share_with_mixed, 1), legal_floor = round(100 * legal_floor, 1), status)])
print(accounting[, .(parties = .N, floor_above_30 = sum(legal_floor > 0.3), median_floor = median(legal_floor)), by = year])
print(accounting[abs(own_share - legal_floor) < 0.035, .(year, party, own_minus_floor = 100 * (own_share - legal_floor))])
print(accounting[, .(women_own_share = sum(fh + female_council) / sum(total), mixed_share = sum(hm) / sum(total)), by = year])

#figureS14
careers <- rbind(
  data.table(outcome = "Previously elected to municipal office", est = coef(h7_prior)[["rm_female:post"]], se = se(h7_prior)[["rm_female:post"]]),
  data.table(outcome = "Previously ran for municipal office", est = coef(h7_ran)[["rm_female:post"]], se = se(h7_ran)[["rm_female:post"]]),
  data.table(outcome = "Headed a ticket in the following election", est = coef(h7_heads)[["rm_female:post"]], se = se(h7_heads)[["rm_female:post"]]),
  data.table(outcome = "Headed a ticket, controlling for the\nnumber of tickets in the following election", est = coef(h7_heads_tickets)[["rm_female:post"]], se = se(h7_heads_tickets)[["rm_female:post"]]),
  data.table(outcome = "Headed a viable ticket in the following election", est = coef(h7_heads_viable)[["rm_female:post"]], se = se(h7_heads_viable)[["rm_female:post"]]),
  data.table(outcome = "Ran for municipal office in the following election", est = coef(h7_runs)[["rm_female:post"]], se = se(h7_runs)[["rm_female:post"]]),
  data.table(outcome = "Won municipal office in the following election", est = coef(h7_elected)[["rm_female:post"]], se = se(h7_elected)[["rm_female:post"]]),
  data.table(outcome = "Headed a ticket in the following\nelection, elected vice-mayors only", est = coef(h7_heads_won)[["rm_female:post"]], se = se(h7_heads_won)[["rm_female:post"]])
)
careers[, outcome := factor(outcome, levels = rev(outcome))]
print(careers[, .(outcome, pp = 100 * est, p = 2 * pnorm(-abs(est / se)))])
figS14 <- ggplot(careers, aes(100 * est, outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Post-earmark change, female vs. male running mates (percentage points)", y = NULL)
figure_S14 <- figS14
if (interactive()) print(figure_S14)
ggsave("figure_S14.png", figure_S14, width = 6.3, height = 3.9, dpi = 300, device = agg_png, bg = "white")

#figureS15
h7_prior_es <- feols(prior_elected ~ rm_female + i(year, rm_female, ref = 2016) | muni^year + party_head^year, running_mates, cluster = ~muni)
h7_heads_es <- feols(heads_next ~ rm_female + i(year, rm_female, ref = 2016) | muni^year + party_head^year, running_mates[year <= 2020], cluster = ~muni)
print(wald(h7_prior_es, keep = "^year::(2008|2012):rm_female$"))
print(wald(h7_heads_es, keep = "^year::(2008|2012):rm_female$"))
event_studies <- data.table()
for (spec in c("h1", "h2", "h3", "h4a", "h4b", "h5", "h7a", "h7b")) {
  model <- switch(spec, h1 = h1_es, h2 = h2_muni, h3 = h3_ppml_es, h4a = h4a_comp_es, h4b = h4b_comp_es, h5 = h5_ppml_es, h7a = h7_prior_es, h7b = h7_heads_es)
  pattern <- switch(spec, h1 = "^year::(\\d{4}):rm_slot$", h2 = "^year::(\\d{4})$", h3 = "^year::(\\d{4}):HM$", h4a = "^year::(\\d{4}):viable$",
                    h4b = "^year::(\\d{4}):endow_statutory$", h5 = "^year::(\\d{4}):FH$", h7a = "^year::(\\d{4}):rm_female$", h7b = "^year::(\\d{4}):rm_female$")
  panel <- switch(spec, h1 = "(a) Running-mate vs. head slot (pp)", h2 = "(b) Replacement by a female running mate (pp)",
                  h3 = "(c) Party funds, mixed vs. all-male (%)", h4a = "(d) Female running mate, viable tickets (pp)",
                  h4b = "(e) Female running mate, per log statutory endowment (pp)", h5 = "(f) Party funds, female-headed vs. all-male (%)",
                  h7a = "(g) Running mates previously elected (pp)", h7b = "(h) Heading a ticket in the following election (pp)")
  ct <- as.data.table(coeftable(model), keep.rownames = "term")[grepl(pattern, term)]
  ct[, year := as.integer(sub(pattern, "\\1", term))]
  event_studies <- rbind(event_studies, rbind(ct[, .(year, est = Estimate, se = `Std. Error`)], data.table(year = 2016L, est = 0, se = 0))[, panel := panel])
}
event_studies[, `:=`(low = est - 1.96 * se, high = est + 1.96 * se)]
event_studies[grepl("\\(pp\\)", panel), `:=`(est = 100 * est, low = 100 * low, high = 100 * high)]
event_studies[grepl("\\(%\\)", panel), `:=`(est = 100 * (exp(est) - 1), low = 100 * (exp(low) - 1), high = 100 * (exp(high) - 1))]
print(event_studies)
figS15 <- ggplot(event_studies, aes(year, est)) + facet_wrap(~panel, scales = "free_y", ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.9) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) + labs(x = NULL, y = "Estimate relative to 2016") +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))
figure_S15 <- figS15
if (interactive()) print(figure_S15)
ggsave("figure_S15.png", figure_S15, width = 6.3, height = 8, dpi = 300, device = agg_png, bg = "white")

#figureS16
for (fake in c(2012L, 2016L)) {
  data_fake <- composition[year <= 2016]
  data_fake[, fake_post := as.integer(year >= fake)]
  m <- feols(rm_female ~ viable + viable:fake_post + experienced_head + coalition_size | muni^year + party_head^year, data_fake, cluster = ~muni)
  placebo <- rbind(placebo, data.table(test = paste0("Viable tickets, placebo ", fake), unit = "pp", est = coef(m)[["viable:fake_post"]], se = se(m)[["viable:fake_post"]]))
  m <- feols(as.formula(paste("rm_female ~ endow_statutory:fake_post +", controls, "| muni^year + party_head")), data_fake[!is.na(endow_statutory)], cluster = ~muni + party_head)
  placebo <- rbind(placebo, data.table(test = paste0("Statutory FEFC endowment, placebo ", fake), unit = "pp", est = coef(m)[["endow_statutory:fake_post"]], se = se(m)[["endow_statutory:fake_post"]]))
  data_fake <- running_mates[year <= 2016]
  data_fake[, fake_post := as.integer(year >= fake)]
  m <- feols(prior_elected ~ rm_female + rm_female:fake_post | muni^year + party_head^year, data_fake, cluster = ~muni)
  placebo <- rbind(placebo, data.table(test = paste0("Running mates previously elected, placebo ", fake), unit = "pp", est = coef(m)[["rm_female:fake_post"]], se = se(m)[["rm_female:fake_post"]]))
  m <- feols(heads_next ~ rm_female + rm_female:fake_post | muni^year + party_head^year, data_fake, cluster = ~muni)
  placebo <- rbind(placebo, data.table(test = paste0("Heading a ticket in the following election, placebo ", fake), unit = "pp", est = coef(m)[["rm_female:fake_post"]], se = se(m)[["rm_female:fake_post"]]))
}
data_fake <- finance_male[year <= 2016]
data_fake[, fake_post := as.integer(year == 2016)]
m <- fepois(as.formula(paste("party_funds ~ HM + HM:fake_post +", controls, "| muni^year + party_head^year")), data_fake, cluster = ~muni)
placebo <- rbind(placebo, data.table(test = "Mixed-ticket premium, placebo 2016", unit = "log", est = coef(m)[["HM:fake_post"]], se = se(m)[["HM:fake_post"]]))
data_fake <- finance[year <= 2016]
data_fake[, fake_post := as.integer(year == 2016)]
m <- fepois(as.formula(paste("party_funds ~ HM + FH + HM:fake_post + FH:fake_post +", controls, "| muni^year + party_head^year")), data_fake, cluster = ~muni)
placebo <- rbind(placebo, data.table(test = "Advantage of female-headed tickets, placebo 2016", unit = "log", est = coef(m)[["FH:fake_post"]], se = se(m)[["FH:fake_post"]]))
placebo[, test := factor(test, levels = rev(test))]
print(placebo[, .(test, estimate = fifelse(unit == "log", 100 * (exp(est) - 1), 100 * est), unit = fifelse(unit == "log", "percent", "pp"), p = 2 * pnorm(-abs(est / se)))])
figS16a <- ggplot(placebo[unit == "pp"], aes(100 * est, test)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Placebo estimate (percentage points)", y = NULL)
figS16b <- ggplot(placebo[unit == "log"], aes(100 * (exp(est) - 1), test)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Placebo estimate (%)", y = NULL)
figure_S16 <- figS16a/figS16b + plot_layout(heights = c(4, 1))
if (interactive()) print(figure_S16)
ggsave("figure_S16.png", figure_S16, width = 6.3, height = 5.2, dpi = 300, device = agg_png, bg = "white")

#figureS17
balance_means <- finance_male[, .(`Head is incumbent mayor` = mean(incumbent_head), `Head's party holds mayoralty` = mean(incumbent_party),
                                  `Head previously elected` = mean(experienced_head), `Running mate previously elected` = mean(experienced_rm, na.rm = TRUE)),
                              by = .(year, type = fifelse(HM == 1, "Mixed tickets", "All-male tickets"))]
balance_means <- melt(balance_means, id.vars = c("year", "type"), variable.name = "covariate", value.name = "mean")
balance_dd <- data.table()
for (covariate in c("incumbent_head", "incumbent_party", "viable", "experienced_head", "experienced_rm", "coalition_size")) {
  m <- feols(as.formula(paste(covariate, "~ HM + HM:post | muni^year + party_head^year")), finance_male, cluster = ~muni)
  balance_dd <- rbind(balance_dd, data.table(covariate = covariate, est = coef(m)[["HM:post"]], se = se(m)[["HM:post"]]))
}
balance_dd[, covariate := factor(covariate, levels = rev(c("incumbent_head", "incumbent_party", "viable", "experienced_head", "experienced_rm", "coalition_size")),
                                 labels = rev(c("Head is incumbent mayor", "Head's party holds mayoralty", "Viable ticket", "Head previously elected",
                                                "Running mate previously elected", "Parties in coalition")))]
print(balance_dd[, .(covariate, est, p = 2 * pnorm(-abs(est / se)))])
figS17a <- ggplot(balance_means, aes(year, 100 * mean, linetype = type, shape = type)) + facet_wrap(~covariate, ncol = 2) +
  geom_vline(xintercept = 2018, linetype = "dotted", colour = "grey35") +
  geom_line(linewidth = 0.6) + geom_point(size = 1.8, fill = "white") +
  scale_shape_manual(values = c(21, 16)) + scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024)) + labs(x = NULL, y = "Share of tickets (%)") +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))
figS17b <- ggplot(balance_dd, aes(est, covariate)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Post-earmark change, mixed vs. all-male tickets\n(proportion; number of parties for coalition size)", y = NULL)
figure_S17 <- figS17a/figS17b + plot_layout(heights = c(1.4, 1))
if (interactive()) print(figure_S17)
ggsave("figure_S17.png", figure_S17, width = 6.3, height = 7, dpi = 300, device = agg_png, bg = "white")

#figureS18
clustering <- data.table()
for (cl in c("muni", "state")) {
  ct <- coeftable(h1_dd, vcov = as.formula(paste0("~", cl)))
  clustering <- rbind(clustering, data.table(hypothesis = "H1 running-mate vs. head slot (pp)", cluster = cl, est = 100 * ct["rm_slot:post", 1], se = 100 * ct["rm_slot:post", 2]))
}
h2_weights <- setNames(rep(0, length(coef(h2_muni))), names(coef(h2_muni)))
h2_weights[c("year::2008", "year::2012")] <- -1 / 3
h2_weights["year::2020"] <- 1
for (cl in c("muni", "state", "party_head")) {
  h2_variance <- vcov(h2_muni, vcov = as.formula(paste0("~", cl)))
  clustering <- rbind(clustering, data.table(hypothesis = "H2 replacement by a female running mate (pp)", cluster = cl,
    est = 100 * sum(h2_weights * coef(h2_muni)), se = 100 * sqrt(drop(t(h2_weights) %*% h2_variance %*% h2_weights))))
}
for (cl in c("muni", "state", "party_head", "muni + party_head")) {
  ct <- coeftable(h3_ppml, vcov = as.formula(paste0("~", cl)))
  clustering <- rbind(clustering, data.table(hypothesis = "H3 mixed-ticket premium (log points)", cluster = cl, est = ct["HM:post", 1], se = ct["HM:post", 2]))
  ct <- coeftable(h5_ppml, vcov = as.formula(paste0("~", cl)))
  clustering <- rbind(clustering, data.table(hypothesis = "H5a advantage of female-headed tickets (log points)", cluster = cl, est = ct["FH:post", 1], se = ct["FH:post", 2]))
  ct <- coeftable(h7_heads, vcov = as.formula(paste0("~", cl)))
  clustering <- rbind(clustering, data.table(hypothesis = "H7 heading a ticket in the following election (pp)", cluster = cl, est = 100 * ct["rm_female:post", 1], se = 100 * ct["rm_female:post", 2]))
}
for (cl in c("party", "state", "party + state")) {
  ct <- coeftable(h6_dd, vcov = as.formula(paste0("~", cl)))
  clustering <- rbind(clustering, data.table(hypothesis = "H6 difference-in-differences (share)", cluster = cl, est = ct["exposure_post", 1], se = ct["exposure_post", 2]))
}
clustering[, cluster := factor(cluster, levels = c("muni", "state", "party_head", "muni + party_head", "party", "party + state"),
                               labels = c("Municipality", "State", "Party", "Municipality and party", "Party ", "Party and state"))]
clustering[, p := 2 * pnorm(-abs(est / se))]
print(clustering)
figS18 <- ggplot(clustering, aes(est, cluster)) + facet_wrap(~hypothesis, scales = "free", ncol = 2, labeller = label_wrap_gen(width = 30)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "Estimate with 95 percent confidence interval", y = "Clustering of standard errors") +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))
figure_S18 <- figS18
if (interactive()) print(figure_S18)
ggsave("figure_S18.png", figure_S18, width = 6.3, height = 6.2, dpi = 300, device = agg_png, bg = "white")

#tableS23
print(bootstrap[match(c("H1 gubernatorial tickets, 2018 and 2022", "H1 gubernatorial tickets, 2002-2018 sample", "H1 gubernatorial tickets, 2018 coefficient of the event study",
                  "H1 Senate alternates, 2018 and 2022", "H4b female running mate, per log point of statutory endowment", "Rival hypothesis, log party size x post",
                  "Rival hypothesis, left-leaning party x post", "Rival hypothesis, viable ticket x post", "H6 difference-in-differences",
                  "H6 cross-section, mixed minus all-male tickets", "H3 funds from the running mate's party, clustered by that party"), case)])
print(tstat(h6_dd)[["exposure_post"]])

#tableS24
print(score)

#figureS19
print(coef(h3_ppml)[["HM:post"]])
figS19 <- ggplot(data.table(estimate = permutations), aes(estimate)) +
  geom_histogram(bins = 40, fill = "grey75", colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = coef(h3_ppml)[["HM:post"]], linewidth = 0.8) +
  geom_vline(xintercept = -coef(h3_ppml)[["HM:post"]], linewidth = 0.4, linetype = "dashed") +
  labs(x = "Mixed-ticket premium under random reassignment (log points)", y = "Number of permutations")
figure_S19 <- figS19
if (interactive()) print(figure_S19)
ggsave("figure_S19.png", figure_S19, width = 6, height = 3.2, dpi = 300, device = agg_png, bg = "white")

#figureS20
figS20 <- ggplot(honest_plot, aes(factor(Mbar), 100 * (exp((lb + ub) / 2) - 1))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(ymin = 100 * (exp(lb) - 1), ymax = 100 * (exp(ub) - 1)), width = 0.2) +
  labs(x = "Largest post-earmark violation of common trends allowed,\nrelative to the largest pre-earmark violation (0 = common trends)",
       y = "Average mixed-ticket premium,\n2020 and 2024 (%)")
figure_S20 <- figS20
if (interactive()) print(figure_S20)
ggsave("figure_S20.png", figure_S20, width = 6, height = 3.4, dpi = 300, device = agg_png, bg = "white")

#figureS21
finance_male[, any_party := as.integer(party_funds > 0)]
finance_male[, ihs_party := asinh(party_funds)]
finance_male[, ln_party_thousands := log1p(party_funds / 1000)]
finance_male[, ln_party_cents := log1p(100 * party_funds)]
finance[, any_party := as.integer(party_funds > 0)]
finance[, ihs_party := asinh(party_funds)]
outcomes <- data.table(hypothesis = "Mixed tickets (H3)", outcome = "ppml", est = coef(h3_ppml)[["HM:post"]], se = se(h3_ppml)[["HM:post"]])
outcomes <- rbind(outcomes, data.table(hypothesis = "Mixed tickets (H3)", outcome = "ppml_cash", est = coef(h3_ppml_cash)[["HM:post"]], se = se(h3_ppml_cash)[["HM:post"]]))
outcomes <- rbind(outcomes, data.table(hypothesis = "Mixed tickets (H3)", outcome = "ppml_public", est = coef(h3_ppml_public)[["HM:post"]], se = se(h3_ppml_public)[["HM:post"]]))
for (y in c("ln_party", "ihs_party", "ln_party_thousands", "ln_party_cents", "any_party", "share_party")) {
  m <- feols(as.formula(paste(y, "~ HM + HM:post +", controls, "| muni^year + party_head^year")), finance_male, cluster = ~muni)
  outcomes <- rbind(outcomes, data.table(hypothesis = "Mixed tickets (H3)", outcome = y, est = coef(m)[["HM:post"]], se = se(m)[["HM:post"]]))
}
outcomes <- rbind(outcomes, data.table(hypothesis = "Female-headed tickets (H5a)", outcome = "ppml", est = coef(h5_ppml)[["FH:post"]], se = se(h5_ppml)[["FH:post"]]))
for (y in c("ln_party", "ihs_party", "any_party", "share_party")) {
  m <- feols(as.formula(paste(y, "~ HM + FH + HM:post + FH:post +", controls, "| muni^year + party_head^year")), finance, cluster = ~muni)
  outcomes <- rbind(outcomes, data.table(hypothesis = "Female-headed tickets (H5a)", outcome = y, est = coef(m)[["FH:post"]], se = se(m)[["FH:post"]]))
}
outcomes[, label := factor(outcome, levels = rev(c("ppml", "ppml_cash", "ppml_public", "ln_party", "ihs_party", "ln_party_thousands", "ln_party_cents", "any_party", "share_party")),
                           labels = rev(c("Party funds in levels (Poisson), baseline", "Cash transfers only (Poisson)", "Public funds (Poisson)",
                                          "Log(1 + party funds)", "Inverse hyperbolic sine", "Log(1 + party funds in thousands)", "Log(1 + party funds in cents)",
                                          "Any party funds (percentage points)", "Party share of receipts (percentage points)")))]
outcomes[, pct := !outcome %in% c("any_party", "share_party")]
print(outcomes[, .(hypothesis, outcome, estimate = fifelse(pct, 100 * (exp(est) - 1), 100 * est), p = 2 * pnorm(-abs(est / se)))])
figS21a <- ggplot(outcomes[pct == TRUE], aes(100 * (exp(est) - 1), label, shape = hypothesis)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.2, orientation = "y", position = position_dodge(0.5)) +
  geom_point(size = 2, fill = "white", position = position_dodge(0.5)) + scale_shape_manual(values = c(24, 16)) +
  labs(x = "Exponentiated estimate: 100 x [exp(b) - 1]", y = NULL)
figS21b <- ggplot(outcomes[pct == FALSE], aes(100 * est, label, shape = hypothesis)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y", position = position_dodge(0.5)) +
  geom_point(size = 2, fill = "white", position = position_dodge(0.5)) + scale_shape_manual(values = c(24, 16)) +
  labs(x = "Post-earmark change relative to all-male tickets\n(percentage points)", y = NULL)
figure_S21 <- figS21a/figS21b + plot_layout(heights = c(3.2, 1), guides = "collect") & theme(legend.position = "bottom") & guides(shape = guide_legend(nrow = 2))
if (interactive()) print(figure_S21)
ggsave("figure_S21.png", figure_S21, width = 6.3, height = 5.4, dpi = 300, device = agg_png, bg = "white")

#figureS22
loo_states <- data.table()
for (uf in sort(unique(slots$state))) {
  m <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots[state != uf], cluster = ~muni)
  loo_states <- rbind(loo_states, data.table(excluded = uf, est = 100 * coef(m)[["rm_slot:post"]], se = 100 * se(m)[["rm_slot:post"]]))
}
loo_cells <- data.table()
for (pp in sort(unique(h6_cells$party))) {
  m <- feols(women_share ~ exposure_post + share_female_council_cands | state^party + party^year + state^year, h6_cells[party != pp], weights = ~total, cluster = ~party)
  loo_cells <- rbind(loo_cells, data.table(excluded = pp, est = coef(m)[["exposure_post"]], se = se(m)[["exposure_post"]]))
}
loo_parties[, excluded := fcase(excluded == "UNIAO", "UNIÃO", excluded == "PC DO B", "PC do B", default = excluded)]
loo_cells[, excluded := fcase(excluded == "UNIAO", "UNIÃO", excluded == "PC DO B", "PC do B", default = excluded)]
loo_states[, excluded := factor(excluded, levels = excluded[order(est)])]
loo_parties[, excluded := factor(excluded, levels = excluded[order(est)])]
loo_cells[, excluded := factor(excluded, levels = excluded[order(est)])]
print(loo_states[order(est), .(excluded, pp = est, p = 2 * pnorm(-abs(est / se)))])
print(loo_parties[order(est), .(excluded, percent = 100 * (exp(est) - 1), p = 2 * pnorm(-abs(est / se)))])
print(loo_cells[order(est), .(excluded, est, p = 2 * pnorm(-abs(est / se)))])
figS22a <- ggplot(loo_states, aes(est, excluded)) + geom_vline(xintercept = 100 * coef(h1_dd)[["rm_slot:post"]], linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), width = 0.3, orientation = "y") + geom_point(size = 1.4) +
  labs(x = "H1: running-mate vs.\nhead slot (pp)", y = "State excluded") + theme(axis.text.y = element_text(size = 6.5))
figS22b <- ggplot(loo_parties, aes(100 * (exp(est) - 1), excluded)) + geom_vline(xintercept = 100 * (exp(coef(h3_ppml)[["HM:post"]]) - 1), linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 0, colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.3, orientation = "y") + geom_point(size = 1.4) +
  labs(x = "H3: mixed-ticket\npremium (%)", y = "Party excluded") + theme(axis.text.y = element_text(size = 6.5))
figS22c <- ggplot(loo_cells, aes(100 * est, excluded)) + geom_vline(xintercept = 100 * coef(h6_dd)[["exposure_post"]], linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 0, colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.3, orientation = "y") + geom_point(size = 1.4) +
  labs(x = "H6: difference-in-differences\n(pp per unit of exposure)", y = "Party excluded") + theme(axis.text.y = element_text(size = 6))
figure_S22 <- figS22a + figS22b + figS22c
if (interactive()) print(figure_S22)
ggsave("figure_S22.png", figure_S22, width = 6.8, height = 5.6, dpi = 300, device = agg_png, bg = "white")

#figureS23
samples <- rbind(samples,
                 data.table(hypothesis = "H1", sample = "All elections and municipalities", est = coef(h1_dd)[["rm_slot:post"]], se = se(h1_dd)[["rm_slot:post"]]),
                 data.table(hypothesis = "H3", sample = "All elections and municipalities", est = coef(h3_ppml)[["HM:post"]], se = se(h3_ppml)[["HM:post"]]))
m <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots[year >= 2004], cluster = ~muni)
samples <- rbind(samples, data.table(hypothesis = "H1", sample = "Excluding 2000", est = coef(m)[["rm_slot:post"]], se = se(m)[["rm_slot:post"]]))
m <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots[runoff_muni == 0], cluster = ~muni)
samples <- rbind(samples, data.table(hypothesis = "H1", sample = "Excluding municipalities with runoffs", est = coef(m)[["rm_slot:post"]], se = se(m)[["rm_slot:post"]]))
m <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots[capital == 0], cluster = ~muni)
samples <- rbind(samples, data.table(hypothesis = "H1", sample = "Excluding state capitals", est = coef(m)[["rm_slot:post"]], se = se(m)[["rm_slot:post"]]))
for (rg in c("North", "Northeast", "Center-West", "Southeast", "South")) {
  m <- feols(female ~ rm_slot + rm_slot:post | muni^year, slots[region == rg], cluster = ~muni)
  samples <- rbind(samples, data.table(hypothesis = "H1", sample = paste("Region:", rg), est = coef(m)[["rm_slot:post"]], se = se(m)[["rm_slot:post"]]))
}
samples[, sample := factor(sample, levels = rev(c("All elections and municipalities", "Excluding 2000", "Excluding municipalities with runoffs", "Excluding state capitals",
                                                   paste("Region:", c("North", "Northeast", "Center-West", "Southeast", "South")))))]
print(samples[order(hypothesis, -as.integer(sample)), .(hypothesis, sample, estimate = fifelse(hypothesis == "H1", 100 * est, 100 * (exp(est) - 1)), p = 2 * pnorm(-abs(est / se)))])
figS23a <- ggplot(samples[hypothesis == "H1"], aes(100 * est, sample)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (est - 1.96 * se), xmax = 100 * (est + 1.96 * se)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "H1: running-mate vs. head slot under the earmark (pp)", y = NULL)
figS23b <- ggplot(samples[hypothesis == "H3"], aes(100 * (exp(est) - 1), sample)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = 100 * (exp(est - 1.96 * se) - 1), xmax = 100 * (exp(est + 1.96 * se) - 1)), width = 0.2, orientation = "y") + geom_point(size = 2) +
  labs(x = "H3: mixed-ticket premium under the earmark (%)", y = NULL)
figure_S23 <- figS23a/figS23b
if (interactive()) print(figure_S23)
ggsave("figure_S23.png", figure_S23, width = 6.3, height = 6, dpi = 300, device = agg_png, bg = "white")

#figureS24
floors <- rbind(accounting[, .(rule = "Reconstructed floor\n(proportional)", year, status)], accounting[, .(rule = "Flat 30 percent floor", year, status = status_flat)],
                accounting[, .(rule = "Reconstructed floor, without\neach party's largest mixed ticket", year, status = status_without_top)],
                accounting[, .(rule = "Reconstructed floor, without\nmixed tickets in state capitals", year, status = status_without_capitals)])
floors <- floors[, .N, by = .(rule, year, status)]
floors[, status := factor(status, levels = c("fails", "only_with_mixed", "own"),
                          labels = c("Would not reach the floor\neven counting mixed tickets", "Would reach the floor only\nif mixed tickets counted", "Would reach the floor through\nwomen's own candidacies"))]
floors[, rule := factor(rule, levels = c("Reconstructed floor\n(proportional)", "Flat 30 percent floor", "Reconstructed floor, without\neach party's largest mixed ticket",
                                         "Reconstructed floor, without\nmixed tickets in state capitals"))]
print(floors[order(rule, year, status)])
figS24 <- ggplot(floors, aes(factor(year), N, fill = status)) + facet_wrap(~rule, nrow = 1) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = N, colour = status), position = position_stack(vjust = 0.5), size = 3, family = "Times New Roman", show.legend = FALSE) +
  scale_fill_manual(values = c("black", "grey55", "white"), drop = FALSE) + scale_colour_manual(values = c("white", "white", "black"), drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(x = NULL, y = "Number of parties") + theme(strip.background = element_blank(), strip.text = element_text(size = 7.5))
figure_S24 <- figS24
if (interactive()) print(figure_S24)
ggsave("figure_S24.png", figure_S24, width = 6.8, height = 3.8, dpi = 300, device = agg_png, bg = "white")

#tableS25
primary <- data.table(hypothesis = c("H1", "H3", "H6"),
                      p = c(pvalue(h1_dd)[["rm_slot:post"]], pvalue(h3_ppml)[["HM:post"]], pvalue(h6_dd)[["exposure_post"]]))
print(primary[, p_holm := p.adjust(p, method = "holm")][])
all_tests <- data.table(hypothesis = c("H1", "H2", "H3", "H4a", "H4b", "H5a", "H5b", "H6", "H7"),
                        p = c(pvalue(h1_dd)[["rm_slot:post"]], h2_contrasts[model == "h2_muni" & transition == "year::2020", p], pvalue(h3_ppml)[["HM:post"]],
                              pvalue(h4a_comp)[["viable:post"]], pvalue(h4b_comp)[["endow_statutory:post"]], pvalue(h5_ppml)[["FH:post"]],
                              pvalue(h5_ppml_incumbent)[["FH:post:incumbent_head"]], pvalue(h6_dd)[["exposure_post"]], pvalue(h7_prior)[["rm_female:post"]]))
print(all_tests[, p_holm := p.adjust(p, method = "holm")][])

#tableS26
verdicts <- rbind(
  data.table(hypothesis = "H1", test = "Running-mate vs. head slot, 2020-2024 vs. 2000-2016", unit = "pp", est = coef(h1_dd)[["rm_slot:post"]], se = se(h1_dd)[["rm_slot:post"]],
             p = pvalue(h1_dd)[["rm_slot:post"]], p_holm = all_tests[hypothesis == "H1", p_holm], diagnostic = "Joint test of the leads",
             diagnostic_p = wald(h1_es, keep = "^year::(2000|2004|2008|2012):rm_slot$", print = FALSE)$p),
  data.table(hypothesis = "H2", test = "Replacement of male running mates by women, 2016-2020 vs. earlier transitions", unit = "pp",
             est = h2_contrasts[model == "h2_muni" & transition == "year::2020", est], se = h2_contrasts[model == "h2_muni" & transition == "year::2020", se],
             p = h2_contrasts[model == "h2_muni" & transition == "year::2020", p], p_holm = all_tests[hypothesis == "H2", p_holm],
             diagnostic = "Joint placebo test, 2004-2008 and 2008-2012 transitions", diagnostic_p = wald(h2_muni, keep = "^year::(2008|2012)$", print = FALSE)$p),
  data.table(hypothesis = "H3", test = "Mixed-ticket premium in party funds", unit = "log", est = coef(h3_ppml)[["HM:post"]], se = se(h3_ppml)[["HM:post"]],
             p = pvalue(h3_ppml)[["HM:post"]], p_holm = all_tests[hypothesis == "H3", p_holm], diagnostic = "Score bootstrap with parties as clusters",
             diagnostic_p = score[sample == "2012-2024" & cluster == "party_head", p_bootstrap]),
  data.table(hypothesis = "H3", test = "Funds from the running mate's party, allied tickets", unit = "pp", est = coef(cross_party_twoway)[["HM:post"]], se = se(cross_party_twoway)[["HM:post"]],
             p = pvalue(cross_party_twoway)[["HM:post"]], p_holm = NA_real_, diagnostic = "Wild cluster bootstrap by the running mate's party",
             diagnostic_p = bootstrap[case == "H3 funds from the running mate's party, clustered by that party", p_bootstrap]),
  data.table(hypothesis = "H4a", test = "Female running mate, viable vs. nonviable tickets", unit = "pp", est = coef(h4a_comp)[["viable:post"]], se = se(h4a_comp)[["viable:post"]],
             p = pvalue(h4a_comp)[["viable:post"]], p_holm = all_tests[hypothesis == "H4a", p_holm], diagnostic = "Joint test of the leads",
             diagnostic_p = wald(h4a_comp_es, keep = "^year::(2008|2012):viable$", print = FALSE)$p),
  data.table(hypothesis = "H4b", test = "Female running mate, per log statutory FEFC endowment", unit = "pp", est = coef(h4b_comp)[["endow_statutory:post"]], se = se(h4b_comp)[["endow_statutory:post"]],
             p = pvalue(h4b_comp)[["endow_statutory:post"]], p_holm = all_tests[hypothesis == "H4b", p_holm], diagnostic = "Wild cluster bootstrap by party",
             diagnostic_p = bootstrap[case == "H4b female running mate, per log point of statutory endowment", p_bootstrap]),
  data.table(hypothesis = "Rival", test = "Left-leaning party before the earmark", unit = "pp", est = coef(rival_pre)[["left"]], se = se(rival_pre)[["left"]],
             p = pvalue(rival_pre)[["left"]], p_holm = NA_real_, diagnostic = "Change after the earmark, wild cluster bootstrap",
             diagnostic_p = bootstrap[case == "Rival hypothesis, left-leaning party x post", p_bootstrap]),
  data.table(hypothesis = "H5a", test = "Advantage of female-headed tickets", unit = "log", est = coef(h5_ppml)[["FH:post"]], se = se(h5_ppml)[["FH:post"]],
             p = pvalue(h5_ppml)[["FH:post"]], p_holm = all_tests[hypothesis == "H5a", p_holm], diagnostic = "Estimate with 2016 as the only pre-earmark election",
             diagnostic_p = pvalue(h5_ppml_2016)[["FH:post"]]),
  data.table(hypothesis = "H5b", test = "Additional gain of incumbent female heads", unit = "log", est = coef(h5_ppml_incumbent)[["FH:post:incumbent_head"]],
             se = se(h5_ppml_incumbent)[["FH:post:incumbent_head"]], p = pvalue(h5_ppml_incumbent)[["FH:post:incumbent_head"]], p_holm = all_tests[hypothesis == "H5b", p_holm],
             diagnostic = NA_character_, diagnostic_p = NA_real_),
  data.table(hypothesis = "H6", test = "Share reaching women's own candidacies, per unit of 2016 exposure", unit = "share", est = coef(h6_dd)[["exposure_post"]], se = se(h6_dd)[["exposure_post"]],
             p = pvalue(h6_dd)[["exposure_post"]], p_holm = all_tests[hypothesis == "H6", p_holm], diagnostic = "Wild cluster bootstrap by party",
             diagnostic_p = bootstrap[case == "H6 difference-in-differences", p_bootstrap]),
  data.table(hypothesis = "H7", test = "Female running mates previously elected", unit = "pp", est = coef(h7_prior)[["rm_female:post"]], se = se(h7_prior)[["rm_female:post"]],
             p = pvalue(h7_prior)[["rm_female:post"]], p_holm = all_tests[hypothesis == "H7", p_holm], diagnostic = "Joint test of the leads",
             diagnostic_p = wald(h7_prior_es, keep = "^year::(2008|2012):rm_female$", print = FALSE)$p),
  data.table(hypothesis = "H7", test = "Female running mates heading a ticket in the following election", unit = "pp", est = coef(h7_heads)[["rm_female:post"]], se = se(h7_heads)[["rm_female:post"]],
             p = pvalue(h7_heads)[["rm_female:post"]], p_holm = NA_real_, diagnostic = "Joint test of the leads",
             diagnostic_p = wald(h7_heads_es, keep = "^year::(2008|2012):rm_female$", print = FALSE)$p)
)
verdicts[, `:=`(low = est - 1.96 * se, high = est + 1.96 * se)]
verdicts[unit == "log", `:=`(est = 100 * (exp(est) - 1), low = 100 * (exp(low) - 1), high = 100 * (exp(high) - 1))]
verdicts[unit == "pp", `:=`(est = 100 * est, low = 100 * low, high = 100 * high)]
verdicts[unit == "log", unit := "percent"]
print(verdicts[, .(hypothesis, test, unit, est, low, high, p, p_holm, diagnostic, diagnostic_p)])
