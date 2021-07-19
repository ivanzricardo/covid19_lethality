gen internacao = 1
bysort cnes semana_int: egen internacao_semana =  total(internacao)

gen óbito = 0
replace óbito = 1 if morte == "Sim"


bysort cnes semana_int: egen obitos_semana = total(óbito)

gen letalidade_hosp = obitos_semana/internacao_semana

tostring diagsec8 diagsec9, replace
foreach j in diag_princ diagsec1 diagsec2 diagsec3 diagsec4 diagsec5 diagsec6 diagsec7 diagsec8 diagsec9 {
gen `j'_numerica = . 
replace `j'_numerica = 1 if `j' <="B99"
replace `j'_numerica = 2 if `j' >"B99" &  `j' <"D49"
replace `j'_numerica = 3 if `j' >"D49" &  `j' <"D90"
replace `j'_numerica = 4 if `j' >="E00" &  `j' <"E91"
replace `j'_numerica = 5 if `j' >="F00" &  `j' <"G"
replace `j'_numerica = 6 if `j' >="G00" &  `j' <"H"
replace `j'_numerica = 7 if `j' >="H00" &  `j' <"H60"
replace `j'_numerica = 8 if `j' >="H60" &  `j' <"H96"
replace `j'_numerica = 9 if `j' >="I00" &  `j' <"J"
replace `j'_numerica = 10 if `j' >="J00" &  `j' <"K"
replace `j'_numerica = 11 if `j' >="K00" &  `j' <"K94"
replace `j'_numerica = 12 if `j' >="L00" &  `j' <"M"
replace `j'_numerica = 13 if `j' >="M00" &  `j' <"N"
replace `j'_numerica = 14 if `j' >="N00" &  `j' <"O"
replace `j'_numerica = 15 if `j' >="O00" &  `j' <"P"
replace `j'_numerica = 16 if `j' >="P00" &  `j' <"P97"
replace `j'_numerica = 17 if `j' >="Q00" &  `j' <"R"
replace `j'_numerica = 18 if `j' >="R00" &  `j' <"S"
replace `j'_numerica = 19 if `j' >="S00" &  `j' <"T99"
replace `j'_numerica = 20 if `j' >="V01" &  `j' <"Y99"
replace `j'_numerica = 21 if `j' >="Z00" &  `j' <="Z999"
replace `j'_numerica = 99 if `j' >= "U" & `j' <"V"
replace `j'_numerica = 0 if `j' == "NA"
}


forvalues j = 1(1)21 {
gen capitulo_CID_`j' = 0
replace capitulo_CID_`j' = 1 if diag_princ_numerica == `j' | diagsec1_numerica == `j' | diagsec2_numerica == `j' | diagsec3_numerica == `j' | diagsec4_numerica == `j' | diagsec5_numerica == `j' | diagsec6_numerica == `j' | diagsec7_numerica == `j' | diagsec8_numerica == `j' | diagsec9_numerica == `j' 
}

gen capitulo_CID_U = 0
replace capitulo_CID_U = 1 if diag_princ_numerica == 99 | diagsec1_numerica == 99 | diagsec2_numerica == 99 | diagsec3_numerica == 99 | diagsec4_numerica == 99 | diagsec5_numerica == 99 | diagsec6_numerica == 99 | diagsec7_numerica == 99 | diagsec8_numerica == 99 | diagsec9_numerica == 99 

foreach j in diag_princ diagsec1 diagsec2 diagsec3 diagsec4 diagsec5 diagsec6 diagsec7 diagsec8 diagsec9 {
replace capitulo_CID_1 = 0 if `j' == "B342"
replace capitulo_CID_U = 0 if `j' == "U071" | `j' == "U072"
}

gen COVID_diag_principal = 1 if diag_princ == "U071" | diag_princ == "U072" | diag_princ == "B342" 
recode COVID_diag_principal (.=0)

gen gênero_feminino = 0
replace gênero_feminino = 1 if sexo == "Feminino"



gen até_19 = 0
replace até_19 = 1 if faixa_etaria == "00 a 19" 

gen entre_20_e_39 = 0 
replace entre_20_e_39 = 1 if faixa_etaria == "20 a 39" 

gen entre_40_e_59 = 0 
replace entre_40_e_59 = 1 if faixa_etaria == "40 a 59" 

gen entre_60_e_79 = 0 
replace entre_60_e_79 = 1 if faixa_etaria == "60 a 79" 

gen a_partir_de_80 = 0 
replace a_partir_de_80 = 1 if  faixa_etaria == "80 ou mais"

gen hosp_alta_complexidade = 0
replace hosp_alta_complexidade = 1 if complex == "Alta complexidade"


gen raca_numerico = 0
replace raca_numerico = 1 if raca_cor == "Parda"
replace raca_numerico = 2 if raca_cor == "Preta"
replace raca_numerico = 3 if raca_cor == "Amarela"
replace raca_numerico = 4 if raca_cor == "Indígena"
replace raca_numerico = 5 if raca_cor == "NA"


gen utilizou_UTI = 1
replace utilizou_UTI = 0 if marca_uti == "Não utilizou UTI"


tostring munic_res, replace
gen UF_res = substr(munic_res,1,2)
destring UF_res, replace
destring munic_res, replace


gen obesidade = 0
foreach j in diag_princ diagsec1  diagsec2 diagsec3 diagsec4 diagsec5 diagsec6 diagsec7 diagsec8 diagsec9 cid_notif {
replace obesidade = 1 if `j' == "E66" | `j' >= "E660" & `j' <= "E669"
}


gen zero = 0
tostring zero, replace
foreach j in diag_princ diagsec1  diagsec2 diagsec3 diagsec4 diagsec5 diagsec6 diagsec7 diagsec8 diagsec9 cid_notif {
gen len_`j' = length(`j')
egen `j'_4_digitos = concat(`j' zero) if len_`j' == 3
replace  `j'_4_digitos = `j' if  `j'_4_digitos == ""
drop len_`j' 
}

gen E90 = 0 
foreach j in diag_princ diagsec1  diagsec2 diagsec3 diagsec4 diagsec5 diagsec6 diagsec7 diagsec8 diagsec9 cid_notif {
replace E90 = 1 if `j'== "E90"
}

gen doenca_bacteriana = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace doenca_bacteriana = 1 if  `j' >= "A270" & `j' <= "A499"
}

gen cancer = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace cancer = 1 if `j' >= "C010" & `j' <= "C499" | `j' >= "D000" & `j' <= "D480"
}

gen diabetes = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace diabetes = 1 if  `j' >= "E100" & `j' <= "E149"
}

gen HAS = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace HAS = 1 if  `j' >= "I100" & `j' <= "I159"
}

gen insuficiencia_cardiaca = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace insuficiencia_cardiaca = 1 if  `j' >= "I500" & `j' <= "I509"
}

gen AVC = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace AVC = 1 if  `j' >= "I640" & `j' <= "I649"
}

gen doencas_isquemicas_cor = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace doencas_isquemicas_cor = 1 if  `j' >= "I200" & `j' <= "I259"
}

gen outras_doencas_cor = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace outras_doencas_cor = 1 if  `j' >= "I000" & `j' < "I100" | `j' > "I159" & `j' < "I200" | `j' > "I259" & `j' < "I500" | `j' > "I509" & `j' < "I640" | `j' > "I649" & `j' <= "I999"
}

gen insuf_renal = 0
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos{
replace insuf_renal = 1 if  `j' >= "N170" & `j' <= "N179"
}

gen R000_R099 = 0
label var R000_R099 "Sintomas e sinais relativos ao aparelho circulatório e respiratório"
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace R000_R099 = 1 if  `j' >= "R000" & `j' <= "R099"
}

gen asma = 0 
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace asma = 1 if  `j' >= "J450" & `j' <= "J459"
}

gen HIV = 0 
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace HIV = 1 if  `j' >= "B200" & `j' <= "B249"
}


gen J44 = 0
label var J44 "	Outras doenças pulmonares obstrutivas crônicas"
foreach j in diag_princ_4_digitos diagsec1_4_digitos diagsec2_4_digitos diagsec3_4_digitos diagsec4_4_digitos diagsec5_4_digitos diagsec6_4_digitos diagsec7_4_digitos diagsec8_4_digitos diagsec9_4_digitos cid_notif_4_digitos {
replace J44 = 1 if  `j' >= "J440" & `j' <= "J449"
}



*STCOX*
stset dias_perm, failure(óbito)



rename   munic_res cod_mun6
merge m:1 cod_mun6 using "base"
drop if _merge==2
rename cod_mun6 munic_res

gen Asma_J44 = 0
replace Asma_J44 = 1 if asma == 1 | J44 == 1 
rename Asma_J44 doencas_respiratorias

gen doencas_cardiacas = 0 
replace doencas_cardiacas = 1 if HAS ==1 |  insuficiencia_cardiaca == 1 | doencas_isquemicas_cor == 1 | outras_doencas_cor == 1

replace semana_int = "." if semana_int == "NA"
destring semana_int, replace

gen idade_num = 0
replace idade_num = 1 if entre_20_e_39 == 1
replace idade_num = 2 if entre_40_e_59 == 1
replace idade_num = 3 if entre_60_e_79 == 1
replace idade_num = 4 if a_partir_de_80 == 1

drop if ident == "Longa permanência"
gen mes = substr(dt_inter,6,2)

destring mes, replace


tabulate raca_numerico óbito if semana_int>9 & semana_int<41, chi2 

gen comorbidade_num = 0
replace comorbidade_num = 1 if obesidade == 1 | doenca_bacteriana == 1 | cancer==1 | diabetes == 1 | doencas_cardiacas == 1 | insuf_renal == 1 | R000_R099 == 1 | doencas_respiratorias == 1 | HIV == 1  
tabulate comorbidade_num óbito if semana_int>9 & semana_int<41, chi2



stcox i.raca_numerico gênero_feminino COVID_diag_principal hosp_alta_complexidade i.idade_num i.mes utilizou_UTI obesidade doenca_bacteriana cancer diabetes doencas_cardiacas insuf_renal R000_R099 doencas_respiratorias HIV i.co_regsaud if semana_int>9 & semana_int<41, vce(robust)
outreg2 using myfile5.xls, bdec(3) stats(coef) level(95) append eform drop(i.co_regsaud) 2aster
outreg2 using myfile5.xls, bdec(3) stats(ci) level(95) append eform drop(i.co_regsaud) 2aster


foreach j in i.raca_numerico gênero_feminino COVID_diag_principal hosp_alta_complexidade i.idade_num i.mes utilizou_UTI obesidade doenca_bacteriana cancer diabetes doencas_cardiacas insuf_renal R000_R099 doencas_respiratorias HIV  {
stcox `j' if semana_int>9 & semana_int<41, vce(robust)
outreg2 using naoajustado.xls, bdec(3) stats(coef) level(95) append eform  2aster
outreg2 using naoajustado.xls, bdec(3) stats(ci) level(95) append eform  2aster
}


asdoc sum i.raca_numerico gênero_feminino COVID_diag_principal hosp_alta_complexidade i.idade_num i.mes utilizou_UTI obesidade doenca_bacteriana cancer diabetes doencas_cardiacas insuf_renal R000_R099 doencas_respiratorias HIV if semana_int>9 & semana_int<41

