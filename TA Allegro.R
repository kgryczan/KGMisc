library(readxl)
path = "C://Users/Konrad i Basia/Downloads/Raport Allegro, od 2019-01-01 do 2019-01-31 (2).xlsx"

dates = str_extract_all(path, "\\d{4}-\\d{2}-\\d{2}", simplify = T)

S1 = read_excel(path = path,
                sheet = 2,
                range = cell_cols(1), 
                col_names = F)
S2 = read_excel(path = path,
                sheet = 2,
                range = cell_cols(2),
                col_names = F)

kat = which(S1[1] == paste("Seria",i))
kat_1 = S2[kat,1] %>% as.character()


st = which(S1[1]=="Sprzedawca")
ile = length(st)
end = which(is.na(S1[1]))


for (i in 1:(ile-1)){
temp =  c(st[i]-1,end[min(which(st[i] < end))]-1)

}
last = c(st[ile]-1,nrow(S1))


S1_1 = read_excel(path = path,
                  sheet = 2, 
                  range = cell_rows(a1[1]:a1[2]))%>% row_to_names(1) %>% 
  mutate(kategoria = kat_1, from = dates[1], to = dates[2])

