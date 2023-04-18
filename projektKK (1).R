library(dplyr)
library(ggpubr) #wykres
library(gridExtra) #kilka wykresow na raz
library(car)
library(dunn.test)
library(FSA)
library(qpdf)

args=commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Nalezy podac co najmniej jeden argument wejsciowy")
}

#dane <- read.csv2(file=args[1], header=TRUE,sep=";") #skrypt wczytywany w trybie wsadowym
dane <-read.csv2('http://www.cs.put.poznan.pl/kgutowska/RPiS/dane/przykladoweDane-Projekt.csv', header=TRUE, sep=";")
#dane <-read.csv2("grupy2.csv", header=TRUE, sep=";") #plik musi byc w tym samym folderze co projekt

dane

wykresy_pdf <-list()
#uzupelnianie brakow srednia
braki <-sum(is.na(dane))
wiersz <-c()
kolumna <-c()
wprowadzono <-c()
for (i in 1:ncol(dane))
{
  if (is.numeric(dane[,i]))#sprawdzenie czy kolumny sa numeryczne, do wyliczenia sredniej
  {
    for (j in 1:nrow(dane))
    {
      if(is.na(dane[j,i])==TRUE)
      {
        srednia <-mean(dane[which(dane[,1]==dane[j,1]),i], na.rm=TRUE) #srednia bez brakow
        #impute(dane[j,i],srednia)
        dane[j,i] <-srednia
        wiersz <-append(wiersz,j)
        kolumna <-append(kolumna,i)
        wprowadzono <-append(wprowadzono, srednia)
      }
    }
  }
}
cat(paste("Ilosc brakow  w tabeli:",braki,"\nWprowadzone zmiany:\n"))
ramka <- data.frame(Wiersz=wiersz, Kolumna=kolumna, Grupa=dane[wiersz,1], Parametr=colnames(dane)[kolumna], Wprowadzono=wprowadzono)
ramka

dane

#wartosci odstajace
par(mfrow=c(2,5)) #boxploty do kazdego parametru numerycznego
for (i in 2:ncol(dane)) 
{
  if (is.numeric(dane[,i]))
  {
    boxplot(dane[,i], main=colnames(dane[i]))
  }
}
dev.off()

ramka1 <-data.frame()
pdf("w_odstajace.pdf")
w_odstajace <-function()
{
  wiersz <-c()
  kolumna <-c()
  wartosc <-c()
  for (i in 2:ncol(dane))
  {
    if (is.numeric(dane[,i]))
    {
      odstajace <-boxplot(dane[,i],main=colnames(dane[i]))$out
      if(length(odstajace)!=0)#jesli ilosc wartosci odstajacych nie jest zerem
      {
        wiersz <-append(wiersz,which(dane[,i] %in% odstajace))
        kolumna <-append(kolumna,rep(i,length(odstajace)))
        wartosc <-append(wartosc, odstajace)
      }
    }
  }
  ramka1 <<-data.frame(Wiersz=wiersz, Kolumna=kolumna, Grupa=dane[wiersz,1], Parametr=colnames(dane)[kolumna], Wartosc=wartosc)
}
w_odstajace()
cat("Wartosci odstajace:\n")
ramka1
dev.off()

ilosc_grup <-as.numeric(count(unique(dane[1])))

#charakterystyka grup
numeryczne <-data.frame()
nienumeryczne <-data.frame()
pdf("charakt_boxploty.pdf")
for (i in 2:ncol(dane))
{
  if (is.numeric(dane[,i]))
  {
    tabelka <-data.frame()
    parametr <-colnames(dane)[i]
    tabelka <-dane %>% group_by(dane[1]) %>%
        summarise(parametr,ilosc=length(.data[[parametr]]),
                  min=min(.data[[parametr]]),#.data... s³u¿y do pobrania danych za pomoc¹ nazwy kolumny
                  max=max(.data[[parametr]]),
                  mean=mean(.data[[parametr]]),
                  median=median(.data[[parametr]]),
                  IQR=IQR(.data[[parametr]]),
                  var=round(var(.data[[parametr]]),6),
                  sd=sd(.data[[parametr]])
        )
    numeryczne <-rbind(numeryczne,tabelka)
    boxplot(dane[,i] ~ dane[,1], data=dane, main=colnames(dane[i]))
  }
  else
  {
    nienumeryczne <- dane %>% group_by(dane[1]) %>% count(dane[i])
  }
}
numeryczne <-numeryczne[order(numeryczne[,1]),]
print(numeryczne)
print(nienumeryczne)
dev.off()
boxplot(dane[,i] ~ dane[,1], data=dane, main=colnames(dane[i]))

#Analiza porownawcza
par <-c()
zgodnosc <-c()
niezgodnosc <-c()
ramka2 <-data.frame()
#par(mfrow = c(3, 3))
wykresy <-list()
indeks <-1
for (i in 2:ncol(dane))
{
  if (is.numeric(dane[,i]))
  {
    niezgodne <-""
    parametr <-colnames(dane)[i]
    par <-append(par, parametr)
    cat(paste("\n\nParametr: ",parametr,"\n"))
    ShapiroTest<-dane %>% group_by(dane[1]) %>%
      summarise(
        p.value = shapiro.test(.data[[parametr]])$p.value
      )
    #print(ShapiroTest)
    zgodne<-0
    for(j in 1:length(ShapiroTest$p.value))
    {
      if(ShapiroTest$p.value[j] < 0.05)
      {
        cat("\n", as.character(ShapiroTest[j,1]),":", ShapiroTest$p.value[j], "< 0.05 - brak zgodnosci z rozkladem normalnym")
        niezgodne <- paste(niezgodne,as.character(ShapiroTest[j,1]))
      }
      else
      {
        cat("\n", as.character(ShapiroTest[j,1]),":", ShapiroTest$p.value[j], "> 0.05 - wystepuje zgodnosc z rozkladem normalnym")
        zgodne <-zgodne+1
        niezgodne <- paste(niezgodne,"X")
      }
    }
    if(zgodne==ilosc_grup) #jesli wszystkie grupy sa zgodne dla danego parametru
    {
      zgodnosc <-append(zgodnosc,"TAK")
    }
    else
    {
      zgodnosc <-append(zgodnosc, "NIE")
    }
    wykresy[[indeks]] <-ggdensity(dane, x = parametr,                 #wykres
                                  color = colnames(dane)[1], fill = colnames(dane)[1],
                                  palette = c("lightgreen", "lightblue", "pink", "violet", "orange"),
                                  ylab = "gestosc",
                                  xlab = paste(parametr," [mg/l]")
    ) + facet_wrap(~ .data[[colnames(dane)[1]]], scales = "free") #.data... to grupa
    indeks <-indeks+1
    niezgodnosc <-append(niezgodnosc,niezgodne)
  }
}
ramka2 <-data.frame(Parametr=par, Zgodnosc=zgodnosc, Niezgodne_grupy=niezgodnosc) #zgodnosc z rozkladem normalnym
ramka2
grid.arrange(grobs=wykresy, nrow=3)

#dev.off()

#Jednorodnosc wriancji
jednorodnosc <-c()
jednorodnosc_pvalue <-c()
for (i in 1:ncol(dane))
{
  if (is.numeric(dane[,i]))#sprawdzenie czy kolumny sa numeryczne
  {
    levene <- leveneTest(dane[,i] ~ dane[,1], data = dane) #dane[,1] to grupa
    if(levene$"Pr(>F)"[1] < 0.05)
    {
      cat("\n",colnames(dane)[i]," ", levene$"Pr(>F)"[1], "< 0.05 - brak zgodnosci z jednorodnoscia wariancji")
      jednorodnosc <-append(jednorodnosc, "NIE")
      jednorodnosc_pvalue <-append(jednorodnosc_pvalue,levene$"Pr(>F)"[1])
    }
    else
    {
      cat("\n",colnames(dane)[i]," ", levene$"Pr(>F)"[1], "> 0.05 - wystepuje zgodnosc z jednorodnoscia wariancji")
      jednorodnosc <-append(jednorodnosc,"TAK")
      jednorodnosc_pvalue <-append(jednorodnosc_pvalue,levene$"Pr(>F)"[1])
    }
  }
}
ramka2$Jednorodnosc=jednorodnosc #jednorodnosc wariancji
ramka2$Jed_pvalue=jednorodnosc_pvalue
ramka2

#dla wiecej niz 2 grup
if(ilosc_grup>2)
{
  brak_roznic <-c()
  p_value <-c()
  par_roznice <-c()
  grupy <-c()
  pvalue <-c()
  rodzaj_testu <-c()
  kruskal <-function(p)
  {
    pvalueKWtest<- kruskal.test(dane[,p] ~ dane[,1], data=dane)$p.value
    if(pvalueKWtest < 0.05)
    {
      cat(pvalueKWtest, "< 0.05 - wystepuja roznice miedzy grupami\n")
      dunn <-dunnTest(dane[,p], dane[,1])
      print(dunn)
      d <-as.data.frame(dunn[2:2]) #bo przy 1:1 jest tylko metoda holm
      for (j in 2:nrow(d))
      {
        if (d[j,4] < 0.05)
        {
          cat("\nIstotne statystycznie roznice pomiedzy grupami:\n")
          print(d[j,])
          pvalue <<-append(pvalue,d[j,4])
          par_roznice <<-append(par_roznice,colnames(dane)[p])
          grupy <<-append(grupy,d[j,1])
        }
      }
    }
    else
    {
      cat(pvalueKWtest, "> 0.05 - brak roznic miedzy grupami\n")
      brak_roznic <<-append(brak_roznic,colnames(dane)[p])
      p_value <<-append(p_value,pvalueKWtest)
    }
  }

  for(i in 1:nrow(ramka2))
  {
    if(ramka2[i,2]=="TAK") #jesli jest zgodne
    {
      cat("\n",ramka2[i,1])
      if(ramka2[i,4]=="TAK")
      {
        cat(" test anova:\n")
        rodzaj_testu <<-append(rodzaj_testu,"ANOVA")
        pvalueAOVtest <-summary(aov(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane))[[1]][["Pr(>F)"]][[1]]
        if(pvalueAOVtest < 0.05)
        {
          cat(pvalueAOVtest, "< 0.05 - wystepuja roznice miedzy grupami\n")
          tukey <-TukeyHSD(aov(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane))
          print(tukey)
          t <-as.data.frame(tukey[1:1])
          for (j in 2:nrow(t))
          {
            if (t[j,4] < 0.05)
            {
              print("Istotne statystycznie roznice pomiedzy grupami:")
              print(t[j,])
              pvalue <-append(pvalue,t[j,4])
              par_roznice <-append(par_roznice,ramka2[i,1])
              grupy <-append(grupy,rownames(t[j,]))
            }
          }
        }
        else
        {
          cat(pvalueAOVtest, "> 0.05 - brak roznic miedzy grupami\n")
          brak_roznic <-append(brak_roznic,ramka2[i,1])
          p_value <<-append(p_value,pvalueAOVtest)
        }
      }
      else
      {
        cat(" test kruskala:\n")
        rodzaj_testu <<-append(rodzaj_testu,"Kruskal-Wallis")
        kruskal(which(colnames(dane)==ramka2[i,1]))#podaj numer kolumny z tabeli dane, ktora ma nazwe taka jak w ramka2[i,1]
      }
    }
    else
    {
      cat("\n",ramka2[i,1],"test kruskala:\n")
      rodzaj_testu <<-append(rodzaj_testu,"Kruskal-Wallis")
      kruskal(which(colnames(dane)==ramka2[i,1]))
    }
  }
cat("Przeprowadzone testy:\n")
testy <-data.frame(Parametr=ramka2[,1], Test=rodzaj_testu)
print(testy)
cat("\nParametry w ktorych nie wystepuja roznice miedzy grupami:\n",brak_roznic,"\n")
brak <-data.frame(Parametr=brak_roznic, Pvalue=p_value)
print(brak)
cat("\nParametry w ktorych wystepuja roznice miedzy grupami:\n",par_roznice,"\n")
roznice <-data.frame(Parametr=par_roznice, Grupy=grupy, Pvalue=pvalue)
print(roznice)

#zapis do plikow
pdf(file="testy.pdf")
grid.table(testy)
dev.off()
pdf(file="roznice.pdf")
grid.table(roznice)
dev.off()
}
#dla dwóch grup
if (ilosc_grup==2)
{
  r <-c() #wektor roznic
  test <-c()
  p_val <-c()
  for(i in 1:nrow(ramka2))
  {
    if(ramka2[i,2]=="TAK") #jesli jest zgodne
    {
      cat("\n",ramka2[i,1])
      if(ramka2[i,4]=="TAK")
      {
        cat(" test t-studenta:\n")
        test <-append(test,"t-Studenta")
        t.test(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane, var.equal = TRUE)
        pvalueTtest <- t.test(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane, var.equal = TRUE)$p.value
        p_val <-append(p_val,pvalueTtest)
        if(pvalueTtest < 0.05)
        {
          cat(pvalueTtest, "< 0.05 - wystepuja roznice miedzy grupami\n")
          r <-append(r,"TAK")
        }
        else
        {
          cat(pvalueTtest, "> 0.05 - brak roznic miedzy grupami\n")
          r <-append(r,"NIE")
        }
      }
      else
      {
        cat(" test welcha:\n") #test t-studenta z wartoscia FALSE
        test <-append(test, "Welcha")
        t.test(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane, var.equal = FALSE)
        pvalueTFtest <- t.test(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane, var.equal = FALSE)$p.value
        p_val <-append(p_val,pvalueTFtest)
        if(pvalueTFtest < 0.05)
        {
          cat(pvalueTFtest, "< 0.05 - wystepuja roznice miedzy grupami\n")
          r <-append(r,"TAK")
        }
        else
        {
          cat(pvalueTFtest, "> 0.05 - brak roznic miedzy grupami\n")
          r <-append(r,"NIE")
        }
      }
    }
    else #zrobic cos z tym wilcoxonem, bo nie dziala
    {
      cat("\n",ramka2[i,1],"test wilcoxona:\n")
      test <-append(test,"Wilcoxona")
      WilTest <-wilcox.test(dane[,which(colnames(dane)==ramka2[i,1])] ~ dane[,1], data = dane)
      WilTest$p.value
      p_val <-append(p_val,WilTest$p.value)
      if(WilTest$p.value < 0.05)
      {
        cat(WilTest$p.value, "< 0.05 - wystepuja roznice miedzy grupami\n")
        r <-append(r,"TAK")
      }
      else
      {
        cat(WilTest$p.value, "> 0.05 - brak roznic miedzy grupami\n")
        r <-append(r,"NIE")
      }
    }
  }
  cat("\nWystepowanie roznic miedzy grupami:",unique(dane[,1]),"\n")
  grupy_dwie <-data.frame(Parametr=ramka2[,1], Test=test, Pvalue=round(p_val,6), Wystepowanie_roznic=r)
  grupy_dwie
  pdf(file="grupy_dwie.pdf")
  grid.table(grupy_dwie)
  dev.off()
}
#zmienne jakosciowe
x <-which(lapply(dane, is.numeric)==FALSE)
ilosc_nienumerycznych <-nrow(as.data.frame(x))-1 #odjac grupe,ktora tez jest nienumeryczna
pdf("jakosciowe.pdf") #zakomentowac zeby zadzialalo par
par(mfrow=c(1,ilosc_nienumerycznych))
for (i in 2:ncol(dane))
{
  if (is.numeric(dane[,i])==FALSE)#sprawdzenie czy kolumny sa nienumeryczne
  {
    cat("Parametr:",colnames(dane)[i],"\n")
    chisq <-chisq.test(dane[,1], dane[,(colnames(dane)[i])])
    print(chisq)
    barplot(table(dane[,(colnames(dane)[i])], dane[,1]),
            main=paste("p-value =", round(chisq$p.value, digits = 3)),
            ylim = c(0,20),
            beside = TRUE,
            col = c("lightblue", "pink"),
            xlab = colnames(dane)[1],
            ylab = colnames(dane)[i],
            legend = unique(dane[,(colnames(dane)[i])])
            )
    indeks <-indeks+1
  }
}
dev.off()

#Analiza korelacji
numer_kolumn <-c()
for (i in 2:ncol(dane))
{
  if (is.numeric(dane[,i]))#sprawdzenie czy kolumny sa numeryczne
  {
    numer_kolumn <-append(numer_kolumn,i)
  }
}
kombinacje <-combn(names(dane[,numer_kolumn]),2,simplify=FALSE) #kombinacje tylko z danymi numerycznymi
ilosc_kombinacji <-length(kombinacje)

sila_kor <-c()
sila <-function(r)
{
  w_bezwzgledna <-abs(r)
  if(r > -0.2 && r < 0.2)
  {
    sila_kor <<-append(sila_kor,"brak")
  }
  else if (w_bezwzgledna < 0.3)
  {
    sila_kor <<-append(sila_kor,"slaba")
  }
  else if (w_bezwzgledna < 0.5)
  {
    sila_kor <<-append(sila_kor,"srednie natezenie")
  }
  else if (w_bezwzgledna < 0.7)
  {
    sila_kor <<-append(sila_kor,"silna")
  }
  else
  {
    sila_kor <<-append(sila_kor,"bardzo silna")
  }
}
istotna_komb <-c()
istotna_grupa <-c()
p_value <-c()
wspolczynnik <-c()
kierunek <-c() #kierunek korelacji
wykresyy <-list() #lista wykresow Pearsona
indekss <-1
wykresyy2 <-list() #lista wykresow Spearmana
indekss2 <-1
for (i in 1:ilosc_kombinacji)
{
  #cat(kombinacje[[i]],"\n")
  pierwszy <-as.name(first(kombinacje[[i]]))
  #print(pierwszy)
  drugi <-as.name(last(kombinacje[[i]])) #drugi element z kombinacji
  #print(drugi)
  wiersz1 <-strsplit(ramka2[which(ramka2==pierwszy),3], " ")[[1]] #wiersz potrzebny do okreslenia, czy grupa z pierwszego elementu jest w zgodna z rozkl.normalnym
  wiersz2 <-strsplit(ramka2[which(ramka2==drugi),3], " ")[[1]] #drugi wiersz do tej samej grupy
  for (j in 1:length(unique(dane[,1]))) #ta petla jest po to by sprawdzac jednoczesnie dwa parametry dla tej samej grupy
  {                                     #jesli obie grupy maja "X" to maja zgodnosc z rozkladem normalnym
    group <- dane %>% filter(dane[,1]==unique(dane[,1])[j])
    group
    if(grepl(wiersz1[j+1], "X", fixed = TRUE)==TRUE && grepl(wiersz2[j+1], "X", fixed = TRUE)==TRUE) #jesli nazwa grupy sie znajduje w kolumnie niezgodne
    {
      #Pearson
      korelacja <-cor.test(group[,which(colnames(dane)==pierwszy)], group[,which(colnames(dane)==drugi)], method = "pearson")
      #print(korelacja)
      if(korelacja$p.value < 0.05)
      {
        #cat("Wystepuje korelacja pomiedzy",kombinacje[[i]],"w grupie",unique(dane[,1])[j],"\n")
        istotna_komb <-append(istotna_komb,paste(pierwszy,drugi))
        istotna_grupa <-append(istotna_grupa,unique(dane[,1])[j])
        p_value <-append(p_value,round(korelacja$p.value,5))
        wspolczynnik <-append(wspolczynnik,korelacja$estimate)
        if(korelacja$estimate > 0)
        {
          kierunek <-append(kierunek,"dodatni")
        }
        if(korelacja$estimate == 0)
        {
          kierunek <-append(kierunek,"brak")
        }
        if(korelacja$estimate < 0)
        {
          kierunek <-append(kierunek,"ujemny")
        }
        sila(korelacja$estimate)
        wykresyy[[indekss]] <-ggscatter(group, x = kombinacje[[i]][1], y = kombinacje[[i]][2], 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  color = colnames(dane)[1], fill = colnames(dane)[1],
                  palette = c("aquamarine"),
                  ylab = kombinacje[[i]][1], 
                  xlab = kombinacje[[i]][2]
        )
        indekss <-indekss+1
      }
    }
    else #Spearman
    {
      korelacja <-cor.test(group[,which(colnames(dane)==pierwszy)], group[,which(colnames(dane)==drugi)], method = "spearman")
      #print(korelacja)
      if(korelacja$p.value < 0.05)
      {
        #cat("Wystepuje korelacja pomiedzy",kombinacje[[i]],"w grupie",unique(dane[,1])[j],"\n")
        istotna_komb <-append(istotna_komb,paste(pierwszy,drugi))
        istotna_grupa <-append(istotna_grupa,unique(dane[,1])[j])
        p_value <-append(p_value,round(korelacja$p.value,5))
        wspolczynnik <-append(wspolczynnik,korelacja$estimate)
        if(korelacja$estimate > 0)
        {
          kierunek <-append(kierunek,"dodatni")
        }
        if(korelacja$estimate == 0)
        {
          kierunek <-append(kierunek,"brak")
        }
        if(korelacja$estimate < 0)
        {
          kierunek <-append(kierunek,"ujemny")
        }
        sila(korelacja$estimate)
        wykresyy2[[indekss2]] <-ggscatter(group, x = kombinacje[[i]][1], y = kombinacje[[i]][2], 
                                        conf.int = TRUE, 
                                        cor.coef = TRUE, cor.method = "spearman",
                                        color = colnames(dane)[1], fill = colnames(dane)[1],
                                        palette = c("violet"),
                                        ylab = kombinacje[[i]][1], 
                                        xlab = kombinacje[[i]][2]
        )
        indekss2 <-indekss2+1
      }
    }
  }
}
print("Istotne statystycznie korelacje:")
istotne <-data.frame(Grupa=istotna_grupa, Parametry=istotna_komb, Pvalue=p_value, Wspolczynnik=wspolczynnik, Kierunek=kierunek, Sila_korelacji=sila_kor)
istotne <-istotne[order(istotne$Grupa),]
istotne

grid.arrange(grobs=wykresyy, nrow=3)
grid.arrange(grobs=wykresyy2, nrow=3)

#zapis tabel do pliku
pdf(file="ramka.pdf")
grid.table(ramka)
dev.off()
pdf(file="ramka1.pdf")
grid.table(ramka1)
dev.off()
pdf(file="charakterystyka.pdf")
grid.table(numeryczne)
dev.off()
pdf(file="nienumeryczne.pdf")
grid.table(nienumeryczne)
dev.off()
pdf(file="ramka2.pdf")
grid.table(ramka2)
dev.off()
pdf(file="istotne.pdf")
grid.table(istotne)
dev.off()



#zapis wykresow do pliku
ggexport(wykresy, filename = "wykresy_rozklad.pdf")
wykresy_pdf <-append(wykresy_pdf,wykresyy)
wykresy_pdf <-append(wykresy_pdf,wykresyy2)
ggexport(wykresy_pdf, filename = "wykresy_korelacja.pdf")

#raport
#zrobic ifa ze jesli wiecej niz dwie grupy to tak, a jesli mniej to zamiast testy.pdf i czegos bedzie grupy_dwie.pdf
if (ilosc_grup > 2)
{
  qpdf::pdf_combine(c("ramka.pdf", "ramka1.pdf","w_odstajace.pdf","charakterystyka.pdf","charakt_boxploty.pdf","nienumeryczne.pdf","ramka2.pdf","wykresy_rozklad.pdf", "testy.pdf","roznice.pdf","jakosciowe.pdf","istotne.pdf","wykresy_korelacja.pdf"), "raport.pdf")
}
if (ilosc_grup==2)
{
  qpdf::pdf_combine(c("ramka.pdf", "ramka1.pdf","w_odstajace.pdf","charakterystyka.pdf","charakt_boxploty.pdf","nienumeryczne.pdf","ramka2.pdf","wykresy_rozklad.pdf", "grupy_dwie.pdf","jakosciowe.pdf","istotne.pdf","wykresy_korelacja.pdf"), "raport.pdf")
}
usuwanie_plikow <-function(f)
if (file.exists(f)) 
{
  file.remove(f)
}
usuwanie_plikow("charakt_boxploty.pdf")
usuwanie_plikow("charakterystyka.pdf")
usuwanie_plikow("grupy_dwie.pdf")
usuwanie_plikow("istotne.pdf")
usuwanie_plikow("jakosciowe.pdf")
usuwanie_plikow("nienumeryczne.pdf")
usuwanie_plikow("ramka.pdf")
usuwanie_plikow("ramka1.pdf")
usuwanie_plikow("ramka2.pdf")
usuwanie_plikow("roznice.pdf")
usuwanie_plikow("testy.pdf")
usuwanie_plikow("w_odstajace.pdf")
usuwanie_plikow("wykresy_korelacja.pdf")
usuwanie_plikow("wykresy_rozklad.pdf")
