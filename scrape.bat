cd \
cd rprojects\hasznaltauto
git pull
cd \
"Program Files\R\R-4.1.0\bin\R.exe" -e "source('C:/rprojects/hasznaltauto/R/scrape.R', echo=TRUE)"
"Program Files\R\R-4.1.1\bin\R.exe" -e "source('C:/rprojects/hasznaltauto/R/scrape.R', echo=TRUE)"
"Program Files\R\R-4.0.3\bin\R.exe" -e "source('C:/rprojects/hasznaltauto/R/scrape.R', echo=TRUE)"
"Program Files\R\R-4.1.0\bin\R.exe" -e "rmarkdown::render('c:/rprojects/hasznaltauto/README.rmd')"
"Program Files\R\R-4.1.1\bin\R.exe" -e "Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/RStudio/bin/pandoc'); rmarkdown::render('c:/rprojects/hasznaltauto/README.rmd')"
"Program Files\R\R-4.0.3\bin\R.exe" -e "rmarkdown::render('c:/rprojects/hasznaltauto/README.rmd')"
cd rprojects\hasznaltauto
git add .
git commit -m "[daily update]"
git push

