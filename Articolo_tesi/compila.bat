"C:\Program Files\R\R-3.4.3\bin\i386\Rcmd" Stangle --encoding="ISO-8859-1" "ArticoloPLFA.Rnw"
"C:\Program Files\R\R-3.4.3\bin\i386\Rcmd" Sweave --encoding="ISO-8859-1" "ArticoloPLFA.Rnw"

pdflatex ArticoloPLFA.tex

bibtex ArticoloPLFA.aux

pdflatex ArticoloPLFA.tex

pdflatex ArticoloPLFA.tex

