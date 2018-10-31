PRINCIPALE		= ArticoloPLFA
PRINCIPALE_TEX		= $(PRINCIPALE).tex

. PHONY : pulisci
pulisci :
	rm -f *.aux *.lof *.out *.snm *.toc *.bbl *.log *.bbl *.blg *.bcf $(PRINCIPALE)-frn.tex

#
Sweave:
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=RweaveLatex(), encoding = 'utf8')"
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=Rtangle(), encoding = 'utf8')"
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE_TEX)
	evince $(PRINCIPALE).pdf


biblio:
	pdflatex $(PRINCIPALE_TEX)
	bibtex $(PRINCIPALE)
	pdflatex $(PRINCIPALE_TEX)

pdf:
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)

tutto:
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=RweaveLatex(), encoding = 'utf8')"
	Rscript -e "Sweave('$(PRINCIPALE).Rnw', driver=Rtangle(), encoding = 'utf8')"
	pdflatex $(PRINCIPALE)
	bibtex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE)
	pdflatex $(PRINCIPALE_TEX)
	evince $(PRINCIPALE).pdf


