TEXFILES=$(shell find . -name \*.tex)

all: main.pdf

main.pdf : $(TEXFILES) main.bib 
	pdflatex main
	bibtex main
	pdflatex main > /dev/null
	pdflatex main > /dev/null 


clean:
	rm -f main.aux
	rm -f main.log
	rm -f main.bbl
	rm -f main.blg
	rm -f main.pdf
