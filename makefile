THESIS=thesis
TEX=pdflatex
FLAGS=-halt-on-error

all: pdf count

test: pdf testcount

pdf: *.tex literature.bib Background/*.tex Blind/*.tex Static-Analysis/*.tex
	$(TEX) -draftmode $(FLAGS) $(THESIS).tex
	bibtex $(THESIS)
	$(TEX) -draftmode $(FLAGS) $(THESIS).tex
	$(TEX) $(FLAGS) $(THESIS).tex

count: *.tex Background/*.tex Blind/*.tex Static-Analysis/*.tex
	echo "$$(date +"%F %T"),$$(detex thesis.tex | wc -w)" >> wordcount.csv

testcount: *.tex Background/*.tex Blind/*.tex Static-Analysis/*.tex
	echo "$$(date +"%F %T"),$$(detex thesis.tex | wc -w)"

clean:
	rm $(THESIS).aux $(THESIS).bbl $(THESIS).blg $(THESIS).log $(THESIS).pdf
