THESIS=thesis
TEX=pdflatex
FLAGS=-halt-on-error
TARGET=1000

all: pdf

test: pdf testcount

quick:
	$(TEX) $(FLAGS) $(THESIS).tex

pdf: *.tex literature.bib Background/*.tex Blind/*.tex Static-Analysis/*.tex Intro/*.tex
	$(TEX) -draftmode $(FLAGS) $(THESIS).tex
	bibtex $(THESIS)
	$(TEX) -draftmode $(FLAGS) $(THESIS).tex
	makeindex $(THESIS).nlo -s nomencl.ist -o $(THESIS).nls
	$(TEX) $(FLAGS) $(THESIS).tex
	detex thesis.tex | wc -w

count: *.tex Background/*.tex Blind/*.tex Static-Analysis/*.tex Intro/*.tex
	echo "$$(date +"%F %T"),$$(detex thesis.tex | wc -w)" >> wordcount.csv
	tail wordcount.csv

testcount: *.tex Background/*.tex Blind/*.tex Static-Analysis/*.tex Intro/*.tex
	echo "($$(tail -1 wordcount.csv | awk -F, '{print $$2}')+$(TARGET))-$$(detex thesis.tex | wc -w)" | bc

clean:
	rm $(THESIS).aux $(THESIS).bbl $(THESIS).blg $(THESIS).log $(THESIS).pdf
