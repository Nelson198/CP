file = cp1718tp

pdf: $(file).tex
	pdflatex $(file).tex

bib: $(file).aux
	bibtex $(file).aux

index: $(file).idx
	makeindex cp1718tp.idx

run: $(file).lhs
	sudo cp -a Bibliotecas/* .
	ghci $(file).lhs

all: $(file).tex
	lhs2TeX $(file).lhs > $(file).tex
	pdflatex $(file).tex

# Automatically compile what is necessary, as many times as needed.
.PHONY : rubber
rubber :
	@rubber --pdf -f $(file)

clean:
	rm -rf *.aux *.log *.bbl *.bak *.ptb *.blg *.out *.spl *.hs

cleanall : clean
	rm $(file).pdf