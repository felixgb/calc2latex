cd $1
pdflatex out.tex
convert -density 300 out.pdf out.png
