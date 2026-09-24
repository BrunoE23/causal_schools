# Build the September draft with pdfLaTeX and let latexmk run Biber whenever
# the bibliography control file changes. latexmk then repeats LaTeX until all
# citations and cross-references stabilize.
$pdf_mode = 1;
$bibtex_use = 2;
$pdflatex = 'pdflatex -interaction=nonstopmode -halt-on-error %O %S';
$biber = 'biber %O %B';
$max_repeat = 5;
