#!/usr/bin/env bash
# Render paper.qmd and assemble a self-contained LaTeX folder + zip in ceurpaper/submission/
set -euo pipefail
cd "$(dirname "$0")"
quarto render paper.qmd --to pdf
rm -rf submission && mkdir -p submission
cp paper.tex ceurart.cls elsarticle-num-names.bst cc-by.pdf cc-by.png ../ABM-LLM.bib submission/
cp -r figures submission/
cd submission
sed -i 's#\\bibliography{../ABM-LLM.bib}#\\bibliography{ABM-LLM}#' paper.tex
pdflatex -interaction=nonstopmode paper >/dev/null
bibtex paper >/dev/null || true
pdflatex -interaction=nonstopmode paper >/dev/null
pdflatex -interaction=nonstopmode paper >/dev/null
# Keep .bbl (CEUR wants it), drop the .bib and build artifacts
rm -f ABM-LLM.bib *.aux *.log *.out *.blg *.toc
zip -qr ../ceur-submission.zip . -x paper.pdf
ls
