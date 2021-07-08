DIR = .
default: $(DIR)/*csv
	echo > results.md
	for file in $^; do \
		echo \`$$file\`: >> results.md ;\
		echo >> results.md ;\
		./ping.py $$file > tmp ;\
		csv2md -p < tmp >> results.md ;\
	done
	pandoc --pdf-engine=xelatex -V geometry:margin=0.5in intro.md results.md -o nvs21.ps.blatt6-ab4-pape.pdf
	rm tmp
