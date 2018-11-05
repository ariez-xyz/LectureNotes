DATE=$(date "+%d-%m-%Y")

echo \# $DATE > $DATE.md
echo >> $DATE.md
echo "<!-- Mermaid diagram support... add diags via <<{$FILE} -->" >>$DATE.md
echo "<script src="../mermaid/mermaid.min.js"></script>" >> $DATE.md
echo "<script src="../mermaid/removeDiff.js"></script>" >> $DATE.md
echo >> $DATE.md
echo \<\!--TOC--\> >> $DATE.md
echo >> $DATE.md
echo \#\# >> $DATE.md
echo >> $DATE.md
echo "<script src="../mermaid/removeCaptions.js"></script>" >> $DATE.md
