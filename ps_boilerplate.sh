DATE=$(date "+%d-%m-%Y")-PS

echo \# $DATE > $DATE.md
echo >> $DATE.md
echo "<!-- Mermaid diagram support... add diags via <<{$FILE} -->" >>$DATE.md
echo "<script src="../html/mermaid.min.js"></script>" >> $DATE.md
echo "<script src="../html/removeDiff.js"></script>" >> $DATE.md
echo >> $DATE.md
echo \<\!--TOC--\> >> $DATE.md
echo >> $DATE.md
echo \#\# >> $DATE.md
echo >> $DATE.md
echo "<script src="../html/removeCaptions.js"></script>" >> $DATE.md
