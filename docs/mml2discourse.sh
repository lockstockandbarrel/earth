pandoc --list-input-formats --list-output-formats                                 
(
cat \
 Unicode.markdownMML \
 ucs4.markdownMML \
 lesson1_ucs4.markdownMML \
 lesson2_ucs4.markdownMML \
 lesson3_ucs4.markdownMML \
 lesson4_ucs4.markdownMML \
 lesson5_ucs4.markdownMML \
 lesson6_ucs4.markdownMML \
 lesson7_ucs4.markdownMML \
 summary_ucs4.markdownMML \
 no_iso_10646.markdownMML \
 extensions_ext.markdownMML \
 backslash_ext.markdownMML \
 bom_ext.markdownMML \
 utf8_source_ext.markdownMML \
 $NULL
)| \
 sed -e '/|TOP\]\]/d' | \
 sed -e '/|NEXT\]\]/d' | \
 sed -e '/|PREVIOUS\]\]/d' | \
 sed -e 's/\[\[\(.*\)|\(.*\)\]\]/[\2](#\1)/' | \
 sed -e 's/^~~~~~~~~~~ .*{: .*lang=\(.*\)}/```\1/' | \
 sed -e 's/[TOP](Unicode)/[TOP](#Unicode)/' | \
 sed -e 's/^~~~~~~~~~~/```/' >index.md
################################################################################
exit
################################################################################
pandoc --from commonmark_x --to gfm --output - | \
################################################################################
gfm
markdown
markdown_mmd
markdown_phpextra
commonmark
commonmark_x

markdown_strict
################################################################################
biblatex
bibtex
bits
creole
csljson
csv
djot
docbook
docx
dokuwiki
endnotexml
epub
fb2
haddock
html
ipynb
jats
jira
json
latex
man
mediawiki
muse
native
odt
opml
org
ris
rst
rtf
t2t
textile
tikiwiki
tsv
twiki
typst
vimwiki
