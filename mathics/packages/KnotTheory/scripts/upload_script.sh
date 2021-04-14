echo \$Path \= \$Path~Join~{\".\", \"..\", \"../..\"}\; > upload_script_$3.m
echo \<\<KnotTheory\` >> upload_script_$3.m
echo CreateWikiConnection[\"http://katlas.math.toronto.edu/w/index.php\", \"$1\", \"$2\"] >> upload_script_$3.m
echo ProcessKnotAtlasUploadQueue[\"$3\"] >> upload_script_$3.m
echo Exit[] >> upload_script_$3.m
cat upload_script_$3.m | $4math
