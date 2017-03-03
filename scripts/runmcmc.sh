for i in $(ls Rfile??.R); do
echo $i
R CMD BATCH $i
done

