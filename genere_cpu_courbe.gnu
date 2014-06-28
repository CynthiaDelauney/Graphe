set title "Courbe CPU"
set xlabel "n" ; set ylabel "temps (secondes)"
plot [*:*] [*:*] 'donnees.dat' using 1:2 title 'Bellman_Ford' with linespoints, 'donnees.dat' using 1:3 title 'Bellman' with linespoints, 'donnees.dat' using 1:4 title 'Dijkstra' with linespoints
