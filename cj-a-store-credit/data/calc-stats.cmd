set terminal png size 800,600 enhanced font "Vera,12"
set output 'calc-stats.png'
set title 'Running times for 50K, 100K and 200K calculations'

set multiplot
set xrange [ 0 : 16 ]
set yrange [ 0 : 300 ]
set xlabel 'CPU cores'
set ylabel 'Running time in seconds'

set tics out
set xtics 1
set ytics 25

set key left top
plot '100000-stats-A-200k.dat.stats' with linespoints lt 1 title '200K'
set key center top
plot '50000-stats-A-100k.dat.stats' with linespoints lt 2 title '100K'
set key right top
plot '25000-stats-A-50k.dat.stats' with linespoints lt 3 title '150K'
