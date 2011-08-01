set terminal png size 800,600 enhanced font "Vera,12"
set output '200k-rchans-variation.png'
set title "Running times for 200K calculations and \n varying result channel sizes"

set multiplot
set xrange [ 0 : 16 ]
set yrange [ 0 : 300 ]
set xlabel 'CPU cores'
set ylabel 'Running time in seconds'

set tics out
set xtics 1
set ytics 25

set key left top
plot '50000-stats-A-200k.dat.stats' with linespoints lt 1 title 'chan size: 50K'
set key center top
plot '100000-stats-A-200k.dat.stats' with linespoints lt 2 title 'chan size: 100K'
set key right center
plot '200000-stats-A-200k.dat.stats' with linespoints lt 4 title 'chan size: 200K'
set key right top
plot '150000-stats-A-200k.dat.stats' with linespoints lt 3 title 'chan size: 150K'
