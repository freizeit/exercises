set terminal png size 800,600 enhanced font "Vera,12"
set output '100k-rchans-variation.png'
set title "timings for 100K calculation \n and varying result channel sizes" font "Vera,12" 
set multiplot
set xrange [ 0 : 16 ]
set yrange [ 0 : 150 ]
set xlabel 'CPU cores' font "Vera,12"
set ylabel 'seconds' font "Vera,12"

set tics out
set xtics 1
set ytics 25

set key left top
plot '25000-stats-A-100k.dat.stats' with linespoints lt 1 title '25K'
set key center top
plot '50000-stats-A-100k.dat.stats' with linespoints lt 2 title '50K'
set key right center
plot '100000-stats-A-100k.dat.stats' with linespoints lt 4 title '100K'
set key right top
plot '75000-stats-A-100k.dat.stats' with linespoints lt 3 title '75K'
