set datafile commentschars ";"

set terminal qt

set multiplot layout 2,1

set ylabel "sample value"
set bmargin 0
set format x ""
set ytics -0.8,0.2
set key bottom
plot "jumps.dat" using 1:2 every 100 with lines lc rgbcolor "#a0a0b0" title "left channel"

set xlabel "time (s)"
set bmargin
set tmargin 0
set format x "%g"
set ytics -1.0,0.2,0.8
set key top

plot ""         using 1:3 every 100 with lines lc rgbcolor "#a0a0b0" title "right channel"
unset multiplot
pause mouse button1
