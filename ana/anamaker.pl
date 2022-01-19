#!/usr/bin/perl -s

$category=$c||"mssd";


#$filename="$category.ana";
$filename="auto.ana";
open (OUT,">$filename");

print OUT "c ::made with the script anamaker.pl -c=$category\n";

if ($category eq 'mssd') {

print OUT 
"analys
5

";

foreach $id (1..6) {
    $nmux=(int(($id-1)/2)+1); 
    $nsub=($id%2)||2; 
    @name[$id] = "MUX $nmux-$nsub";


    print OUT "hst1\n";    
    print OUT "0, 5,$id,$id,2, 1000,0,4000, '$name[$id] E1raw'\n";
    print OUT "0, 5,$id,$id,3, 1000,0,4000, '$name[$id] E2raw'\n";
    print OUT "0, 5,$id,$id,4, 1000,0,4000, '$name[$id] P1'\n";
    print OUT "0, 5,$id,$id,5, 1000,0,4000, '$name[$id] P2'\n";
    print OUT "0, 5,$id,$id,6, 1000,0,4000, '$name[$id] T'\n";
    print OUT "0, 5,$id,$id,12, 500,0,40, '$name[$id] E1cal'\n";
    print OUT "0, 5,$id,$id,13, 500,0,40, '$name[$id] E2cal'\n";
    print OUT "0, 5,$id,$id,14, 16,0.5,16.5, '$name[$id] P1n'\n";
    print OUT "0, 5,$id,$id,15, 16,0.5,16.5, '$name[$id] P2n'\n";
    print OUT "0, 5,$id,$id,16, 1000,0,400, '$name[$id] Tcal'\n";

    print OUT "hst2\n";    
    print OUT "0, 5,$id,$id,4, 5,$id,$id,2, 200,0,4000, 200,0,4000, '$name[$id] E1 vs P1raw'\n";
    print OUT "0, 5,$id,$id,5, 5,$id,$id,3, 200,0,4000, 200,0,4000, '$name[$id] E2 vs P2raw'\n";



}

#hst2
#0, 3,1,1,2, 3,1,1,3, 200,0,4000, 200,0,4000, 'SSD1 T vs A' 

print OUT "exit\n";


}


elsif ($category eq 'dssd') {

print OUT 
"analys
4
hst1
";

@telnames=("","Tel1 dE1X","Tel1 dE2Y", "Tel2 dE2Y", "Tel3 dE2Y");

foreach $id (1..64) {

    $ntel=(int(($id-1)/16)+1);
    $nstrip=($id%16)||16; 

    @name[$id] = "$telnames[$ntel] E${nstrip}raw";
    
    print OUT "0, 4,$id,$id,2, 1000,0,4000, '$name[$id]'\n";
}

foreach $id (1..64) {
    $ntel=(int(($id-1)/16)+1);
    $nstrip=($id%16)||16; 

    @name[$id] = "$telnames[$ntel] T${nstrip}raw";
    
    print OUT "0, 4,$id,$id,3, 1000,0,10000, '$name[$id]'\n";
}

print OUT "hst2\n";
print OUT "0, 4,1,64,1, 4,1,64,2, 64,0.5,64.5, 200,0,4000, 'DSSD Eraw vs ch'\n";
print OUT "0, 4,1,64,1, 4,1,64,4, 64,0.5,64.5, 200,0,20, 'DSSD Ecal vs ch'\n";
print OUT "0, 4,1,64,1, 4,1,64,3, 64,0.5,64.5, 200,0,10000, 'DSSD Traw vs ch'\n";
print OUT "0, 4,1,64,1, 4,1,64,5, 64,0.5,64.5, 200,0,800, 'DSSD Tcal vs ch'\n";

foreach $ntel (1..4) {
    $start=($ntel-1)*16+1; $end=$start+15;
    print OUT "0, 4,$start,$end,6, 4,$start,$end,2, 16,0.5,16.5, 200,0,4000, '$telnames[$ntel] Eraw vs nstrip'\n";
    print OUT "0, 4,$start,$end,6, 4,$start,$end,3, 16,0.5,16.5, 200,0,4000, '$telnames[$ntel] Ecal vs nstrip'\n";
}



print OUT "exit\n";



}

elsif ($category eq 'ssd') {

print OUT 
"analys
6
hst1
";

@ssdnames = ("","Tel1 E1","Tel1 E2","Tel2 E1","Tel2 E2",
              "Tel3 E1","Tel3 E2","Tel4 E1","Tel4 E2",
              "Tel5 E1","Tel5 E2","Tel1 dE2X","Tel2 dE2Y",
              "Tel3 dE2X","Lowgain 1","Lowgain 2","Lowgain 3","F2SSD");

foreach $id (1..$#ssdnames) {

    @name[$id] = "$ssdnames[$id] Eraw";
    
    print OUT "0, 6,$id,$id,2, 1000,0,4000, '$name[$id]'\n";
}

foreach $id (1..$#ssdnames) {
    @name[$id] = "$ssdnames[$id] Traw";
    
    print OUT "0, 6,$id,$id,3, 1000,0,10000, '$name[$id]'\n";
}

print OUT "hst2\n";
print OUT "0, 6,1,17,1, 6,1,17,2, 17,0.5,17.5, 200,0,4000, 'SSD Eraw vs ch'\n";
print OUT "0, 6,1,17,1, 6,1,17,4, 17,0.5,17.5, 200,0,20, 'SSD Ecal vs ch'\n";
print OUT "0, 6,1,17,1, 6,1,17,3, 17,0.5,17.5, 200,0,10000, 'SSD Traw vs ch'\n";


print OUT "exit\n";



}

elsif ($category eq 'ppac') {

print OUT 
"analys
1
2
";

@ppacnames = ("","F3 PPACa","F3 PPACb","F2 PPAC","F1 PPAC");

foreach $id (1..$#ppacnames) {

    $name[$id] = "$ppacnames[$id]";
   
 print OUT "    
c $name[$id]
hst1
0, 2,$id,$id,5, 2000,0,8000, '$name[$id] X1raw'
0, 2,$id,$id,6, 2000,0,8000, '$name[$id] X2raw'
0, 2,$id,$id,7, 2000,0,8000, '$name[$id] Y1raw'
0, 2,$id,$id,8, 2000,0,8000, '$name[$id] Y2raw'
0, 2,$id,$id,2, 2000,0,8000, '$name[$id] Traw'

0, 2,$id,$id,22, 500,-50.,50., '$name[$id] X(ns)'
0, 2,$id,$id,23, 500,-50.,50., '$name[$id] Y(ns)'
0, 2,$id,$id,24, 500,-50.,50., '$name[$id] X(mm)'
0, 2,$id,$id,25, 500,-50.,50., '$name[$id] Y(mm)'

hst2
0, 2,$id,$id,24,  2,$id,$id,25, 120,-60.,60., 120,-60.,60., '$name[$id] Y vs X (mm)'
0, 2,$id,$id,5,  2,$id,$id,6, 200,0.,4000., 200,0.,4000., '$name[$id] X2 vs X1raw'
0, 2,$id,$id,7,  2,$id,$id,8, 200,0.,4000., 200,0.,4000., '$name[$id] Y2 vs Y1raw'

";    
}


print OUT "exit\n";



}

elsif ($category eq 'tel') {

    $telmax=5;

print OUT 
"analys
4
5
6
7
hst1
";
foreach $id (1..$telmax) {

    $name = "Esum tel $id";
    print OUT "0, 7,$id,$id,2, 1000,0,40, '$name'\n";
}


print OUT 
"hst2
";


foreach $id (1..$telmax) {

    $name = "tel $id Esum vs dE1";
    print OUT "0, 7,$id,$id,11, 7,$id,$id,2, 200,0,10, 200,0,40, '$name'\n";
    $name = "tel $id dE2 vs dE1";
    print OUT "0, 7,$id,$id,11, 7,$id,$id,12, 200,0,10, 200,0,15, '$name'\n";
    $name = "tel $id E1 vs dE2";
    print OUT "0, 7,$id,$id,12, 7,$id,$id,13, 200,0,15, 200,0,20, '$name'\n";
    $name = "tel $id E2 vs E1";
    print OUT "0, 7,$id,$id,13, 7,$id,$id,14, 200,0,20, 200,0,20, '$name'\n";

}



}



close OUT
