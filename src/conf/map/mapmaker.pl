#!/usr/bin/perl -s

#HY: please edit this file to produce the map file.
#do not edit *.map, as that can be overwritten.

#usage: mapmaker.pl -c=<category>
# <category> = ppac, rf, ssd etc.

$category=$c||"ppac";

$device = 12; #(12:CRIB)    
$fp = 0;  # everything is 0 at CRIB.

#GEO/id for each module
($adc1,$adc2,$adc3)=(1,2,3);
($madc1,$madc2)=(6,7);
($tdc1,$tdc2,$tdc3)=(11,12,13);
$mhtdc=16;

$did_t=7;      $did_a=6; #detector id, TDC:7, ADC:6 for all ADC/TDC



if ($category eq 'rf') {
     $segment=1; # for anapaw segment id    
     $ndata=2; # number of data per line
     $nid=1; # number of id

# you should prepare here,
#     @module[$nid][$ndata];
#     @ch[$nid][$ndata];
#     @did[$nid][$ndata]
#     @name[$nid];

     foreach $id (1..$nid) {
	 @{$module[$id]}=&repeatn($tdc1,$ndata);   #this fills all with the same number
	 @{$did[$id]}=&repeatn($did_t,$ndata);
     }
     
     @{$ch[1]}[1..$ndata] = (0,1); #ch number, starting from 1

     @name[1] = "RF";

} 

if ($category eq 'coin') {
     $segment=3; # for anapaw segment id    
     $ndata=1; # number of data per line
     $nid=3; # number of id


     @chs = (0,13,14,15);

     foreach $id (1..$nid) {
	 @{$module[$id]}=&repeatn($tdc1,$ndata);   #this fills all with the same number
	 @{$did[$id]}=&repeatn($did_t,$ndata);
	   $ch[$id][1] = $chs[$id]; #ch number, starting from 1
     }

     @name[1..3] = ("Single","Coin","Pileup");


} 


elsif ($category eq 'ppac') {
     $segment=2;    
     $ndata=5;
     $nid=3;# define F2 and F3B ->collapse

     foreach $id (1..$nid) {
	 if ($id ==1) {#F1 PPAC
	     @{$module[$id]}= &repeatn($adc1,$ndata);
	     @{$did[$id]}= &repeatn($did_a,$ndata);
	 }else {#F2,F3 PPACs
	     @{$module[$id]}= &repeatn($mhtdc,$ndata);
	     @{$did[$id]}= &repeatn($did_t,$ndata);
	 }

     }
     
     @{$ch[1]}[1..$ndata] = (0,1,2,3,15);#15..no input in reality,dummy
     @{$ch[2]}[1..$ndata] = (6,7,8,9,5);
     @{$ch[3]}[1..$ndata] = (1,2,3,4,0);
#     @{$ch[4]}[1..$ndata] = (7,8,9,10,6);

     @name[1] = "F1";
     @name[2] = "F2/F3 B";
     @name[3] = "F3 A";
#     @name[4] = "F3 B";

} 

elsif ($category eq 'dssd') {
    #double sided, V785 or MADC32
     $segment=4;    
     $ndata=2;
     $nid=64;

     foreach $id (1..$nid) {
	 $tid=($id%16)||16;  #id -> 1..16 

	 if ($id <= 16) {#Tel1 X
	     @{$module[$id]}[1..2]= ($madc1,$mhtdc);
	     $ch[$id][1] = -1+$tid; #ADC ch
	     $ch[$id][2] = 31+$tid; #TDC ch
	 }elsif ($id <= 32) {#Tel1 Y
	     @{$module[$id]}[1..2]= ($adc1,$mhtdc);
	     $ch[$id][1] = 15+$tid; #ADC ch
	     $ch[$id][2] = 47+$tid; #TDC ch
	 }elsif ($id <= 48) {#Tel2 Y
	     @{$module[$id]}[1..2]= ($adc2,$tdc1);
	     $ch[$id][1] = -1+$tid; #ADC ch
	     $ch[$id][2] = 15+$tid; #TDC ch
	 }else {#Tel3 Y
	     @{$module[$id]}[1..2]= ($adc2,$tdc2);
	     $ch[$id][1] = 15+$tid; #ADC ch
	     $ch[$id][2] = -1+$tid; #TDC ch
      	 }

	 @{$did[$id]}[1..2]= ($did_a,$did_t); #always adc/tdc

     }


     @telnames=("","Tel1 dE1X","Tel1 dE2Y", "Tel2 dE2Y", "Tel3 dE2Y");

     foreach $id (1..$nid) {
	 $ntel=(int(($id-1)/16)+1); 
	 $nstrip=($id%16)||16; 
	 @name[$id] = "$telnames[$ntel] $nstrip";
     }
} 

elsif ($category eq 'mssd') {
    #multiplexed ssd (MUX-32)
     $segment=5;    
     $ndata=5; #E x 2, pos x 2, T 
     $nid=6;

     foreach $id (1..$nid) {
	 @{$module[$id]}= &repeatn($madc2,$ndata);
	 @{$did[$id]}= &repeatn($did_a,$ndata);
	 @{$module[$id]}[5]= $tdc1;
	 @{$did[$id]}[5]= $did_t;
     }

     @{$ch[1]}[1..$ndata] = (0,1,2,3,2);
     @{$ch[2]}[1..$ndata] = (4,5,6,7,3);
     @{$ch[3]}[1..$ndata] = (8,9,10,11,4);
     @{$ch[4]}[1..$ndata] = (12,13,14,15,5);
     @{$ch[5]}[1..$ndata] = (16,17,18,19,6);
     @{$ch[6]}[1..$ndata] = (20,21,22,23,7);

     foreach $id (1..$nid) {
	 $nmux=(int(($id-1)/2)+1); 
	 $nsub=($id%2)||2; 
	 @name[$id] = "$nmux-$nsub";
     }
} 

elsif ($category eq 'ssd') {
     $segment=6;    
     $ndata=2;
     $nid=17;

     foreach $id (1..$nid) {
	 $ch_offset=0;	
	 if ($id <= 16) {#MSCF16-> V1190
	     @{$module[$id]}[1..2]= ($madc1,$mhtdc);
	     #if ($id>10) {$ch_offset =2;} change 1/17 okawa 
	     if ($id>10) {$ch_offset =0;} 
	     $ch[$id][1] = 15+$id+$ch_offset; #ADC ch
	     $ch[$id][2] = 15+$id+$ch_offset; #TDC ch
	 }  else{#F2ssd $id=17
	     @{$module[$id]}[1..2]= ($adc1,$mhtdc+1); # Warning: Specifying non-existing TDC
	     $ch[$id][1] = $id-13; #ADC ch
	     $ch[$id][2] = $id-13; #TDC ch
	 }
	 @{$did[$id]}[1..2]= ($did_a,$did_t); #always adc/tdc
   print "; $ch[$id][1] \n";


     }
     
     @name = ("","Tel1 E1","Tel1 E2","Tel2 E1","Tel2 E2",
	      "Tel3 E1","Tel3 E2","Tel4 E1","Tel4 E2",
	      "Tel5 E1","Tel5 E2","N/A","N/A",
	      "Lowgain 1","Lowgain 2","Lowgain 3","Lowgain 4",
	      "F2SSD");
} 





$filename="$category.map";

open (OUT,">$filename");


print OUT "# Map for $category\n";
print OUT "# Note: automatically produced with mapmaker.pl. Do not edit it directly\n";
foreach $id (1..$nid) {
    printf OUT "%d,%2d, ", $segment, $id;
    foreach $data (1..$ndata) {
	printf OUT "%d %d %2d %3d %3d, ",
		$device, $fp, $did[$id][$data], $module[$id][$data], $ch[$id][$data];
    }
    print OUT "; $category $name[$id] \n";
}

system("cat $filename"); 

sub repeatn {
#    return an array filled with $unit, index 0 to $n+1  
    my ($unit, $n) = @_;
    my @array=($unit) x ($n+1);
    return @array;

}


