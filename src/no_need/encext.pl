#!/usr/bin/perl

# Extract Analyzer descriptions
# from 'enc*.f' files automatically.
#

use strict;
use warnings;
use IO::File;

my $i = 0;
my @files;
{
    @files =  split(/\s+/,`ls enc*.f`);
}

my @patterns =
    (
     '^c-----------------------',
     '^c\s*ANALYZER\s*([0-9]+)',
     '^c-----------------------',
     );

my $preamble;
my $j;
#@files = ("src/enccoin.f");

my @a = ();
foreach my $f (@files){
    my $io = IO::File->new($f, 'r') or die$!;
    $i = 0;
    undef $j;
    $preamble = "";
    foreach my $line ($io->getlines){
#	print STDERR $line;
	if($i >= 3){
	    if($i==3){
		if(defined($j)){
		    push @{$a[$j]}, $preamble;
		}
		++$i;
	    }
	    if($line=~m/^c/){
		if(defined($j)){
		    push @{$a[$j]}, $line;
		}
	    }	    
	    if($line=~m/^c\s*---------------/){
		last;
	    }
	}elsif( $line=~m/$patterns[$i]/){
	    if($i==1 && defined($1)){
		$j = $1;
	    }
	    ++$i;

	    $preamble .= $line;
	}else{
	    $i = 0;
	    $preamble = "";
	}
    }
}

for my $t (@a){
    if($#{$t}>0){
	print @$t, "\n" x 5;
    }
}
