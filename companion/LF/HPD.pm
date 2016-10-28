# Highest posterior density helper module

package LF::HPD;

use Moose qw/has/;
use PDL;


has evolution => (is => 'rw', isa => 'Str');
has Lx        => (is => 'rw', isa => 'PDL');
has dimension => (is => 'rw', isa => 'Num');
has epsilon   => (is => 'rw', isa => 'Num');

# computed LF
has x  => (is => 'rw', isa => 'PDL');
has y  => (is => 'rw', isa => 'PDL');
has yt => (is => 'rw', isa => 'PDL');
has wt => (is => 'rw', isa => 'PDL');
has hx => (is => 'rw', isa => 'PDL');
has hy => (is => 'rw', isa => 'PDL');


# double powerlaw
has weight => (is => 'rw', isa => 'PDL');
has A      => (is => 'rw', isa => 'PDL');
has gamma1 => (is => 'rw', isa => 'PDL');
has gamma2 => (is => 'rw', isa => 'PDL');
has Lstar  => (is => 'rw', isa => 'PDL');

# LDDE + LADE
has zc => (is => 'rw', isa => 'PDL');
has p1 => (is => 'rw', isa => 'PDL');
has p2 => (is => 'rw', isa => 'PDL');

# LDDE only
has alfa => (is => 'rw', isa => 'PDL');
has La   => (is => 'rw', isa => 'PDL');

# LADE only
has ddd  => (is => 'rw', isa => 'PDL');

# PDLE
has etad => (is => 'rw', isa => 'PDL');
has etal => (is => 'rw', isa => 'PDL');


## same, but threaded versions

# double powerlaw
has Tweight => (is => 'rw', isa => 'PDL');
has TA => (is => 'rw', isa => 'PDL');
has Tgamma1 => (is => 'rw', isa => 'PDL');
has Tgamma2 => (is => 'rw', isa => 'PDL');
has TLstar => (is => 'rw', isa => 'PDL');

# LDDE + LADE
has Tzc => (is => 'rw', isa => 'PDL');
has Tp1 => (is => 'rw', isa => 'PDL');
has Tp2 => (is => 'rw', isa => 'PDL');

# LDDE only
has Talfa => (is => 'rw', isa => 'PDL');
has TLa => (is => 'rw', isa => 'PDL');

# LADE only
has Tddd => (is => 'rw', isa => 'PDL');

# PDLE
has Tetad => (is => 'rw', isa => 'PDL');
has Tetal => (is => 'rw', isa => 'PDL');





sub read_ldde {
    my $self = shift;
    my $f = shift;
    my ($weight,$A,$gamma1,$gamma2,$Lstar,$zc,$p1,$p2,$alfa,$La) = rcols($f);
    $self->weight($weight);
    $self->A($A);
    $self->gamma1($gamma1);
    $self->gamma2($gamma2);
    $self->Lstar($Lstar);
    $self->zc($zc);
    $self->p1($p1);
    $self->p2($p2);
    $self->alfa($alfa);
    $self->La($La);

    $self->evolution('ldde');
}

sub read_lade {
    my $self = shift;
    my $f = shift;
    my ($weight,$A,$gamma1,$gamma2,$Lstar,$zc,$p1,$p2,$ddd) = rcols($f);
    $self->weight($weight);
    $self->A($A);
    $self->gamma1($gamma1);
    $self->gamma2($gamma2);
    $self->Lstar($Lstar);
    $self->zc($zc);
    $self->p1($p1);
    $self->p2($p2);
    $self->ddd($ddd);

    $self->evolution('lade');
}

sub read_pdle {
    my $self = shift;
    my $f = shift;
    my ($weight,$A,$gamma1,$gamma2,$Lstar,$etad,$etal) = rcols($f);
    $self->weight($weight);
    $self->A($A);
    $self->gamma1($gamma1);
    $self->gamma2($gamma2);
    $self->Lstar($Lstar);
    $self->etad($etad);
    $self->etal($etal);

    $self->evolution('pdle');
}

sub read_noevol {
    my $self = shift;
    my $f = shift;
    my ($weight,$A,$gamma1,$gamma2,$Lstar) = rcols($f);
    $self->weight($weight);
    $self->A($A);
    $self->gamma1($gamma1);
    $self->gamma2($gamma2);
    $self->Lstar($Lstar);

    $self->evolution('noevol');
}

sub normalise_weights {
    my $self = shift;

    $self->weight( $self->weight / $self->weight->sum );
}



sub prepare_threads {
    my $self = shift;

    my $d = $self->Lx->dim(0);
    $self->dimension($d);

    $self->Tweight( $self->weight->slice("*$d") );
    $self->TA(      $self->A->slice("*$d") );
    $self->Tgamma1( $self->gamma1->slice("*$d") );
    $self->Tgamma2( $self->gamma2->slice("*$d") );
    $self->TLstar(  $self->Lstar->slice("*$d") );

    if ($self->evolution eq 'ldde') {
	$self->Tzc(     $self->zc->slice("*$d") );
	$self->Tp1(     $self->p1->slice("*$d") );
	$self->Tp2(     $self->p2->slice("*$d") );
	$self->Talfa(   $self->alfa->slice("*$d") );
	$self->TLa(     $self->La->slice("*$d") );

    } elsif ($self->evolution eq 'lade') {
	$self->Tzc(     $self->zc->slice("*$d") );
	$self->Tp1(     $self->p1->slice("*$d") );
	$self->Tp2(     $self->p2->slice("*$d") );
	$self->Tddd(    $self->ddd->slice("*$d") );

    } elsif ($self->evolution eq 'pdle') {
	$self->Tetad(     $self->etad->slice("*$d") );
	$self->Tetal(     $self->etal->slice("*$d") );
    }

    my $d1 = $self->A->dim(0);
    my $x = $self->Lx->slice("*$d1")->transpose;
}


sub calc {
    my $self = shift;
    my $z = shift;

    if ($self->evolution eq 'ldde') {
	$self->calc_ldde($z);
    } elsif ($self->evolution eq 'lade') {
	$self->calc_lade($z);
    } elsif ($self->evolution eq 'pdle') {
	$self->calc_pdle($z);
    } elsif ($self->evolution eq 'noevol') {
	$self->calc_noevol;
    }

    # exchange dimensions
    $self->yt( $self->y->transpose );
    $self->wt( $self->Tweight->transpose );

}

sub weighted_hist {
    my $self = shift;
    my $i = shift;

    my $yt = $self->yt;
    my $wt = $self->wt;
    my ($hx,$hy) = whist( $yt->slice(":,$i"), $wt->slice(":,$i"), -8, -2.5, .01 );
    $self->hx($hx);
    $self->hy($hy);
}




sub calc_lade {
    my $self = shift;
    my $z = shift;  # a scalar

    my $Lx= $self->Lx;  # a piddle

    # LADE: luminosity evolution
    my $t = (1+$self->Tzc)/(1+$z);

    # normalize the evolution so that el(z=0)=1, following Fotopoulou et al.
    # (NB Aird 2010 does not normalize)
    my $norm = (1+$self->Tzc)**$self->Tp1 + (1+$self->Tzc)**$self->Tp2;

    my $el = -log10( ($t**$self->Tp1 + $t**$self->Tp2) / $norm );

    # double powerlaw
    # this is dFi/dLogX in Ebrero+2009
    my $doublepowerlaw = -6.+$self->TA
       - log10( 10.**(($Lx-$self->TLstar-$el)*$self->Tgamma1)
	      + 10.**(($Lx-$self->TLstar-$el)*$self->Tgamma2) );

    # density evolution
    my $led = $self->Tddd*(1+$z);

    $doublepowerlaw += $led;

    $self->y($doublepowerlaw);
}


sub calc_ldde {
    my $self = shift;
    my $z = shift;  # a scalar

    my $Lx= $self->Lx;  # a piddle

    # this is dFi/dLogX in Ebrero+2009
    my $doublepowerlaw = -6.+$self->TA
       - log10( 10.**(($Lx-$self->TLstar)*$self->Tgamma1)
	      + 10.**(($Lx-$self->TLstar)*$self->Tgamma2) );

    # LDDE
    my $zclx = $self->Tzc + zeroes $Lx;
    my $vals = 10.**($self->Talfa*($Lx-$self->TLa));
    my $msk = $Lx < $self->TLa;
    $zclx->where( $msk ) *= $vals->where( $msk );

    my $lddevol = zeroes $Lx;
    $msk = $z <= $zclx;
    $lddevol->where($msk) .= $self->Tp1->where($msk) * log10(1.+$z);

    $vals = $self->Tp1*log10(1.+$zclx) + $self->Tp2*log10( (1.+$z)/(1.+$zclx) );
    $lddevol->where(!$msk) .= $vals->where(!$msk);


    $self->y( $doublepowerlaw + $lddevol );
}


sub calc_pdle {
    my $self = shift;
    my $z = shift;  # a scalar

    my $Lx= $self->Lx;  # a piddle


    # PLE
    my $Tlumstar = $self->TLstar + $self->Tetal * (1+$z);

    # this is dFi/dLogX in Ebrero+2009
    my $doublepowerlaw = -6.+$self->TA
       - log10( 10.**(($Lx-$Tlumstar)*$self->Tgamma1)
	      + 10.**(($Lx-$Tlumstar)*$self->Tgamma2) );

    # PDE
    $doublepowerlaw += $self->Tetad * (1+$z);


    $self->y($doublepowerlaw);
}


sub calc_noevol {
    my $self = shift;

    my $Lx= $self->Lx;  # a piddle

    # this is dFi/dLogX in Ebrero+2009
    my $doublepowerlaw = -6.+$self->TA
       - log10( 10.**(($Lx-$self->TLstar)*$self->Tgamma1)
	      + 10.**(($Lx-$self->TLstar)*$self->Tgamma2) );

    $self->y($doublepowerlaw);
}



sub findHDI {
    # find Highest Density Intervals, interpolating on the
    # probability vectors

    # see here for the algorithm:
    # https://stat.ethz.ch/pipermail/r-help/2007-November/146688.html

    my $self = shift;
    my $lev = shift; # probability level (e.g.: .95)
    my $hx = $self->hx;  # abscissas of probability density
    my $hy = $self->hy;  # ordinate of probability density

    # find ordinates corresponding to level
    my $levy = $hy->max;
    $levy -= $self->epsilon while ($hy->where($hy>$levy)->sum < $lev);

    # find abscissas
    my $half = $hy->maximum_ind;
    my $lower = vsearch( $levy, $hy->slice("0:$half") );
    my $upper = vsearch( $levy, $hy->slice("$half:-1") );

    my $hdi = pdl( [$hx->at($lower),$hx->at($half+$upper)] );
    return $hdi;
}


1;
