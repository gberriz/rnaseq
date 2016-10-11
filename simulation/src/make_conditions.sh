#!/usr/bin/env zsh

# src/make_conditions.sh io/mh 500 32000 125 8000

basedir=$1
gmin=$2
gmax=$3
cmin=$4
cmax=$5

# -----------------------------------------------------------------------------

set -x
metadata=$basedir/._/metadata

[[ -d $metadata ]] || {
    echo "there's no directory $metadata" >&2
    exit 1
}
set +x

# -----------------------------------------------------------------------------

make_touch () {
    path_=$1
    mkdir -vp $( dirname $path_ )
    touch $path_
}

# -----------------------------------------------------------------------------

gformat=$( printf '%%0%dd' $#gmax )
cformat=$( printf '%%0%dd' $#cmax )

g=$gmin

while (( g <= gmax ))
do
    c=$cmin

    while (( c <= cmax ))
    do

        inputdir=$( printf "$basedir/$gformat/$cformat" $g $c )

        mkdir -p $inputdir
        rln -fvs $metadata $inputdir

        for method in deseq2 edger
        do

            make_touch $inputdir/results/$method/.factor/method
            for treatment in T1 T2
            do
                make_touch $inputdir/results/$method/simulation/$treatment/.factor/treatment
            done

        done

        make_touch $inputdir/.factor/expected_number_of_counts_per_gene


        c=$(( 2 * c ))
    done
    make_touch $( dirname $inputdir )/.factor/number_of_genes

    g=$(( 2 * g ))
done
