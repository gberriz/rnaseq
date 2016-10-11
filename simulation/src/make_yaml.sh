#!/usr/bin/env zsh

# src/make_yaml.sh io/mh 500 32000 125 8000

basedir=$1
gmin=$2
gmax=$3
cmin=$4
cmax=$5

# -----------------------------------------------------------------------------

relpath () {
    local target=$1
    local reference=$2
    realpath -s $target --relative-to=$reference
}

make_yaml () {

    local inputdir=${1:A}

    local method=$2

    local yamlfile=$inputdir/sartools/$method/parameters.yaml

    mkdir -vp $( dirname $yamlfile )

    local workdir=$inputdir/results/$method/sartools

    local targetfile=$( relpath $inputdir/target/data.tsv $workdir )
    local     rawdir=$( relpath $inputdir/counts          $workdir )

    cat <<EOF > $yamlfile
## workDir: C:/path/to/your/working/directory/
workDir: $workdir

## projectName: projectName
## author: Your name
## targetFile: target.txt
targetFile: $targetfile

## rawDir: raw
rawDir: $rawdir

## featuresToRemove:
## - alignment_not_unique
## - ambiguous
## - no_feature
## - not_aligned
## - too_low_aQual
## varInt: group
## condRef: WT
condRef: U

## batch: ~
## fitType: parametric
## cooksCutoff: yes
## independentFiltering: yes
## alpha: 0.05
## pAdjustMethod: BH
## typeTrans: VST
## locfunc: median
## colors:
## - dodgerblue
## - firebrick1
## - MediumVioletRed
## - SpringGreen
EOF

  # perl -lne 'print unless /^\s*(?:#|$)/' $yamlfile

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
        [[ -d $inputdir ]] || {
            echo "there's no directory $inputdir" >&2
            exit 1
        }

        for method in deseq2 edger
        do
            make_yaml $inputdir $method
        done

        c=$(( 2 * c ))
    done

    g=$(( 2 * g ))
done
