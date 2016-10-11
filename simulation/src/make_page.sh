#!/usr/bin/env zsh

# src/make_page.sh io/mh $HOME/public_html/rnaseq/simulation

basedir=$1
target=$2

$commands[find] $basedir/**/simulation -name plot.png | \
    while IFS= read -r plot
    do
       thumbnail=${plot/plot/thumbnail}
       echo $(dirname $plot)
       convert -thumbnail 200 $plot $thumbnail
    done

for n in $basedir/**/thumbnail.png
do
    d=$( dirname $n )
    e=${${${${d/results\//}/simulation\//}/$basedir/$target/img}%/*}
    echo $e
    mkdir -p $e
    ln -vfs ${d:A} $e
done

PYTHONPATH=$HOME/_/projects/gbmi/lib/python \
    python src/tabulate.py $target/img $target
