#!/bin/bash
cat <<EOF >&2
THIS SCRIPT IS FOR ILLUSTRATION PURPOSES ONLY!
exiting...
EOF
exit 1

# -----------------------------------------------------------------------------
# to access bcbio-nextgen

unset PYTHONHOME PYTHONPATH
module load dev/java/jdk1.7
export PATH=/opt/bcbio/centos/bin:$PATH

# -----------------------------------------------------------------------------
# definitions

BASEDIR=PLACEHOLDER__REPLACE_WITH_THE_APPROPRIATE_BASE_DIRECTORY
PROJECTNAME=PLACEHOLDER__REPLACE_WITH_THE_APPROPRIATE_PROJECT_NAME

# .............................................................................

INPUTDIR=$BASEDIR/input
OUTPUTDIR=$BASEDIR/output
MERGEDDIR=$OUTPUTDIR/merged

MERGESPEC=merge_spec

# -----------------------------------------------------------------------------
# merge files

# the following command will generate a file $INPUTDIR/${MERGESPEC}-merged.csv
bcbio_prepare_samples.py --csv "$INPUTDIR/$MERGESPEC.csv" --out "$MERGEDDIR"

# symlink to output; this avoids ending up with the ${MERGESPEC}-merged prefix
# throughout subsequent output
ln -s -- "$INPUTDIR/${MERGESPEC}-merged.csv" "$INPUTDIR/$PROJECTNAME.csv"

# -----------------------------------------------------------------------------
# generate config file (the following will generate the file
# $OUTPUTDIR/bcbio_nextgen/$PROJECTNAME/config/$PROJECTNAME.yaml)
(
  cd "$OUTPUTDIR/bcbio_nextgen"
  bcbio_nextgen.py                   \
    -w template                      \
    "$INPUTDIR/illumina-rnaseq.yaml" \
    "$INPUTDIR/$PROJECTNAME.csv"     \
    "$MERGEDDIR/"*gz
)

# -----------------------------------------------------------------------------
# main run
(
  cd "$OUTPUTDIR/bcbio_nextgen/$PROJECTNAME/work"
  bsub < "$BASEDIR/SRC/run.sh"
)
