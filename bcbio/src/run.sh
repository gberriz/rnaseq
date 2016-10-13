#BSUB -q priority
#BSUB -J PLACEHOLDER__REPLACE_WITH_THE_APPROPRIATE_PROJECT_NAME
#BSUB -oo ../final/main.%J.out
#BSUB -eo ../final/main.%J.err
#BSUB -n 1
#BSUB -R "rusage[mem=4024]"
#BSUB -W 48:00

bcbio_nextgen.py                                                          \
    ../config/PLACEHOLDER__REPLACE_WITH_THE_APPROPRIATE_PROJECT_NAME.yaml \
    -n 500                                                                \
    -t ipython                                                            \
    -s lsf                                                                \
    -q parallel                                                           \
    '-rW=52:00'                                                           \
    -r mincores=2                                                         \
    -rminconcores=2                                                       \
    --retries 3                                                           \
    --timeout 180                                                         \
    --tag PLACEHOLDER__REPLACE_WITH_THE_APPROPRIATE_PROJECT_NAME
