#!/bin/bash

export LM_TO_STORE_DIR="language_models"
export NEW_DEV_DATA_TO_STORE_DIR="dev_for_rerank"

export JO_TAGS_DIR="/datastore/for_milos"
export DATA_DIR="/home/mstanoj1/experiments/2016_LIMSI_cooperation_2/all_data/data_converted_max50"
export DEV_DATA_DIR="/home/mstanoj1/experiments/2016_LIMSI_cooperation_2/all_data/devdata_converted_max50"
export SOURCE_PERMUTATION_FILE="$DATA_DIR/newsco.en2de.unfoldNULL.permutation"


export MY_CODE_DIR="/home/mstanoj1/experiments/2016_LIMSI_cooperation_2/en-de_jo/reranker"
export KENLM_DIR="$MY_CODE_DIR/dependencies/kenlm"

mkdir -p $LM_TO_STORE_DIR

######### CREATING RAW PERMUTED DATA ########

### feature 1,2,3,4
for X in \
	$DATA_DIR/newsco.en2de.unfoldNULL.source \
	$JO_TAGS_DIR/newsco.en2de.unfoldNULL.source.clausetypes \
       	$JO_TAGS_DIR/newsco.en2de.unfoldNULL.source.source_pos \
	$JO_TAGS_DIR/newsco.en2de.unfoldNULL.source.sprimem_withpos_allwords \
	; do
	echo $MY_CODE_DIR/scripts/permute.pl $SOURCE_PERMUTATION_FILE $X $LM_TO_STORE_DIR/`basename $X`.raw
	$MY_CODE_DIR/scripts/permute.pl $SOURCE_PERMUTATION_FILE $X > $LM_TO_STORE_DIR/`basename $X`.raw
done

### feature 5
$MY_CODE_DIR/scripts/merge_jo_pos_tags.pl \
	$LM_TO_STORE_DIR/newsco.en2de.unfoldNULL.source.clausetypes.raw \
       	$LM_TO_STORE_DIR/newsco.en2de.unfoldNULL.source.source_pos.raw \
	$LM_TO_STORE_DIR/newsco.en2de.unfoldNULL.source.sprimem_withpos_allwords.raw \
	> $LM_TO_STORE_DIR/newsco.en2de.combined_POS.raw

######### CREATING LANGUAGE MODEL ########

for X in \
	newsco.en2de.unfoldNULL.source \
	newsco.en2de.unfoldNULL.source.clausetypes \
       	newsco.en2de.unfoldNULL.source.source_pos \
	newsco.en2de.unfoldNULL.source.sprimem_withpos_allwords \
	newsco.en2de.combined_POS \
	; do
	$KENLM_DIR/bin/lmplz -o 5 < $LM_TO_STORE_DIR/${X}.raw > $LM_TO_STORE_DIR/${X}.arpa
	$KENLM_DIR/bin/build_binary $LM_TO_STORE_DIR/${X}.arpa $LM_TO_STORE_DIR/${X}.binary
done


######### CREATING DEV DATA ########

mkdir $NEW_DEV_DATA_TO_STORE_DIR

cp $DEV_DATA_DIR/nt10.en2de.unfoldNULL.permutation $NEW_DEV_DATA_TO_STORE_DIR

cp $DEV_DATA_DIR/nt10.en2de.unfoldNULL.source $NEW_DEV_DATA_TO_STORE_DIR
cp $JO_TAGS_DIR/nt10.en2de.unfoldNULL.source.clausetypes $NEW_DEV_DATA_TO_STORE_DIR
cp $JO_TAGS_DIR/nt10.en2de.unfoldNULL.source.source_pos $NEW_DEV_DATA_TO_STORE_DIR
cp $JO_TAGS_DIR/nt10.en2de.unfoldNULL.source.sprimem_withpos_allwords $NEW_DEV_DATA_TO_STORE_DIR

$MY_CODE_DIR/scripts/merge_jo_pos_tags.pl \
	$JO_TAGS_DIR/nt10.en2de.unfoldNULL.source.clausetypes \
       	$JO_TAGS_DIR/nt10.en2de.unfoldNULL.source.source_pos \
	$JO_TAGS_DIR/nt10.en2de.unfoldNULL.source.sprimem_withpos_allwords \
	> $NEW_DEV_DATA_TO_STORE_DIR/nt10.en2de.combined_POS.raw

######### CREATING TEST DATA ########

cp $DEV_DATA_DIR/nt09.en2de.unfoldNULL.permutation $NEW_DEV_DATA_TO_STORE_DIR

cp $DEV_DATA_DIR/nt09.en2de.unfoldNULL.source $NEW_DEV_DATA_TO_STORE_DIR
cp $JO_TAGS_DIR/nt09.en2de.unfoldNULL.source.clausetypes $NEW_DEV_DATA_TO_STORE_DIR
cp $JO_TAGS_DIR/nt09.en2de.unfoldNULL.source.source_pos $NEW_DEV_DATA_TO_STORE_DIR
cp $JO_TAGS_DIR/nt09.en2de.unfoldNULL.source.sprimem_withpos_allwords $NEW_DEV_DATA_TO_STORE_DIR

$MY_CODE_DIR/scripts/merge_jo_pos_tags.pl \
	$JO_TAGS_DIR/nt09.en2de.unfoldNULL.source.clausetypes \
       	$JO_TAGS_DIR/nt09.en2de.unfoldNULL.source.source_pos \
	$JO_TAGS_DIR/nt09.en2de.unfoldNULL.source.sprimem_withpos_allwords \
	> $NEW_DEV_DATA_TO_STORE_DIR/nt09.en2de.combined_POS.raw






