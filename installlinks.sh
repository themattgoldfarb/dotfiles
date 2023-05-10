#!/bin/bash

DIR=~/dotfiles/ # dotfiles directory
OLD_DIR=~/dotfiles_old/             # old dotfiles backup directory
DEST_DIR=~/
BASE_DIR=~/

DEBUG=1
NOMOVE=0

ROOT="$DIR"

## list of files/folders to symlink in homedir
#files="calendar.desktop cool-retro-term.desktop \
       #google-play-music.desktop inbox.desktop"

############

FILES=()


__log() {
  [[ DEBUG -eq 1 ]] && echo $@
}

__logn() {
  [[ DEBUG -eq 1 ]] && echo -n $@
}

__move_file() {
  file=$1
  target=$2
  dir=$3
  dest_dir=$4
  bkp_dir=$5
  base_dir=$6

  [[ $dir =~ /$ ]] && dir=${dir%/}
  [[ $dest_dir =~ /$ ]] && dest_dir=${dest_dir%/}
  [[ $bkp_dir =~ /$ ]] && bkp_dir=${bkp_dir%/}
  [[ $base_dir =~ /$ ]] && dir=${dir%/}

  __logn "  $dir/$file -> $dest_dir/$target..."
  [[ $NOMOVE -eq 0 ]] && mkdir -p $dest_dir
  [[ $NOMOVE -eq 0 ]] && mkdir -p $bkp_dir
  if [[ -e "$dest_dir/$target" || -L "$dest_dir/$target" ]] ; then
    __logn "moving..." 
 
  [[ $NOMOVE -eq 0 ]] && mv $dest_dir/$target $bkp_dir/$target
    __logn "moved..."
  fi
  [[ $NOMOVE -eq 0 ]] && ln -s $dir/$file $dest_dir/$target
  __log " done."
}



dir=${1:-$DIR}
bkp_dir=${2:-$OLD_DIR}
dest_dir=${3:-$DEST_DIR}
root=${4:-$ROOT}
base_dir=${5:-$BASE_DIR}

[[ $dir =~ /$ ]] && dir=${dir%/}
[[ $dest_dir =~ /$ ]] && dest_dir=${dest_dir%/}
[[ $bkp_dir =~ /$ ]] && bkp_dir=${bkp_dir%/}
[[ $root =~ /$ ]] && root=${root%/}
[[ $base_dir =~ /$ ]] && base_dir=${base_dir%/}

diff=${dir:${#root}}

# change to the dotfiles directory
__logn "Changing to the $dir directory ..."
cd $dir
__log "done"


excludes=("dotconfig")
remaps=()
dots=()
host=$(hostname | sed 's,\..*,,')

[[ -e "$root/dotconfig" ]] && \
while IFS='' read -r line || [[ -n "$line" ]]; do
  [[ "$line" =~ ^# ]] && continue

  if [[ "$line" =~ ^[$] ]] ; then
    line_host=$(echo $line | sed 's,[!/].*,,')
    line_host=${line_host:1}
    if [[ "$line_host" == "$host" ]] ; then
      line=$(echo $line | sed 's,[^!/]*,,')
    else
      line=$(echo $line | sed 's,[^!/]*,,')
      line=$(echo $line | sed 's,->.*,,')
      line="!$line"
    fi
  fi


  if [[ ${#diff} > 0 ]] ; then
    [[ ! $line =~ $diff ]] && continue
    [[ $line =~ ^[.!]?$diff ]] && line="$(echo $line | sed "s,$diff/,,")"
  fi




  [[ "$line" =~ ^! ]] && excludes+=("${line:1}")
  [[ "$line" =~ "->" ]] && remaps+=("$line")
  [[ "$line" =~ ^\. ]] && dots+=("${line:1}")
  [[ "$line" =~ ^DEST= ]] && dest_dir="$base_dir/$(echo $line | sed 's,DEST=,,')"
  [[ "$line" =~ ^BKP= ]] && bkp_dir="$HOME/$(echo $line | sed 's,BKP=,,')"
done < $root/dotconfig

raw_files=$( find . -maxdepth 1 -type f -printf "%f\n" )

files=()
for file in $raw_files ; do
  nomatch=true
  for exclude in "${excludes[@]}" ; do
    [[ $file == $exclude ]] && nomatch=false
  done
  for dot in "${dots[@]}" ; do
    [[ $file == $dot ]] && nomatch=false
  done
  for remap in "${remaps[@]}" ; do
    remap_file=$(echo $remap | sed 's,->.*,,')
    [[ $file == $remap_file ]] && nomatch=false
  done
  [[ $nomatch == true ]] && files+=($file)
done;

for file in ${files[@]} ; do
  __move_file $file "$file" $dir $dest_dir $bkp_dir
done

for file in ${dots[@]} ; do
  [[ -e "$file" ]] && __move_file $file ".$file" $dir $dest_dir $bkp_dir
done

for remap in ${remaps[@]} ; do
  file=$(echo $remap | sed 's,->.*,,')
  remap=$(echo $remap | sed 's,.*->,,')
  [[ -e "$file" ]] && __move_file $file $remap $dir $dest_dir $bkp_dir
done


directories=$(find . -maxdepth 1 -type d -not -path '*/\.*' -printf "%f\n")
for directory in $directories ; do
  if [[ $directory != "." ]] ; then
    $root/installlinks.sh "$dir/$directory" "$bkp_dir/$directory" "$dest_dir/$directory" "$root" "$dest_dir"

  fi
done
