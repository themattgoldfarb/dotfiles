#!/bin/bash

DIR=~/dotfiles/bash_files # dotfiles directory
OLD_DIR=~/dotfiles_old             # old dotfiles backup directory
DEST_DIR=~/

ROOT="$(pwd)"



## list of files/folders to symlink in homedir
#files="calendar.desktop cool-retro-term.desktop \
       #google-play-music.desktop inbox.desktop"

############

FILES=()



__move_file() {
  file=$1
  target=$2
  dir=$3
  dest_dir=$4
  bkp_dir=$5
  #mkdir -p $dest_dir
  #mkdir -p $bkp_dir
  if [[ -e "$dest_dir/$target" ]] ; then
    printf "  '%s' already exists in '%s'.  Moving existing to '%s'..." \
      "$target" "$dest_dir" "$bkp_dir"
    #mv $dest_dir/$target $bkp_dir/$target
    echo "done."
  fi
  printf "  Creating symlink '%s' to file '%s' in '%s'..." \
    "$target" "$file" "$dest_dir"
  #ln -s $dir/$file $dest_dir/$target
  echo "done."
}

dir=${1:-$DIR}
bkp_dir=${2:-$OLD_DIR}
dest_dir=${3:-$DEST_DIR}
root=${4:-$ROOT}


# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"


excludes=("dotconfig")
remaps=()
dots=()

[[ -e "dotconfig" ]] && \
while IFS='' read -r line || [[ -n "$line" ]]; do
  [[ "$line" =~ ^! ]] && excludes+=("${line:1}")
  [[ "$line" =~ "->" ]] && remaps+=("$line")
  [[ "$line" =~ ^\. ]] && dots+=("${line:1}")
  [[ "$line" =~ ^DEST= ]] && dest_dir="$HOME/$(echo $line | sed 's,DEST=,,')"
  [[ "$line" =~ ^BKP= ]] && bkp_dir="$HOME/$(echo $line | sed 's,BKP=,,')"
done < dotconfig

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
  __move_file $file $file $dir $dest_dir $bkp_dir
done

for file in ${dots[@]} ; do
  [[ -e "$file" ]] && __move_file $file ".$file" $dir $dest_dir $bkp_dir
done

for remap in ${remaps[@]} ; do
  file=$(echo $remap | sed 's,->.*,,')
  remap=$(echo $remap | sed 's,.*->,,')
  [[ -e "$file" ]] && __move_file $file $remap $dir $dest_dir $bkp_dir
done

directories=$(find . -maxdepth 1 -type d -printf "%f\n")
for directory in $directories ; do
  if [[ $directory != "." ]] ; then
    echo "runnning ....."
    cmd="$root/installlinks.sh $dir/$directory $bkp_dir/$directory $dest_dir/$directory $root"
    exec $cmd
    echo "hello"

  fi
done

