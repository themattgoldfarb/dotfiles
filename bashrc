# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# ANSI color codes
RS="\[\033[0m\]"    # reset
HC="\[\033[1m\]"    # hicolor
UL="\[\033[4m\]"    # underline
INV="\[\033[7m\]"   # inverse background and foreground
FBLK="\[\033[30m\]" # foreground black
FRED="\[\033[31m\]" # foreground red
FGRN="\[\033[32m\]" # foreground green
FYEL="\[\033[33m\]" # foreground yellow
FBLE="\[\033[34m\]" # foreground blue
FMAG="\[\033[35m\]" # foreground magenta
FCYN="\[\033[36m\]" # foreground cyan
FWHT="\[\033[37m\]" # foreground white
BBLK="\[\033[40m\]" # background black
BRED="\[\033[41m\]" # background red
BGRN="\[\033[42m\]" # background green
BYEL="\[\033[43m\]" # background yellow
BBLE="\[\033[44m\]" # background blue
BMAG="\[\033[45m\]" # background magenta
BCYN="\[\033[46m\]" # background cyan
BWHT="\[\033[47m\]" # background white

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) exit;;
esac

#PROMPT_COMMAND=prompt_command

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=-1
HISTFILESIZE=-1

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

is_hg() {
  hg log &> /dev/null && echo true || echo false
}

hg_message() {
    message=$(hg log -r "p1()" 2> /dev/null | grep "^summary:.*" | sed "s/summary:\s//")
    message=$(echo $message)
    line=$(hg xl 2> /dev/null | grep "^@")
    commit=$(echo $line | sed -E 's,^@ *(\S*).*,\1,')
    cl=$(echo $line | sed -E 's,.*(http[^\s>]*).*,\1,')
    output=$(echo "$commit $cl" | sed -E 's,(\S+) (\S+), \(\1 - \2\),')
    echo " ($message)"
}

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

arrow_prompt=$'  \[\e[1m\]\u2192\[\e[0m\] '

if [ "$SSH_CLIENT"] || [ "$SSH_TTY" ];
then
  host="\h"
else
  host="local"
fi

if [ "$color_prompt" = yes ]; then
  PS1="\[\e[92m\e[1m\]##--< \[\e[0m\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@${host}\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\[\033[01;32m\]\[\033[01;91m\]\$(hg_message)\$(parse_git_branch)\[\033[00m\e[92m\e[1m\] (\t) >--##\[\e[0m\]\n$arrow_prompt"
    PS2="  \[\e[92m\]$arrow_prompt"
    #PS2="&gt; "
else
    PS1='${debian_chroot:+($debian_chroot)}\u@${host}:\w\$ '
    #PS2="&gt; "
    #PS2=$'  \u2192 '
    PS2="  $arrow_prompt"
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# use vim keybindings
set -o vi

[[ -f "$HOME/.bashrc_work" ]] &&    source "$HOME/.bashrc_work"
[[ -f "$HOME/.bash_files/alert_on_long_running" ]] && source "$HOME/.bash_files/alert_on_long_running"

[[ -f "$HOME/.bash_files/bash_functions" ]] && source "$HOME/.bash_files/bash_functions"
[[ -f "$HOME/.bash_files/bash_aliases" ]] &&   source "$HOME/.bash_files/bash_aliases"
[[ -f "$HOME/.bash_files/bash_env" ]] &&       source "$HOME/.bash_files/bash_env"

[[ -f "$HOME/.fzf.bash" ]] && source ~/.fzf.bash
