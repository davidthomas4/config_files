# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# *{{ Default options

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# }}*

# *{{ PS1

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

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

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

PROMPT_DIRTRIM=3

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
*)
    ;;
esac

if [ -n "$SSH_CONNECTION" ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;36m\]\u@\h\[\033[00m\]:\[\033[01;35m\]\w\[\033[00m\]\$ '
fi

# }}*

# *{{ Colour support

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# }}*

# *{{ Misc.

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

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

# }}*

# *{{ mkcd

# make directory and cd into in one go
mkcd () {
mkdir -p "$*"
cd "$*"
}

# }}*

# *{{ CASINO/CASTEP

[ -e "$HOME/.bashrc.casino" ] && source "$HOME/.bashrc.casino"
export PATH=$PATH:$HOME/CASTEP_19.11/obj/linux_x86_64_gfortran9.0--mpi
export PATH=$PATH:$HOME/CASTEP_19.11/bin/linux_x86_64_gfortran9.0--mpi
export PATH=$PATH:$HOME/Downloads/q-e-qe-6.7.0/bin

# }}*

# *{{ Aliases

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

alias desktop='ssh David@192.168.0.32'
alias pyb6='ssh -X thomas@pyb075000006.lancs.ac.uk'
alias pyb7='ssh -X thomas@pyb075000007.lancs.ac.uk'
alias pyb8='ssh -X thomas@pyb075000008.lancs.ac.uk'
alias pyb10='ssh -X thomas@pyb075000010.lancs.ac.uk'
alias pyb11='ssh -X thomas@pyb075000011.lancs.ac.uk'
alias pyc11='ssh -X thomas@pyc008000011.lancs.ac.uk'
alias pyc17='ssh -X thomas@pyc008000017.lancs.ac.uk'
alias hec='ssh -X thomasd3@wayland.hec.lancs.ac.uk'
alias ius='ssh -Y thomasd3@ius.lancs.ac.uk'
alias archer='ssh -X -l thomasd3 login.archer.ac.uk'
alias csd3='ssh -X -i ~/.ssh/id_rsa.tier2 cs-thom1@login-cpu.hpc.cam.ac.uk'
alias rsynchec='rsync -r thomasd3@wayland.hec.lancs.ac.uk:/storage/hpc/44/thomasd3/ HEC'
alias rsynchecsc='rsync -r thomasd3@wayland.hec.lancs.ac.uk:/scratch/hpc/44/thomasd3/ HEC'
alias sl='ls'
alias hdrive='smbclient //homes/44/ -W LANCS -U thomasd3 -D thomasd3'
alias fitter_mc='~/Documents/Software/Fitter/fitter_mc'
alias gitb='git branch -l'
alias castep='mpirun ~/CASTEP_19.11/bin/linux_x86_64_gfortran9.0--mpi/castep.mpi'
alias protect='protect'
alias rm='rm-p'
alias nautilus='nautilus --no-desktop'
alias vi='/usr/bin/vim'
alias ci='/usr/bin/vim'
alias vb='vi ~/.bashrc'
alias vv='vi ~/.vimrc'
alias seagatemount='sudo mount -t ntfs /dev/sdb1 ~/Seagate_Drive'
alias seagateunmount='sudo umount ~/Seagate_Drive'
alias seagatesync='rsync -avu /home/thomas/Documents/ /home/thomas/Seagate_Drive'
alias tailf='tail -f'
alias python='python3'
alias synchecscratch='rsync -r --exclude-from=QMC_exclude thomas@pyc008000011.lancs.ac.uk:/home/thomas/Documents/HEC_scratch/ ~/Documents/HEC_scratch/'
alias uomvpm='globalprotect connect --portal vpnconnect.manchester.ac.uk'
alias uomvpmoff='globalprotect disable'

# }}*

# *{{ Navigation options

shopt -s autocd
shopt -s cdspell
shopt -s extglob
shopt -s dirspell
shopt -s cdable_vars
shopt -s checkwinsize

bind "set completion-ignore-case on"
bind "set show-all-if-ambiguous on"
bind "set show-all-if-unmodified on"

# }}*

# *{{ history searching
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# }}*

# *{{ stop rm
alias rm='echo -e "[rm/tr]\"rm\" has been \
	routed elsewhere, use \"tr\" (alias to trash-put)."'
alias RM='/bin/rm -I'  # use RM if you really want to rm
alias tr='gio trash'   # tr to trash
alias trash='cd ~/.local/share/Trash/files/' # go to trash directory,
#you'll want to empty this every once in a while with an "RM"

# }}*

# *{{ general purpose file extractor
function extract() {

if [ -f $1 ] ; then
   echo -e "[extract] Preparing $1."
   case $1 in
     *.tgz | *.tar.gz)
                tar xvzf $1 2>&1 ;;
     *.tbz | *.tbz2 | *.tar.bz2)
                tar xvjf $1 2>&1 ;;
     *.gz)      gunzip   $1 2>&1 ;;
     *.tar)     tar xvf  $1 2>&1 ;;
     *.bz2)     bunzip2  $1 2>&1 ;;
     *.rar)     unrar x  $1 2>&1 ;;
     *.zip)     unzip    $1 2>&1 ;;
     *.7z)      7z x     $1 2>&1 ;;
     *.xz)      unxz     $1 2>&1 ;;
     *.lzma)    unlmza   $1 2>&1 ;;
     *) echo -e "[extract] File \"$1\" is not valid."
        return 1
   esac
   echo -e "[extract] Done extracting $1."
else
   echo -e "[extract] File \"$1\" doesn't exist."
fi
}

# }}*

# *{{ make dodgy/god-awful file name formatting look OK
function ffix {
if [ -f "$*" ] || [ -d "$*" ]; then
  new_name="$(echo -n "$*" |             # read all args as a filename
              \tr '[A-Z]' '[a-z]' |      # lower-case everything
              sed 's/\(-\|\ \|+\)/_/g')" # replace ('-',' ','+') -> '_'
  mv -i "$*" "$new_name"                 # rename original
  echo -e "[ffix] File "$*" has been ffixed."
else
  echo -e "[ffix] File "$*" doesn't exist!"
fi
}
# }}*

# *{{ To add keys to remote host (passwordless login):
# ------------------------------------------------------
# 1) generate keys,
#  $ ssh-keygen -t rsa
#    (place by default in ~/.ssh/id_rsa)
# 2) place a key on HST, USR being your username
#  $ cat .ssh/id_rsa.pub | ssh USR@HST 'cat >> .ssh/authorized_keys'
#    (assumes .ssh directory exists in HST home, if it doesn't, make it)
#
# }}*

# *{{ Folder sizes
# du -sh - size of current folder
#      - list of subfolder sizes
# }}*

[ -e "/home/david/.bashrc.casino" ] && source "/home/david/.bashrc.casino"
