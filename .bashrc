#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

alias ruby='ruby-2.3'
alias gem='gem-2.3'
[[ -s ~/.nurc ]] && source ~/.nurc
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
export GEM_HOME=$(ruby -e 'print Gem.user_dir')

export GITAWAREPROMPT=~/.bash/git-aware-prompt
source "${GITAWAREPROMPT}/main.sh"
export PS1="\${debian_chroot:+(\$debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtylw\] \D{%T}$txtrst \$ "

export TERM=xterm-color

# added by Anaconda3 installer
export PATH="/home/claynon/anaconda3/bin:$PATH"

unset python
