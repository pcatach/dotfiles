echo "Sourcing .bashrc"

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# color
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
    xterm-ghostty) color_prompt=yes
esac

# prompt
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}
get_git_email() {
    git config user.email
}

if [ "$color_prompt" = yes ]; then
    PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;31m\] $(parse_git_branch)\[\033[00m\] \[\033[00;33m\]$(get_git_email)\[\033[00m\]\$ '
else
    PS1='\u@\h:\w $(parse_git_branch) $(get_git_email)\$ '
fi

# history stuff
shopt -s histappend
HISTSIZE=
HISTFILESIZE=
HISTCONTROL=ignoreboth
export PROMPT_COMMAND='history -a'

export PAGER="less -S"
