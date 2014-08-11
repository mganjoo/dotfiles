# Add noglob to major commands
alias git='noglob git'

# Viewing processes
alias psa="ps aux"
alias psg="ps aux | grep "
alias psp="ps aux | peco "

# Moving around
alias ..='cd ..'
alias cdb='cd -'

# Quitting
alias ZZ='exit'
alias :q='exit'

# Show human friendly numbers and colors
alias df='df -h'
alias ll='ls -alGh'
alias ls='ls -Gh'
alias du='du -h -d 2'

# show me files matching "ls grep"
alias lsg='ll | grep'
alias lsp='ll | peco'

# Alias Editing
alias ae='vi ~/.zsh/aliases.zsh' # alias edit
alias ar='source ~/.zsh/aliases.zsh'  # alias reload

# .zshrc reloading
alias zr='source ~/.zshrc'

# Git Aliases
alias gi='vi .gitignore'
alias gunc='git uncommit'

# Commit
alias gcam='git commit --all --message'

# Index
alias giaa='git add -A'

# Log
alias gl='git log --topo-order'
alias glg='git log --topo-order --graph --pretty=one'
alias gls='git log --topo-order --stat'
alias gld='git log --topo-order --stat --patch --full-diff'
alias glo='git oneline'
alias glb='git brief'

# Fetch
alias gup='git up'

# Common shell functions
alias less='less -r'
alias l='less'

# Archive files
alias guz='tar -zxvf'
alias gz='tar -zcvf'
alias buz='tar -jxvf'
alias bz='tar -jcvf'

# Kill
alias ka9='killall -9'
alias k9='kill -9'

# Tmux aliases
alias tn='tmux new'
alias tns='tmux new -s'
alias tls='tmux ls'
alias ta='tmux attach'
alias tat='tmux attach -t'
alias tk='tmux kill-session'
alias tkt='tmux kill-session -t'

# Zmv
alias zmv="noglob zmv -W"

# List 10 most used commands (or -n <number>)
alias hist_stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

# List Java versions available
if [[ "$(uname)" == "Darwin" ]]; then
  alias java_ls='/usr/libexec/java_home -V 2>&1 | grep -P "\d.\d.\d_\d\d" | cut -d : -f 1 | colrm 1 4 | grep -v Home'
fi

