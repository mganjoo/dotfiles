# vim: foldmethod=marker:fen ft=zsh
# General aliases

# Processes
alias psg="procs | rg "
alias psf="procs | fzf -m"

# Moving around
alias ..='cd ..'
alias cdb='cd -'

# Quitting
alias ZZ='exit'
alias :q='exit'

# Show human friendly numbers
alias df='df -h'
alias du='du -h -d 2'

# Modern ls
alias ls="eza --icons=always"
alias ll="eza -l --icons=always --git"
alias la="eza -la --icons=always --git"
alias tree="eza --tree"

# Modern cat and grep
alias cat="bat --paging=never"
alias ccat="\cat"
alias grep="rg"
alias ggrep="\grep"

# Show me files matching "ls grep"
alias lsg='ll | rg '
alias lsp='ll | fzf -m'

# Being careful on most commands
alias cp="cp -i"
alias ln="ln -i"
alias mv="mv -i"
alias rm="rm -i"

# Chezmoi
alias ccd="chezmoi cd"
alias cea="chezmoi edit --apply"
alias cup="chezmoi update"

# Private copy of chezmoi that applies to local machine-specific files
alias pchezmoi="chezmoi -S ~/.local/share/chezmoi_private"