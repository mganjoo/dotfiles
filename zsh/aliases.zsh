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

# Git
alias g='git'

# Branch (b)
alias gb='git branch'
alias gbc='git checkout -b'
# alias gbl='git branch -v'
# alias gbL='git branch -av'
# alias gbx='git branch -d'
# alias gbX='git branch -D'
# alias gbm='git branch -m'
alias gbM='git branch -M'
alias gbs='git show-branch'
alias gbS='git show-branch -a'

# Commit (c)
alias gc='git commit --verbose'
# alias gca='git commit --verbose --all'
alias gcm='git commit --message'
alias gcam='git commit --all --message'
alias gco='git checkout'
alias gcO='git checkout --patch'
alias gcf='git commit --amend --reuse-message HEAD'
alias gcF='git commit --verbose --amend'
alias gcp='git cherry-pick --ff'
alias gcP='git cherry-pick --no-commit'
alias gcr='git revert'
alias gcR='git reset "HEAD^"'
alias gcs='git show'
alias gcl='git-commit-lost'
alias gunc='git uncommit'

# Conflict (C)
alias gCl='git status | sed -n "s/^.*both [a-z]*ed: *//p"'
alias gCa='git add $(gCl)'
alias gCe='git mergetool $(gCl)'
alias gCo='git checkout --ours --'
alias gCO='gCo $(gCl)'
alias gCt='git checkout --theirs --'
alias gCT='gCt $(gCl)'

# Data (d)
alias gd='git ls-files'
alias gdc='git ls-files --cached'
alias gdx='git ls-files --deleted'
alias gdm='git ls-files --modified'
alias gdu='git ls-files --other --exclude-standard'
alias gdk='git ls-files --killed'
alias gdi='git status --porcelain --short --ignored | sed -n "s/^!! //p"'

# Fetch (f)
alias gf='git fetch'
alias gfc='git clone'
alias gfm='git pull'
alias gfr='git pull --rebase'

# Grep (g)
alias gg='git grep'
alias ggi='git grep --ignore-case'
alias ggl='git grep --files-with-matches'
alias ggL='git grep --files-without-matches'
alias ggv='git grep --invert-match'
alias ggw='git grep --word-regexp'

# Index (i)
# alias gia='git add'
alias giaa='git add -A'
alias giA='git add --patch'
alias giu='git add --update'
alias gid='git diff --no-ext-diff --cached'
alias giD='git diff --no-ext-diff --cached --word-diff'
alias gir='git reset'
alias giR='git reset --patch'
alias gix='git rm -r --cached'
alias giX='git rm -rf --cached'

# Merge (m)
alias gm='git merge'
alias gmC='git merge --no-commit'
alias gmF='git merge --no-ff'
alias gma='git merge --abort'
alias gmt='git mergetool'

# Push (p)
alias gp='git push'
alias gpf='git push --force'
alias gpa='git push --all'
alias gpA='git push --all && git push --tags'
alias gpt='git push --tags'
alias gpc='git push --set-upstream origin "$(git-branch-current 2> /dev/null)"'
alias gpp='git pull origin "$(git-branch-current 2> /dev/null)" && git push origin "$(git-branch-current 2> /dev/null)"'

# Rebase (r)
alias gr='git rebase'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias gri='git rebase --interactive'
alias grs='git rebase --skip'

# Remote (R)
alias gR='git remote'
alias gRl='git remote --verbose'
alias gRa='git remote add'
alias gRx='git remote rm'
alias gRm='git remote rename'
alias gRu='git remote update'
alias gRp='git remote prune'
alias gRs='git remote show'
alias gRb='git-hub-browse'

# Stash (s)
alias gs='git stash'
alias gsa='git stash apply'
alias gsx='git stash drop'
alias gsX='git-stash-clear-interactive'
alias gsl='git stash list'
alias gsL='git-stash-dropped'
alias gsd='git stash show --patch --stat'
alias gsp='git stash pop'
alias gsr='git-stash-recover'
alias gss='git stash save --include-untracked'
alias gsS='git stash save --patch --no-keep-index'
alias gsw='git stash save --include-untracked --keep-index'

# Submodule (S)
alias gS='git submodule'
alias gSa='git submodule add'
alias gSf='git submodule foreach'
alias gSi='git submodule init'
alias gSI='git submodule update --init --recursive'
alias gSl='git submodule status'
alias gSm='git-submodule-move'
alias gSs='git submodule sync'
alias gSu='git submodule foreach git pull origin master'
alias gSx='git-submodule-remove'

# Working Copy (w)
alias gws='git status --ignore-submodules=none --short'
alias gwS='git status --ignore-submodules=none'
alias gwd='git diff --no-ext-diff'
alias gwD='git diff --no-ext-diff --word-diff'
alias gwr='git reset --soft'
alias gwR='git reset --hard'
alias gwc='git clean -n'
alias gwC='git clean -f'
alias gwx='git rm -r'
alias gwX='git rm -rf'

# Log
alias gl='git log --topo-order'
alias glg='git log --topo-order --graph --pretty=one'
alias gls='git log --topo-order --stat'
alias gld='git log --topo-order --stat --patch --full-diff'
alias glo='git oneline'
alias glb='git brief'
alias glc='git shortlog --summary --numbered'

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

