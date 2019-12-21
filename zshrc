# vim: foldmethod=marker:fen

# == Aliases == {{{1

# Add noglob so we use references like ^foo
alias git='noglob git'

# Viewing processes
alias psa="ps aux | less"
alias psg="ps aux | grep "
alias psf="ps aux | fzf -m"

# Moving around
alias ..='cd ..'
alias cdb='cd -'

# Quitting
alias ZZ='exit'
alias :q='exit'

# Show human friendly numbers
alias df='df -h'
alias ls='ls -h'
alias du='du -h -d 2'

# Long form ls with hidden info
alias ll="ls -lh"
alias la="ls -lah"

# show me files matching "ls grep"
alias lsg='ll | grep '
alias lsp='ll | fzf -m'

# Being careful on most commands
alias cp="cp -i"
alias ln="ln -i"
alias mv="mv -i"
alias rm="rm -i"

# Branch (b)
alias gb='git branch'
alias gbc='git checkout -q -b'
alias gbl='git branch -v'
alias gbL='git branch -av'
alias gbx='git branch -d'
alias gbX='git branch -D'
alias gbm='git branch -m'
alias gbM='git branch -M'
alias gbs='git show-branch'
alias gbS='git show-branch -a'

# Commit (c)
alias gc='git commit --verbose'
alias gca='git commit --verbose --all'
alias gcm='git commit --message'
alias gco='git checkout'
alias gcO='git checkout --patch'
alias gcf='git commit --amend --reuse-message HEAD'
alias gcF='git commit --verbose --amend'
alias gcp='git cherry-pick --ff'
alias gcP='git cherry-pick --no-commit'
alias gcr='git revert'
alias gcR='git reset "HEAD^"'
alias gcs='git show'

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
alias gia='git add'
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
alias gmm='git merge origin/master'

# Push (p)
alias gp='git push'
alias gpf='git push --force'
alias gpa='git push --all'
alias gpA='git push --all && git push --tags'
alias gpt='git push --tags'
alias gpc='git push --set-upstream origin HEAD'

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

# Stash (s)
alias gs='git stash'
alias gsa='git stash apply'
alias gsx='git stash drop'
alias gsl='git stash list'
alias gsd='git stash show --patch --stat'
alias gsp='git stash pop'
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
alias glg='git log --topo-order --graph --pretty=one --abbrev-commit --decorate'
alias gls='git log --topo-order --stat'
alias gld='git log --topo-order --stat --patch --full-diff'
alias glo='git oneline'
alias glb='git brief'
alias glc='git shortlog --summary --numbered'

# Fetch
alias gup='git up'

# Common shell functions
alias less='less -r'

# Archive files
alias guz='tar -zxvf'
alias gz='tar -zcvf'
alias buz='tar -jxvf'
alias bz='tar -jcvf'

# Kill
alias k9='kill -9'

# Tmux aliases
alias tn='tmux new'
alias tns='tmux new -s'
alias tls='tmux ls'
alias ta='tmux attach'
alias tat='tmux attach -t'
alias tk='tmux kill-session'
alias tkt='tmux kill-session -t'

# tmuxifier
alias txs='tmuxifier s'
alias txes='tmuxifier es'

# emacs
alias cemacs='emacs -nw'

# fasd
alias v='LANG=c f -t -e vim -b viminfo'

# zmv
alias zmv='noglob zmv'

# rot
alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"

# scala
alias newscala="sbt new scala/scalatest-example.g8"

# == Settings == {{{1

# Changing directories {{{2
setopt AUTO_CD            # Change to directory when just the name is specified.
setopt AUTO_PUSHD         # Make cd push the old directory onto the stack.
setopt PUSHD_IGNORE_DUPS  # Do not push duplicates onto the stack.
setopt PUSHD_SILENT       # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME      # Have pushd with no arguments act like `pushd $HOME'.
setopt CDABLE_VARS        # Treat `cd var' as `cd ~var' when var is a variable.

# Completion {{{2
setopt AUTO_NAME_DIRS     # Automatically create named directories for vars.
setopt COMPLETE_IN_WORD   # Complete from both ends of a word.
setopt ALWAYS_TO_END      # Move cursor to the end of a completed word.
setopt AUTO_MENU          # Show completion menu on a succesive tab press.
setopt AUTO_LIST          # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH   # If completed parameter is a directory, add a trailing slash.
setopt NO_MENU_COMPLETE   # Do not autoselect the first completion entry.

# Scripts and functions {{{2
setopt MULTIOS            # Allow multiple input and output redirections.

# Expansion and globbing {{{2
setopt EXTENDED_GLOB      # Enable extended glob matching.
setopt BRACE_CCL          # Allow brace character class list expansion.
setopt NO_CASE_GLOB       # Make globbing case insensitive.

# ZLE {{{2
setopt COMBINING_CHARS    # Handle combining characters specially.
setopt BEEP               # Beep on error in line editor.

# Input/output {{{2
setopt NO_CLOBBER         # Don't truncate existing files with '>'.
setopt RC_QUOTES          # Allow '' to represent ' in single-quoted strings.
setopt NO_FLOW_CONTROL    # Disable start/stop characters (^s and ^q).
setopt PATH_DIRS          # Perform path search even on command names with slashes.
setopt CORRECT            # Correct commands.

# Job control {{{2
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing jobs first.
setopt NOTIFY             # Report status of background jobs immediately.
setopt NO_BG_NICE         # Don't run all background jobs at a lower priority.
setopt NO_HUP             # Don't kill jobs on shell exit.
setopt NO_CHECK_JOBS      # Don't report on jobs when shell exits.

# History {{{2
setopt BANG_HIST               # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY        # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY      # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY           # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST  # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS        # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS    # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS       # Do not display a previously found event.
setopt HIST_IGNORE_SPACE       # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS       # Do not write a duplicate event to the history file.
setopt HIST_VERIFY             # Do not execute immediately upon history expansion.
setopt HIST_BEEP               # Beep when accessing non-existent history.

# }}}1
# == Interactive (not for dumb terminals) == {{{1

source ~/.zsh/interactive.zsh

# == Additional plugins == {{{1

# fasd
if (( $+commands[fasd] )); then
  eval "$(fasd --init auto)"
fi

# == Source .zshrc_local file == {{{1

if [ -e ~/.zshrc_local ]; then
  source ~/.zshrc_local
fi
