# vim: foldmethod=marker:fen
# Settings for interactive features (not for dumb terminals)

# == Guard == {{{1
if [[ "$TERM" == 'dumb' || ! $- =~ i  ]]; then
  return
fi

# == Completion == {{{1

fpath+=("$HOME/.zsh/external/zsh-completions/src")

if [[ -d "/opt/homebrew/share/zsh/site-functions" ]]; then
  fpath+=("/opt/homebrew/share/zsh/site-functions")
fi

autoload -Uz compinit
compinit

# Use caching to make completion for commands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "$HOME/.zcompcache"

# Rehash automatically when new executables are found
zstyle ':completion:*' rehash true

# Case-insensitive (all), partial-word, and then substring completion.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions.
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Increase the number of errors based on the length of the typed word.
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Don't complete unavailable commands.
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environmental Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion.
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
  dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
  hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
  mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,3))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

# == Set up bindings (before plugins) == {{{1

bindkey -d  # Reset to default key bindings.
bindkey -v  # Set to vi mode.

# Load terminfo to get key codes for special keys
zmodload zsh/terminfo

typeset -A keycode
keycode=(
  'Backspace' "^?"
  'Delete'    "^[[3~"
  'Home'      "$terminfo[khome]"
  'End'       "$terminfo[kend]"
  'Up'        "$terminfo[kcuu1]"
  'Left'      "$terminfo[kcub1]"
  'Down'      "$terminfo[kcud1]"
  'Right'     "$terminfo[kcuf1]"
)

# == Interactive plugins == {{{1

# fzf
export FZF_DEFAULT_COMMAND='(fd --type f --strip-cwd-prefix --hidden --follow --exclude .git) 2>/dev/null'
export FZF_CTRL_T_COMMAND='(fd --type f --strip-cwd-prefix --hidden --follow --exclude .git) 2>/dev/null'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# == Widgets == {{{1

# Enable command line editing through an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

# Activate url-quote-magic on all entries.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# Expand ... to ../..
expand-dot-to-parent-directory-path() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
zle -N expand-dot-to-parent-directory-path

# Displays an indicator when completing.
expand-or-complete-with-indicator() {
  local indicator
  zstyle -s ':editor:info:completing' format 'indicator'
  print -Pn "$indicator"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-indicator

# Inserts 'sudo ' at the beginning of the line.
prepend-sudo() {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-sudo

# Inserts 'echo ' at the beginning of the line.
prepend-echo() {
  if [[ "$BUFFER" != echo\ * ]]; then
    BUFFER="echo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-echo

# Branch widget.
__bsel() {
  git for-each-ref --format='%(refname:short)' refs/heads/ 2>/dev/null | fzf -m | tr "\n" " "
}
fzf-branch-widget() {
  LBUFFER="${LBUFFER}$(__bsel)"
  zle redisplay
}
zle -N fzf-branch-widget

zmodload -i zsh/parameter
insert-last-command-output() {
  LBUFFER+="$(eval $history[$((HISTCMD-1))])"
}
zle -N insert-last-command-output

pb-yank-whole-line() {
  zle vi-yank-whole-line
  print -rn $CUTBUFFER | pbcopy
}
zle -N pb-yank-whole-line

# == Key bindings == {{{1

# Motion keys.
bindkey -M viins "$keycode[Home]" beginning-of-line
bindkey -M viins "$keycode[End]" end-of-line
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins "$keycode[Delete]" delete-char
bindkey -M viins "$keycode[Backspace]" backward-delete-char
bindkey -M viins "$keycode[Left]" backward-char
bindkey -M viins "$keycode[Right]" forward-char

# Kill till start of line.
bindkey -M viins '^u' backward-kill-line

# Expand history on space.
bindkey -M viins ' ' magic-space

# Expand command name to full path.
for key in '\e'{E,e}; do
  bindkey -M viins "$key" expand-cmd-path
done

# Duplicate the previous word.
for key in '\e'{M,m}; do
  bindkey -M viins "$key" copy-prev-shell-word
done

# Use a flexible push-line which works on multiple lines.
for key in '^q' '\e'{Q,q}; do
  bindkey -M viins "$key" push-line-or-edit
done

# Display an indicator when completing.
bindkey -M viins '^i' expand-or-complete-with-indicator

# Prepend commands at the beginning of the line.
bindkey -M viins '^x^s' prepend-sudo
bindkey -M viins '^x^o' prepend-echo

# Expand ... to ../.., but not during incremental search.
bindkey -M viins '.' expand-dot-to-parent-directory-path
bindkey -M isearch '.' self-insert

# Go up a directory (Esc-.)
bindkey -M viins -s '\e.' '..\n'

# Editing command line in external editor.
bindkey -M vicmd 'v' edit-command-line
bindkey -M viins '^x^e' edit-command-line

# Undo and redo.
bindkey -M vicmd 'u' undo
bindkey -M vicmd '^r' redo

# History search.
bindkey -M vicmd '?' history-incremental-pattern-search-backward
bindkey -M vicmd '/' history-incremental-pattern-search-forward

# Copy line.
bindkey -M vicmd 'Y' pb-yank-whole-line

# Paste the selected branches into the command line.
bindkey -M viins '^b' fzf-branch-widget

# Output of last command.
bindkey -M viins "^x^l" insert-last-command-output

# run-help.
autoload run-help
bindkey -M viins '^x^h' run-help

# == ZSH plugins == {{{1

# zsh-history-substring-search
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=magenta,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'  # Case insensitive.
source ~/.zsh/external/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey -M viins "^p" history-substring-search-up
bindkey -M viins "^n" history-substring-search-down
bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down
bindkey -M viins "$keycode[Up]" history-substring-search-up
bindkey -M viins "$keycode[Down]" history-substring-search-down

# zsh-autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
ZSH_AUTOSUGGEST_USE_ASYNC=1
source ~/.zsh/external/zsh-autosuggestions/zsh-autosuggestions.zsh

# zsh-syntax-highlighting (must be last thing to be sourced)
ZSH_HIGHLIGHT_HIGHLIGHTERS=( main brackets pattern cursor )
source ~/.zsh/external/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# == Command completion for special apps == {{{1

# Homebrew
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if (( $+commands[uv] )); then
  eval "$(uv generate-shell-completion zsh)"
fi

if (( $+commands[uvx] )); then
  eval "$(uvx --generate-shell-completion zsh)"
fi

if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi

# fd completion
if (( $+commands[fd] )); then
  eval "$(fd --gen-completions zsh)"
fi

# == Prompt == {{{1

if [[ -f /opt/homebrew/bin/starship ]]; then
  eval "$(starship init zsh)"
fi
