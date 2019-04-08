# vim: foldmethod=marker:fen
# Settings for interactive features (not for dumb terminals)

# == Guard == {{{1

if [[ "$TERM" == 'dumb' || ! $- =~ i  ]]; then
  return
fi

# == Completion == {{{1

fpath=(
  "$HOME/.zsh/external/zsh-completions/src"
  "$HOME/.zsh/external/conda-zsh-completion"
  $fpath
)

autoload -Uz compinit
compinit -i

# Use caching to make completion for cammands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "$HOME/.zcompcache"

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

if command -v brew > /dev/null 2>&1; then
  local aws_completion_file=$(brew --prefix)/share/zsh/site-functions/_aws
  if [[ -f "$aws_completion_file" ]]; then
    source $aws_completion_file
  fi
  unset aws_completion_file
fi


# == Appearance == {{{1

# Prompt {{{2
fpath=( ~/.zsh/prompt $fpath )
autoload -Uz promptinit
promptinit
prompt 'mganjoo'

# Colors {{{2

RCol=$'\E[0m'  # Text Reset

# Regular         Bold               Underline          High Intensity     BoldHigh Intens     Background         High Intensity Backgrounds
Bla=$'\e[0;30m';  BBla=$'\E[1;30m';  UBla=$'\e[4;30m';  IBla=$'\e[0;90m';  BIBla=$'\e[1;90m';  On_Bla=$'\e[40m';  On_IBla=$'\e[0;100m';
Red=$'\e[0;31m';  BRed=$'\E[1;31m';  URed=$'\e[4;31m';  IRed=$'\e[0;91m';  BIRed=$'\e[1;91m';  On_Red=$'\e[41m';  On_IRed=$'\e[0;101m';
Gre=$'\e[0;32m';  BGre=$'\E[1;32m';  UGre=$'\e[4;32m';  IGre=$'\e[0;92m';  BIGre=$'\e[1;92m';  On_Gre=$'\e[42m';  On_IGre=$'\e[0;102m';
Yel=$'\e[0;33m';  BYel=$'\E[1;33m';  UYel=$'\e[4;33m';  IYel=$'\e[0;93m';  BIYel=$'\e[1;93m';  On_Yel=$'\e[43m';  On_IYel=$'\e[0;103m';
Blu=$'\e[0;34m';  BBlu=$'\E[1;34m';  UBlu=$'\e[4;34m';  IBlu=$'\e[0;94m';  BIBlu=$'\e[1;94m';  On_Blu=$'\e[44m';  On_IBlu=$'\e[0;104m';
Pur=$'\e[0;35m';  BPur=$'\E[1;35m';  UPur=$'\e[4;35m';  IPur=$'\e[0;95m';  BIPur=$'\e[1;95m';  On_Pur=$'\e[45m';  On_IPur=$'\e[0;105m';
Cya=$'\e[0;36m';  BCya=$'\E[1;36m';  UCya=$'\e[4;36m';  ICya=$'\e[0;96m';  BICya=$'\e[1;96m';  On_Cya=$'\e[46m';  On_ICya=$'\e[0;106m';
Whi=$'\e[0;37m';  BWhi=$'\E[1;37m';  UWhi=$'\e[4;37m';  IWhi=$'\e[0;97m';  BIWhi=$'\e[1;97m';  On_Whi=$'\e[47m';  On_IWhi=$'\e[0;107m';

# Colorize less for man
export LESS_TERMCAP_mb=$BRed       # start blink
export LESS_TERMCAP_md=$BRed       # start bold
export LESS_TERMCAP_me=$RCol       # turn off bold, blink and underline
export LESS_TERMCAP_so=$Bla$On_Whi # start standout
export LESS_TERMCAP_se=$RCol       # stop standout
export LESS_TERMCAP_us=$BGre       # start underline
export LESS_TERMCAP_ue=$RCol       # stop underline

# Colorize ls
export CLICOLOR=1
if [[ "$OSTYPE" == darwin* ]]; then
  export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
fi

# }}}1
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

# lesspipe.sh {{{2
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

# fzf {{{2
export FZF_TMUX=0
export FZF_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD ||
  find . -path "*/\.*" -prune -o -type f -print -o -type l -print | sed s/^..//) 2>/dev/null'
export FZF_CTRL_T_COMMAND='(git ls-files --exclude-standard -co ||
  find * -name ".*" -prune -o -type f -print -o -type l -print) 2>/dev/null'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# run-help {{{2
autoload run-help
if command -v brew > /dev/null 2>&1; then
  HELPDIR=$(brew --prefix)/share/zsh/help
fi

# zsh-users plugins (order of loading matters) {{{2

ZSH_HIGHLIGHT_HIGHLIGHTERS=( main brackets pattern cursor )
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=magenta,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'  # Case insensitive.

source ~/.zsh/external/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/external/zsh-history-substring-search/zsh-history-substring-search.zsh

# }}}1
# == Widgets and bindings == {{{1

# Enable command line editing through an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

# Activate url-quote-magic on all entries.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# == editor-info and update to default ZLE widgets == {{{2

# Set indicators depending on the current editor state.
# We insert a call to editor-info in all the basic widgets.
function editor-info {
  unset editor_info
  typeset -gA editor_info

  if [[ "$KEYMAP" == 'vicmd' ]]; then
    zstyle -s ':prompt:indicator:keymap:command' format 'REPLY'
    editor_info[keymap]="$REPLY"
  else
    zstyle -s ':prompt:indicator:keymap:insert' format 'REPLY'
    editor_info[keymap]="$REPLY"
  fi
  unset REPLY

  # Cause prompt to be redisplayed using new styles.
  zle reset-prompt
  zle -R
}
zle -N editor-info

zle-keymap-select() {
  zle editor-info
}
zle -N zle-keymap-select

zle-line-init() {
  echoti smkx
  zle editor-info
}
zle -N zle-line-init

zle-line-finish() {
  echoti rmkx
  zle editor-info
}
zle -N zle-line-finish

overwrite-mode() {
  zle .overwrite-mode
  zle editor-info
}
zle -N overwrite-mode

vi-insert() {
  zle .vi-insert
  zle editor-info
}
zle -N vi-insert

vi-insert-bol() {
  zle .vi-insert-bol
  zle editor-info
}
zle -N vi-insert-bol

vi-replace() {
  zle .vi-replace
  zle editor-info
}
zle -N vi-replace

# == Custom widgets == {{{2

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

# == Key bindings == {{{2

# Motion keys.
bindkey -M viins "$keycode[Home]" beginning-of-line
bindkey -M viins "$keycode[End]" end-of-line
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins "$keycode[Delete]" delete-char
bindkey -M viins "$keycode[Backspace]" backward-delete-char
bindkey -M viins "$keycode[Left]" backward-char
bindkey -M viins "$keycode[Right]" forward-char

# Kill till start of line (don't use annoying vi-equivalent).
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

# Copy line
bindkey -M vicmd 'Y' pb-yank-whole-line

# Paste the selected branches into the command line
bindkey -M viins '^b' fzf-branch-widget

# Output of last command.
bindkey -M viins "^x^l" insert-last-command-output

# run-help
bindkey -M viins '^x^h' run-help

# zsh-history-substring-search
bindkey -M viins "^p" history-substring-search-up
bindkey -M viins "^n" history-substring-search-down
bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down
bindkey -M viins "$keycode[Up]" history-substring-search-up
bindkey -M viins "$keycode[Down]" history-substring-search-down

# }}}1
