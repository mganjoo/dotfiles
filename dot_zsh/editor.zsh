# vim: foldmethod=marker:fen ft=zsh

# == Options and variables {{{1
setopt BEEP                       # Beep on error in line editor.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>' # Do not treat / and = as part of a word.

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

# Enable command line editing through an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

# Better handling of URLs.
autoload -Uz bracketed-paste-url-magic
zle -N bracketed-paste bracketed-paste-url-magic
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# == Functions {{{1

# Runs bindkey but for all of the keymaps. Running it with no arguments
# will print out the mappings for all of the keymaps.
function bindkey-all {
  local keymap=''
  for keymap in $(bindkey -l); do
    [[ "$#" -eq 0 ]] && printf "#### %s\n" "${keymap}" 1>&2
    bindkey -M "${keymap}" "$@"
  done
}

# Expand ... to ../..
function expand-dot-to-parent-directory-path() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
zle -N expand-dot-to-parent-directory-path

# Displays an indicator when completing.
function expand-or-complete-with-indicator() {
  local indicator
  zstyle -s ':editor:info:completing' format 'indicator'

  # This is included to work around a bug in zsh which shows up when interacting
  # with multi-line prompts.
  if [[ -z "$indicator" ]]; then
    zle expand-or-complete
    return
  fi

  print -Pn "$indicator"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-indicator

# Inserts 'sudo ' at the beginning of the line.
function prepend-sudo() {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-sudo

# Inserts 'echo ' at the beginning of the line.
functin prepend-echo() {
  if [[ "$BUFFER" != echo\ * ]]; then
    BUFFER="echo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-echo

# Insert the output of the last command (note that it re-executes).
zmodload -i zsh/parameter
function insert-last-command-output() {
  LBUFFER+="$(eval $history[$((HISTCMD-1))])"
}
zle -N insert-last-command-output

# Expand aliases.
function glob-alias {
  zle _expand_alias
  zle expand-word
  zle magic-space
}
zle -N glob-alias

function pb-yank-whole-line() {
  zle vi-yank-whole-line
  print -rn $CUTBUFFER | pbcopy
}
zle -N pb-yank-whole-line

# == Key bindings == {{{1

# Layout
bindkey -d  # Reset to default key bindings.
bindkey -v  # Set to vi mode.

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

# Expand all aliases, including global
bindkey -M viins '^ ' glob-alias

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
bindkey -M vicmd '\er' redo

# History search.
bindkey -M vicmd '?' history-incremental-pattern-search-backward
bindkey -M vicmd '/' history-incremental-pattern-search-forward

# Copy line.
bindkey -M vicmd 'Y' pb-yank-whole-line

# Output of last command.
bindkey -M viins "^x^l" insert-last-command-output

# run-help.
autoload run-help
bindkey -M viins '^x^h' run-help
