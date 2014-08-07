# == Prologue == {{{1

# Ignore this file on dumb terminals.
if [[ "$TERM" == 'dumb' ]]; then
  return
fi

# Disable START/STOP signals (some terminals capture ^S and ^Q).
stty -ixon

# Load terminfo to get key codes for special keys
zmodload zsh/terminfo

# == Array of key mappings == {{{1

typeset -A key_code
key_code=(
  'Backspace' "^?"
  'Delete'    "^[[3~"
  'Home'      "$terminfo[khome]"
  'End'       "$terminfo[kend]"
  'Up'        "$terminfo[kcuu1]"
  'Left'      "$terminfo[kcub1]"
  'Down'      "$terminfo[kcud1]"
  'Right'     "$terminfo[kcuf1]"
)

# == Custom ZLE widgets == {{{1

# Expand ... to ../..
expand-dot-to-parent-directory-path() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}

# Displays an indicator when completing.
expand-or-complete-with-indicator() {
  local indicator
  zstyle -s ':editor:info:completing' format 'indicator'
  print -Pn "$indicator"
  zle expand-or-complete
  zle redisplay
}

# Inserts 'sudo ' at the beginning of the line.
prepend-sudo() {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}

# Inserts 'echo ' at the beginning of the line.
prepend-echo() {
  if [[ "$BUFFER" != echo\ * ]]; then
    BUFFER="echo $BUFFER"
    (( CURSOR += 5 ))
  fi
}

# == Load widgets == {{{1

autoload -Uz edit-command-line
zle -N edit-command-line
zle -N expand-dot-to-parent-directory-path
zle -N expand-or-complete-with-indicator
zle -N prepend-sudo
zle -N prepend-echo

# == Key bindings == {{{1

bindkey -d  # Reset to default key bindings.
bindkey -v  # Set to vi mode.

# Motion keys.
bindkey -M viins "$key_code[Home]" beginning-of-line
bindkey -M viins "$key_code[End]" end-of-line
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins "$key_code[Delete]" delete-char

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
if (( $+widgets[history-incremental-pattern-search-backward] )); then
  bindkey -M vicmd '?' history-incremental-pattern-search-backward
  bindkey -M vicmd '/' history-incremental-pattern-search-forward
  bindkey -M viins '^r' history-incremental-pattern-search-backward
else
  bindkey -M vicmd '?' history-incremental-search-backward
  bindkey -M vicmd '/' history-incremental-search-forward
  bindkey -M viins '^r' history-incremental-search-backward
fi

# TODO: add history search from history search plugin

# == Epilogue == {{{1

# Unset the variables used in the file.
unset key key_code

# vim: fdm=marker
