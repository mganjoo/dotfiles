# vim: foldmethod=marker:fen ft=zsh
# Key bindings and custom widgets

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