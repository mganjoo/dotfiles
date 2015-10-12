## ZLE widgets

# Ignore this file on dumb terminals.
if [[ "$TERM" == 'dumb' || ! $- =~ i ]]; then
  return
fi

# Set indicators depending on the current editor state.
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

# Insert a call to editor-info in all the basic widgets.

zle-keymap-select() {
  zle editor-info
}
zle -N zle-keymap-select

zmodload zsh/terminfo
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

# == Custom widgets ==

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
