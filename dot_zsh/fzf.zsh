[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_COMMAND='(rg --files) 2>/dev/null'
export FZF_CTRL_T_COMMAND='(rg --files) 2>/dev/null'

# Branch widget for FZF.
function __bsel() {
  git for-each-ref --format='%(refname:short)' refs/heads/ 2>/dev/null | fzf -m | tr "\n" " "
}
function fzf-branch-widget() {
  LBUFFER="${LBUFFER}$(__bsel)"
  zle redisplay
}
zle -N fzf-branch-widget

# Paste the selected branches into the command line.
bindkey -M viins '^b' fzf-branch-widget
