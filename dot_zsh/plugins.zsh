# vim: foldmethod=marker:fen ft=zsh
# Plugin loading and external tool completions

# == Interactive plugins == {{{1

# fzf
export FZF_DEFAULT_COMMAND='(fd --type f --strip-cwd-prefix --hidden --follow --exclude .git) 2>/dev/null'
export FZF_CTRL_T_COMMAND='(fd --type f --strip-cwd-prefix --hidden --follow --exclude .git) 2>/dev/null'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# == iTerm shell integration == {{{1
if [[ -f ~/.iterm2_shell_integration.zsh ]]; then
  source ~/.iterm2_shell_integration.zsh
fi

# == 1Password plugins == {{{1
if [[ -f ~/.config/op/plugins.sh ]]; then
  source ~/.config/op/plugins.sh
fi

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
# Load keycode from keybindings.zsh context
zmodload zsh/terminfo
typeset -A keycode
keycode=(
  'Up'        "$terminfo[kcuu1]"
  'Down'      "$terminfo[kcud1]"
)

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

if (( $+commands[uv] )); then
  eval "$(uv generate-shell-completion zsh)"
fi

if (( $+commands[uvx] )); then
  eval "$(uvx --generate-shell-completion zsh)"
fi

if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi

if (( $+commands[fd] )); then
  eval "$(fd --gen-completions zsh)"
fi
