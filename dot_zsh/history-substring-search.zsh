HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=magenta,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'  # Case insensitive.
source ~/.zsh/external/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey -M viins "^p" history-substring-search-up
bindkey -M viins "^n" history-substring-search-down
bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down

zmodload zsh/terminfo
typeset -A keycode
keycode=(
  'Up'        "$terminfo[kcuu1]"
  'Down'      "$terminfo[kcud1]"
)

bindkey -M viins "$keycode[Up]" history-substring-search-up
bindkey -M viins "$keycode[Down]" history-substring-search-down
