ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
ZSH_AUTOSUGGEST_USE_ASYNC=1

source ~/.zsh/external/zsh-autosuggestions/zsh-autosuggestions.zsh

# Keybindings for accepting suggestions.
bindkey -M viins "^f" vi-forward-word
bindkey -M viins "^e" vi-add-eol
