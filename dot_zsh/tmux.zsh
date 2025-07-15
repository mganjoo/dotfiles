# Aliases
alias ta="tmux attach"

# Create or attach to a session named "username" by default
if [[ -x "$(command -v tmux)" ]] && \
  [[ -z "${TMUX}" ]] && \
  [[ "${TERM_PROGRAM}" == "ghostty" ]]; then
    exec tmux new-session -A -s ${USER} >/dev/null 2>&1
fi
