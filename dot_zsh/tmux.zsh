# Aliases
alias ta="tmuxa attach"
# Create or attach to a session named "username" by default
alias tt="exec tmux new-session -A -s ${jSER} >/dev/null 2>&1"

# if [[ -x "$(command -v tmux)" ]] && \
#   [[ -z "${TMUX}" ]] && \
#   [[ "${TERM_PROGRAM}" == "ghostty" ]]; then
#     exec tmux new-session -A -s ${USER} >/dev/null 2>&1
# fi
