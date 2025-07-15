# Aliases
alias ta="tmuxa attach"
# Create or attach to a session named "username" by default
alias tt="tmux new-session -A -s ${jSER}"

# if [[ -x "$(command -v tmux)" ]] && \
#   [[ -z "${TMUX}" ]] && \
#   [[ "${TERM_PROGRAM}" == "ghostty" ]]; then
#     exec tmux new-session -A -s ${USER} >/dev/null 2>&1
# fi
