# == Options == {{{1

# Changing directories {{{2
setopt AUTO_CD            # Change to directory when just the name is specified.
setopt AUTO_PUSHD         # Make cd push the old directory onto the stack.
setopt PUSHD_IGNORE_DUPS  # Don't push duplicates onto the stack.
setopt PUSHD_SILENT       # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME      # Have pushd with no arguments act like `pushd $HOME'.
setopt CDABLE_VARS        # Treat `cd var' as `cd ~var' when var is a variable.

# Completion {{{2
setopt AUTO_NAME_DIRS     # Automatically create named directories for vars.
setopt COMPLETE_IN_WORD   # Complete from both ends of a word.
setopt ALWAYS_TO_END      # Move cursor to the end of a completed word.
setopt AUTO_MENU          # Show completion menu on a succesive tab press.
setopt AUTO_LIST          # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH   # If completed parameter is a directory, add a trailing slash.
setopt NO_MENU_COMPLETE   # Do not autoselect the first completion entry.

# Scripts and functions {{{2
setopt MULTIOS            # Allow multiple input and output redirections.

# Expansion and globbing {{{2
setopt EXTENDED_GLOB      # Enable extended glob matching.
setopt BRACE_CCL          # Allow brace character class list expansion.
setopt NO_CASE_GLOB       # Make globbing case insensitive.

# ZLE {{{2
setopt COMBINING_CHARS    # Handle combining characters specially.
setopt BEEP               # Beep on error in line editor.

# Input/output {{{2
setopt NO_CLOBBER         # Don't truncate existing files with '>'.
setopt RC_QUOTES          # Allow '' to represent ' in single-quoted strings.
setopt NO_FLOW_CONTROL    # Disable start/stop characters (^s and ^q).
setopt PATH_DIRS          # Perform path search even on command names with slashes.
setopt CORRECT            # Correct commands.

# Job control {{{2
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing jobs first.
setopt NOTIFY             # Report status of background jobs immediately.
setopt NO_BG_NICE         # Don't run all background jobs at a lower priority.
setopt NO_HUP             # Don't kill jobs on shell exit.
setopt NO_CHECK_JOBS      # Don't report on jobs when shell exits.

# History {{{2
setopt BANG_HIST               # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY        # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY      # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY           # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST  # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS        # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS    # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS       # Do not display a previously found event.
setopt HIST_IGNORE_SPACE       # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS       # Do not write a duplicate event to the history file.
setopt HIST_VERIFY             # Do not execute immediately upon history expansion.
setopt HIST_BEEP               # Beep when accessing non-existent history.

# }}}1

# == Colorizing == {{{1

# Colorize less {{{2
export LESS_TERMCAP_mb=$'\E[01;31m'     # Begins blinking.
export LESS_TERMCAP_md=$'\E[01;31m'     # Begins bold.
export LESS_TERMCAP_me=$'\E[0m'         # Ends mode.
export LESS_TERMCAP_se=$'\E[0m'         # Ends standout-mode.
export LESS_TERMCAP_so=$'\E[00;47;30m'  # Begins standout-mode.
export LESS_TERMCAP_ue=$'\E[0m'         # Ends underline.
export LESS_TERMCAP_us=$'\E[01;32m'     # Begins underline.

# Colorize grep {{{2
export GREP_OPTIONS="--color=auto"

# Colorize ls {{{2
export CLICOLOR=1
if [[ "$OSTYPE" == darwin* ]]; then
  export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
fi

# }}}1

# == History configuration == {{{1

HISTFILE="$HOME/.zhistory"  # Path to the history file.
HISTSIZE=10000              # Max number of events to store in session.
SAVEHIST=10000              # Max number of history events to save.

# }}}1

# == Editor and viewers == {{{1

# / and = are not considered part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

# }}}1

# == Plugins == {{{1

ZSH_HIGHLIGHT_HIGHLIGHTERS=( main brackets pattern cursor )
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=magenta,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'  # Case insensitive.

# }}}1

# vim: fdm=marker
