# vim: foldmethod=marker:fen ft=zsh
# Shell settings and options

# History parameters
HISTFILE="$HOME/.zsh_history"  # Path to the history file.
HISTSIZE=100000                # Max number of events to store in session.
SAVEHIST=100000                # Max number of history events to save.

# Editor and visual
export EDITOR='nvim'
export VISUAL='nvim'
export PAGER='less'
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Changing directories
setopt AUTO_CD            # Change to directory when just the name is specified.
setopt AUTO_PUSHD         # Make cd push the old directory onto the stack.
setopt PUSHD_IGNORE_DUPS  # Do not push duplicates onto the stack.
setopt PUSHD_SILENT       # Do not print directory stack after pushd or popd.
setopt PUSHD_TO_HOME      # Have pushd with no arguments act like `pushd $HOME'.
setopt CDABLE_VARS        # Treat `cd var' as `cd ~var' when var is a variable.

# Completion
setopt AUTO_NAME_DIRS     # Automatically create named directories for vars.
setopt COMPLETE_IN_WORD   # Complete from both ends of a word.
setopt ALWAYS_TO_END      # Move cursor to the end of a completed word.
setopt AUTO_MENU          # Show completion menu on a succesive tab press.
setopt AUTO_LIST          # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH   # If completed parameter is a directory, add a trailing slash.
setopt NO_MENU_COMPLETE   # Do not autoselect the first completion entry.

# Scripts and functions
setopt MULTIOS            # Allow multiple input and output redirections.

# Expansion and globbing
setopt EXTENDED_GLOB      # Enable extended glob matching.
setopt BRACE_CCL          # Allow brace character class list expansion.
setopt NO_CASE_GLOB       # Make globbing case insensitive.

# ZLE
setopt COMBINING_CHARS            # Handle combining characters specially.
setopt BEEP                       # Beep on error in line editor.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>' # Do not treat / and = as part of a word.

# Input/output
setopt NO_CLOBBER         # Don't truncate existing files with '>'.
setopt RC_QUOTES          # Allow '' to represent ' in single-quoted strings.
setopt NO_FLOW_CONTROL    # Disable start/stop characters (^s and ^q).
setopt PATH_DIRS          # Perform path search even on command names with slashes.
setopt CORRECT            # Correct commands.
setopt INTERACTIVECOMMENTS # Allow comments in interactive shells.

# Job control
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing jobs first.
setopt NOTIFY             # Report status of background jobs immediately.
setopt NO_BG_NICE         # Don't run all background jobs at a lower priority.
setopt NO_HUP             # Don't kill jobs on shell exit.
setopt NO_CHECK_JOBS      # Don't report on jobs when shell exits.

# History
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