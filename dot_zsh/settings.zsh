# General options {{{1
setopt COMBINING_CHARS      # Combine zero-length punctuation characters
                            # (accents) with the base character.
setopt RC_QUOTES            # Allow '' to represent ' in single-quoted strings.
setopt INTERACTIVE_COMMENTS # Allow comments in interactive shells.
unsetopt MAIL_WARNING       # No warning message if a mail file is accessed.

# Disable XON/XOFF conrol (legacy terminal pause/resume behavior)
# and allow mapping of Ctrl+S and Ctrl-Q shortcuts
[[ -r ${TTY:-} && -w ${TTY:-} && $+commands[stty] == 1 ]] && stty -ixon <$TTY >$TTY

# Job control
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing jobs first.
setopt NOTIFY             # Report status of background jobs immediately.
setopt NO_BG_NICE         # Don't run all background jobs at a lower priority.
setopt NO_HUP             # Don't kill jobs on shell exit.
setopt NO_CHECK_JOBS      # Don't report on jobs when shell exits.

# History {{{1
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

# History parameters
HISTFILE="$HOME/.zsh_history"  # Path to the history file.
HISTSIZE=100000                # Max number of events to store in session.
SAVEHIST=100000                # Max number of history events to save.

# Directory {{{1
setopt AUTO_CD            # Change to directory when just the name is specified.
setopt AUTO_PUSHD         # Make cd push the old directory onto the stack.
setopt PUSHD_IGNORE_DUPS  # Do not push duplicates onto the stack.
setopt PUSHD_SILENT       # Do not print directory stack after pushd or popd.
setopt PUSHD_TO_HOME      # Have pushd with no arguments act like `pushd $HOME'.
setopt CDABLE_VARS        # Treat `cd var' as `cd ~var' when var is a variable.
setopt MULTIOS            # Allow multiple input and output redirections.
setopt EXTENDED_GLOB      # Enable extended glob matching.
setopt BRACE_CCL          # Allow brace character class list expansion.
setopt NO_CASE_GLOB       # Make globbing case insensitive.
unsetopt CLOBBER          # Don't truncate existing files with '>'.

# Other utilities {{{1
setopt CORRECT            # Correct commands.

