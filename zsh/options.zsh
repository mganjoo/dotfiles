setopt AUTO_CD            # Change to directory when just the name is specified.
setopt AUTO_PUSHD         # Make cd push the old directory onto the stack.
setopt PUSHD_IGNORE_DUPS  # Don't push duplicates onto the stack.
setopt PUSHD_SILENT       # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME      # Have pushd with no arguments act like `pushd $HOME'.
setopt CDABLE_VARS        # Treat `cd var' as `cd ~var' when var is a variable.
setopt AUTO_NAME_DIRS     # Automatically create named directories for vars.
setopt EXTENDED_GLOB      # Enable extended glob matching.
setopt NO_CLOBBER         # Don't truncate existing files with '>'.
setopt MULTIOS            # Allow multiple input and output redirections.
