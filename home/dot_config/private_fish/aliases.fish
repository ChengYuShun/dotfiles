set -l SH_CONFIG_HOME $XDG_CONFIG_HOME'/sh'

# POSIX Shell.
source $SH_CONFIG_HOME'/aliases.sh'

if test (uname -s) = 'MSYS_NT-10.0'
    # Fish.
    alias help '$BROWSER (cygpath -w '"'"'/usr/share/doc/fish/index.html'"'"')'
    # Windows.
    alias wczm 'chezmoi \
	-D $USERPROFILE \
	-S $USERPROFILE'"'"'/AppData/Roaming/chezmoi'"'"
    alias wczmcd 'env \
	-C $USERPROFILE'"'"'/AppData/Roaming/chezmoi'"'"' \
	$SHELL -i'
end

alias env-term 'env -i \
    ALACRITTY_LOG="$ALACRITTY_LOG" \
    ALACRITTY_SOCKET="$ALACRITTY_SOCKET" \
    ALACRITTY_WINDOW_ID="$ALACRITTY_WINDOW_ID" \
    COLORTERM="$COLORTERM" \
    TERM="$TERM" \
    TERMINFO="$TERMINFO" \
    TERMINFO_DIRS="$TERMINFO_DIRS"'

set -e SH_CONFIG_HOME
