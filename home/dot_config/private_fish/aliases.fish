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

set -e SH_CONFIG_HOME
