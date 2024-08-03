#
# FUNCTIONS
#

_append_path() {
  _var_name="$1"
  _path="$2"

  case $(eval 'printf' "'%s'" '":${'"$_var_name"'}:"') in
    *:"$_path":* )
    ;;
    * )
      eval ${_var_name}'="${'${_var_name}':+${'${_var_name}'}:}${_path}"'
  esac

  unset -v _var_name
  unset -v _path
}

_insert_path() {
  _var_name="$1"
  _path="$2"

  case $(eval 'printf' "'%s'" '"${'${_var_name}'}"') in
    '' )
      eval ${_var_name}'="${_path}"'
    ;;
    "${_path}" | "${_path}":* )
    ;;
    *:"${_path}" )
      eval ${_var_name}'="${_path}:${'${_var_name}'%:${_path}}"'
    ;;
    *:"${_path}":* )
      eval '_prefix="${'${_var_name}'%%${_path}:*}"'
      eval '_suffix="${'${_var_name}'#*${_path}:}"'
      eval ${_var_name}'="${_path}:${_prefix}${_suffix}"'
      unset -v _prefix
      unset -v _suffix
    ;;
    * )
      eval ${_var_name}'="${_path}:${'${_var_name}'}"'
  esac

  unset -v _var_name
  unset -v _path
}

append_path() {
  _append_path PATH "$1"
}

insert_path() {
  _insert_path PATH "$1"
}

append_man() {
  _append_path MANPATH "$1"
}

insert_man() {
  _insert_path MANPATH "$1"
}

append_info() {
  _append_path INFOPATH "$1"
}

insert_info() {
  _insert_path INFOPATH "$1"
}

#
# VARIABLES
#

# platform specific settings
if [ "$UNAME_S" = Linux ]; then
  _insert_path QT_PLUGIN_PATH /usr/lib/qt/plugins
  _insert_path QT_PLUGIN_PATH /usr/lib/qt6/plugins
elif [ "$UNAME_S" = Msys ]; then
  # Python version
  PYTHON_VERSION=310
  # mingw64 paths
  insert_path "/c/Program Files/Mozilla Firefox"
  insert_path /mingw64/bin
  insert_man /mingw64/share/man
  insert_info /mingw64/info
  # Python
  insert_path "/c/Program Files/Python$PYTHON_VERSION"
  insert_path "/c/Program Files/Python$PYTHON_VERSION/Scripts"
  insert_path "/c/Users/$USER/AppData/Roaming/Python/Python$PYTHON_VERSION/Scripts"
  # MiKTeX
  insert_path "/c/Users/$USER/AppData/Local/Programs/MiKTeX/miktex/bin/x64"
elif [ "$UNAME_S" = Darwin ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  insert_man /usr/share/man
  insert_man /opt/homebrew/share/man
  # coreutils
  insert_path /opt/homebrew/opt/coreutils/libexec/gnubin
  insert_man /opt/homebrew/opt/coreutils/libexec/gnuman
  insert_info /opt/homebrew/opt/coreutils/share/info
  # MacTeX
  insert_path /Library/Tex/texbin
  insert_man /Library/Tex/texbin/man
fi

# a programming language called ruby
export PATH
if which ruby 1>/dev/null 2>/dev/null; then
  GEM_HOME="$(ruby -e "puts Gem.user_dir")"
  append_path "$GEM_HOME/bin"
fi

# a programming language called rust
CARGO_HOME="$HOME/.cargo"
insert_path "$CARGO_HOME/bin"

# common paths
insert_path "$HOME/.local/bin"
insert_path "$HOME/.local/bin/monolith"
insert_path "$HOME/.local/bin/scripts"
insert_path "$HOME/.local/bin/wrappers"
insert_path "$HOME/.local/bin/control"

# export variables
envexp PATH
envexp MANPATH
envexp INFOPATH
envexp QT_PLUGIN_PATH
envexp PYTHON_VERSION
envexp GEM_HOME
envexp CARGO_HOME

# Apparently, macOS overwrites the PATH variable for each application.  We set
# an additional environment variable, PATH_FOR_MAC, for the configured PATH to
# be taken by other programs.
if [ "$UNAME_S" = Darwin ]; then
  envexp PATH_FOR_MAC "$PATH"
fi
