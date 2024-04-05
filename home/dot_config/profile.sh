# Copyright (C) 2024  Yushun cheng <chengys@disroot.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#
# CONVENIENT FUNCTIONS
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

run_if_exists() {
  which "$1" 1>/dev/null 2>/dev/null && "$@"
}

#
# SETTINGS
#

# environment.d
spc=" "
for kvpair in $(cat "$HOME/.config/environment.d/00-profile.conf" \
                | sed 's/^[ \t]*//;s/[ \t]*$//'                   \
                | grep -v '^#'                                    \
                | grep -v '^$'                                    \
                | sed 's/ /${spc}/g'); do
  export "$(eval echo $kvpair)"
done
unset -v spc


# Proxies.
if [ -f "$XDG_CONFIG_HOME/clash/settings-private.yaml" ]; then
  PROXY_AUTH=$(yq -r '.["authentication"][0]' \
                  "$XDG_CONFIG_HOME/clash/settings-private.yaml")
  if [ "$PROXY_AUTH" = null ]; then
    unset -v PROXY_AUTH
  else
    export PROXY_AUTH
    export ALL_PROXY=socks5h://$PROXY_AUTH@127.0.0.1:7891
  fi
fi
if [ "$PROXY_AUTH" = "" ]; then
  export ALL_PROXY=socks5h://127.0.0.1:7891
fi
export http_proxy=$ALL_PROXY

# Environments.
export USE_FISH=1
export EDITOR=nvim
_insert_path QT_PLUGIN_PATH "/usr/lib/qt/plugins"
_insert_path QT_PLUGIN_PATH "/usr/lib/qt6/plugins"

# Host specific settings.
if [ "$(hostname)" = "pretty-arch" ]; then
  export EDITOR="emacsclient -nw"
  insert_path /usr/local/texlive/2023/bin/x86_64-linux
fi

# OS Specific settings.
if [ "$(uname -o)" = "GNU/Linux" ]; then
  export DMI_PRODUCT_NAME=$(cat '/sys/devices/virtual/dmi/id/product_name')
elif [ "$(uname -s)" = 'MSYS_NT-10.0' ]; then
  # Editor.
  export EDITOR=vim

  # Python version.
  export PYTHON_VERSION=310

  # Application specific paths.
  insert_path '/c/Program Files/Mozilla Firefox'
  insert_path '/mingw64/bin'
  insert_man '/mingw64/share/man'
  insert_info '/mingw64/share/info'
  insert_path '/c/Program Files/Python'${PYTHON_VERSION}
  insert_path '/c/Program Files/Python'${PYTHON_VERSION}'/Scripts'
  insert_path '/c/Users/'${USER}'/AppData/Roaming/Python/Python'${PYTHON_VERSION}'/Scripts'
  insert_path '/c/Users/'${USER}'/AppData/Local/Programs/MiKTeX/miktex/bin/x64'
elif [ "$(uname -s)" = "Darwin" ]; then
  insert_path "/opt/homebrew/sbin"
  insert_path "/opt/homebrew/bin"
  export PATH
fi

# XDG GUI specific settings.
if [ -n "$XDG_SESSION_TYPE" ] && [ "$XDG_SESSION_TYPE" != tty ]; then
  export TERMINAL=alacritty
  export BROWSER="firefox -P cys"
  export SSH_ASKPASS=/usr/bin/ksshaskpass

  # Input method.
  export INPUT_METHOD="fcitx"
  export XMODIFIERS="@im=fcitx"

  # DPI variables.
  case "${DMI_PRODUCT_NAME}" in
    "HP EliteBook 755 G5" )
      export DPI=144
      export DPI_SCALE=1.5
      ;;
  esac

  # Wayland specific settings.
  if [ "${XDG_SESSION_TYPE}" = "wayland" ]; then
    export MOZ_ENABLE_WAYLAND=1
    export GDK_BACKEND=wayland
    export QT_QPA_PLATFORM=wayland
    export SDL_VIDEODRIVER=wayland
  fi

  # XDG desktop specific settings.
  case "${XDG_CURRENT_DESKTOP}" in
    "GNOME" )
      export QT_QPA_PLATFORMTHEME=gtk3
      ;;
    "KDE" )
      ;;
  esac
fi

# Common paths.
insert_path "$HOME/.local/bin"
insert_path "$HOME/.local/bin/scripts"
insert_path "$HOME/.local/bin/wrappers"
insert_path "$HOME/.local/bin/control"
export PATH

# Non-standard execution required settings.
if [ "$XDG_SESSION_TYPE" = "tty" ]; then
  run_if_exists setfont /usr/share/kbd/consolefonts/ter-118n.psf.gz
fi
export GEM_HOME="$(run_if_exists ruby -e 'puts Gem.user_dir')"
if [ -n "$GEM_HOME" ]; then
  append_path "$GEM_HOME/bin"
fi
