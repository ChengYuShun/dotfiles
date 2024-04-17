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
# NECESSARY INFORMATION
#

_uname_s=$(uname -s)
_uname_o=$(uname -o)

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

envexp() {
  _key=$1
  _val=$2

  if ! [ -n "$_val" ]; then
    eval "_val=\$$_key"
  fi

  export "$_key=$_val"
  if [ "$_uname_s" = "Linux" ]; then
    systemctl --user set-environment "$_key=$_val"
  elif [ "$_uname_s" = "Darwin" ]; then
    launchctl setenv "$_key" "$_val"
  fi

  unset -v _key
  unset -b _val
}

#
# SETTINGS
#

# Homes.
envexp HOME
envexp XDG_CONFIG_HOME "$HOME/.config"
envexp XDG_DATA_HOME "$HOME/.local/share"
envexp XDG_CACHE_HOME "$HOME/.cache"

# Environments
envexp USE_FISH 1
envexp ZDOTDIR "$XDG_CONFIG_HOME/zsh"
envexp EDITOR nvim
envexp GNUPGHOME "$XDG_CONFIG_HOME/gnupg"
envexp CARGO_HOME "$XDG_DATA_HOME/cargo"
envexp AGDA_DIR "$XDG_CONFIG_HOME/agda"
_insert_path QT_PLUGIN_PATH "/usr/lib/qt/plugins"
_insert_path QT_PLUGIN_PATH "/usr/lib/qt6/plugins"

# Host specific settings.
if [ "$(hostname)" = "pretty-arch" ]; then
  envexp EDITOR "emacsclient -nw"
  insert_path /usr/local/texlive/2023/bin/x86_64-linux
fi

# OS Specific settings.
if [ "$_uname_o" = "GNU/Linux" ]; then
  envexp DMI_PRODUCT_NAME "$(cat "/sys/devices/virtual/dmi/id/product_name")"
  envexp SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent"
elif [ "$_uname_s" = "MSYS_NT-10.0" ]; then
  # Editor.
  envexp EDITOR vim

  # Python version.
  envexp PYTHON_VERSION 310

  # Application specific paths.
  insert_path '/c/Program Files/Mozilla Firefox'
  insert_path '/mingw64/bin'
  insert_man '/mingw64/share/man'
  insert_info '/mingw64/share/info'
  insert_path '/c/Program Files/Python'${PYTHON_VERSION}
  insert_path '/c/Program Files/Python'${PYTHON_VERSION}'/Scripts'
  insert_path '/c/Users/'${USER}'/AppData/Roaming/Python/Python'${PYTHON_VERSION}'/Scripts'
  insert_path '/c/Users/'${USER}'/AppData/Local/Programs/MiKTeX/miktex/bin/x64'
elif [ "$_uname_s" = "Darwin" ]; then
  insert_path "/opt/homebrew/sbin"
  insert_path "/opt/homebrew/bin"
  insert_path "/opt/homebrew/opt/coreutils/libexec/gnubin"
  envexp PATH
fi

# XDG GUI specific settings.
if [ -n "$XDG_SESSION_TYPE" ] && [ "$XDG_SESSION_TYPE" != tty ]; then
  envexp TERMINAL alacritty
  envexp BROWSER "firefox -P cys"
  envexp SSH_ASKPASS /usr/bin/ksshaskpass

  # Input method.
  envexp INPUT_METHOD "fcitx"
  envexp XMODIFIERS "@im=fcitx"

  # DPI variables.
  case "${DMI_PRODUCT_NAME}" in
    "HP EliteBook 755 G5" )
      envexp DPI 144
      envexp DPI_SCALE 1.5
      ;;
  esac

  # Wayland specific settings.
  if [ "${XDG_SESSION_TYPE}" = "wayland" ]; then
    envexp MOZ_ENABLE_WAYLAND 1
    envexp GDK_BACKEND wayland
    envexp QT_QPA_PLATFORM wayland
    envexp SDL_VIDEODRIVER wayland
  fi

  # XDG desktop specific settings.
  case "${XDG_CURRENT_DESKTOP}" in
    "GNOME" )
      envexp QT_QPA_PLATFORMTHEME gtk3
      ;;
    "KDE" )
      ;;
  esac
fi

# PATH
insert_path "$HOME/.local/bin"
insert_path "$HOME/.local/bin/monolith"
insert_path "$HOME/.local/bin/scripts"
insert_path "$HOME/.local/bin/wrappers"
insert_path "$HOME/.local/bin/control"
envexp PATH

# Proxies.
if [ -f "$XDG_CONFIG_HOME/clash/settings-private.yaml" ]; then
  PROXY_AUTH=$(yq -r '.["authentication"][0]' \
                  "$XDG_CONFIG_HOME/clash/settings-private.yaml")
  if [ "$PROXY_AUTH" = null ]; then
    unset -v PROXY_AUTH
  else
    envexp PROXY_AUTH
    envexp ALL_PROXY "socks5h://$PROXY_AUTH@127.0.0.1:7891"
  fi
fi
if [ "$PROXY_AUTH" = "" ]; then
  envexp ALL_PROXY socks5h://127.0.0.1:7891
fi
envexp http_proxy "$ALL_PROXY"

# Non-standard execution required settings.
if [ "$XDG_SESSION_TYPE" = "tty" ]; then
  run_if_exists setfont /usr/share/kbd/consolefonts/ter-118n.psf.gz
fi
if which ruby 1>/dev/null 2>/dev/null; then
  envexp GEM_HOME "$(ruby -e 'puts Gem.user_dir')"
fi
if [ -n "$GEM_HOME" ]; then
  append_path "$GEM_HOME/bin"
fi

#
# ENABLE ENVIRONMENT SERVICE
#

if [ "$_uname_s" = "Linux" ]; then
  systemctl --user start envvar-setting.service
fi

#
# UNSET VARIABLES
#

unset -v _uname_s
unset -v _uname_o
