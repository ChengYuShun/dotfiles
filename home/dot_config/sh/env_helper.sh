envsrc() {
  if eval [ '"$'$1_HAS_BEEN_SET_BY_CYS'"' != YES ]; then
    . ~/.config/profile.d/$1.sh
    envexp $1_HAS_BEEN_SET_BY_CYS YES
  fi
}

envexp() {
  _key="$1"
  _val="$2"

  if ! [ -n "$_val" ]; then
    eval _val=\$$_key
  fi
  if [ -n "$_val" ]; then
    export "$_key=$_val"
    if [ "$UNAME_S" = Linux ]; then
      systemctl --user set-environment "$_key=$_val"
    elif [ "$UNAME_S" = Darwin ]; then
      launchctl setenv "$_key" "$_val"
    fi
  fi

  unset -v _key
  unset -v _val
}

if ! [ -n "$UNAME_S" ]; then
  UNAME_S="$(uname -s)"
  envexp UNAME_S
fi
