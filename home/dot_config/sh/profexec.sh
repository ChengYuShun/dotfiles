. ~/.config/sh/env_helper.sh
envsrc "$1"
shift 1
exec "$@"
