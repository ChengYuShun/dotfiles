function sudo-pacman-update
    set abs (readlink -f $argv)
    sudo pacman -U "file://$abs"
    sudo pacman -U $abs
end
