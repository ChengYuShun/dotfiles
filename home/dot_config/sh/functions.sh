# coreutils
mkcd () {
  mkdir -p $1
  cd -P $1
}

cdl () {
  cd -P $1
  ls -lha .
}

# git
git-push () {
  for f in $(ls ~/repo)
  do
    echo $f:
    git --git-dir=$HOME/repo/$f/.git --work-tree=$HOME/repo/$f\
      push $1 --all --force
    git --git-dir=$HOME/repo/$f/.git --work-tree=$HOME/repo/$f\
      push $1 --tags --force
  done
}
