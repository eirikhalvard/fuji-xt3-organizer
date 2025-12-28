# fuji

## Setup

## Usage

`stack install`

For building and installing the `fuji` script on your PATH, `stack install` will build and install the `fuji` executable on the PATH.

---

`fuji --help`

will show a user guide for the `fuji` script

---

add the following to .zshrc:

`source <(fuji --zsh-completion-script `which fuji`)`

```bash
if [ $commands[fuji] ]; then
  mkdir -p ~/.oh-my-zsh/completions
  fuji --zsh-completion-script $(which fuji) > "${ZSHCOMPDIR}/_fuji"
fi
```

will install auto completion for zsh.
