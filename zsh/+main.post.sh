# -*- mode: sh -*-
#!/usr/bin/env zsh



#———————————————————————————————————————————————————————————————————————————————
# ŝarĝinte

def_mk cp! cp -rf
def_mk mv! mv -f

#———————————————————————————————————————————————————————————————————————————————
# bibliotekoj

if chrootp; then
  path=(
    $path
    ${HOME}/.local/share/tresorit
  )
fi

#———————————————————————————————————————————————————————————————————————————————
# invito

if [[ "$TERM" == "dumb" ]]; then
  unsetopt zle prompt_cr prompt_subst
  for i in precmd preexec; do wh $i && unfn $i; done
  PS1="$ "
else
  precmd ()  { linux_pc_test && vcs_info; print -Pn "\e]0;%n %m:%1~\a" }
  preexec () { print -Pn "\e]0;%n %m:($1)\a" }
fi

autoload -Uz add-zsh-hook vcs_info
setopt prompt_subst
add-zsh-hook precmd vcs_info

def rgbcolor {
  echo "16 + $1 * 36 + $2 * 6 + $3" | bc
}

orange=$(rgbcolor 5 3 0)

#———————————————————————————————————————————————————————————————————————————————
# ŝanĝradikigmedioj

chrootp && CHROOT_PROMPT='%F{yellow}(C)%f'

#———————————————————————————————————————————————————————————————————————————————
# invito

if linux_pc_test || darwin_test; then
  if [[ "$TERM" == "dumb" ]]; then
    PS1='%n %m %~ %D{%n}⊻ '
  else
    PS1='%F{cyan}%B%n%b%f%B%F{yellow} %f%b%B%F{red}%m%f%b%F{yellow}%B %b%f%F{yellow}%B%~%b%f${vcs_info_msg_0_}%b%D{%n}%F{green}%B>%b%f%F{white} '
  fi
else
  PS1='%n %m %0d %D{%n} '
fi

#———————————————————————————————————————————————————————————————————————————————
# kompletigoj

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' squeeze-slashes true

zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:d:*' ignored-patterns '(*/)#CVS'
zstyle ':completion:*:d:*' ignore-parents parent pwd

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# zstyle ':completion:*:*:o:*' menu yes select
# zstyle ':completion:*:*:o:*' file-sort time

comps=(
  d "cd"
  s "sudo"
); def_comps

#———————————————————————————————————————————————————————————————————————————————
# dosierujoj

CDX=(
  # ${HOME}/kampu/*(/)
  # ${HOME}/vti/*(/)
  ${HOME}/cl/*
  ${HOME}/org/*
); def_cdx


#———————————————————————————————————————————————————————————————————————————————
# docker

fpath=(/Users/ebzzry/.docker/completions $fpath)
autoload -Uz compinit
compinit

#———————————————————————————————————————————————————————————————————————————————
# direnv

which direnv > /dev/null 2>&1 && eval "$(direnv hook zsh)"
