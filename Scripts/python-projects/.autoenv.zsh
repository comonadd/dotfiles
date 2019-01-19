if [[ $autoenv_event == 'enter' ]]; then
  autoenv_source_parent

  _my_autoenv_venv_chpwd() {
    if [[ -z "$_ZSH_ACTIVATED_VIRTUALENV" && -n "$VIRTUAL_ENV" ]]; then
      return
    fi

    setopt localoptions extendedglob
    local -a venv
    venv=(./(../)#.venv(NY1:A))

    if [[ -n "$_ZSH_ACTIVATED_VIRTUALENV" && -n "$VIRTUAL_ENV" ]]; then
      if ! (( $#venv )) || [[ "$_ZSH_ACTIVATED_VIRTUALENV" != "$venv[1]" ]]; then
        unset _ZSH_ACTIVATED_VIRTUALENV
        echo "De-activating virtualenv: ${(D)VIRTUAL_ENV}" >&2

        # Simulate "deactivate", but handle $PATH better (remove VIRTUAL_ENV).
        if ! autoenv_remove_path $VIRTUAL_ENV/bin; then
          echo "warning: ${VIRTUAL_ENV}/bin not found in \$PATH" >&2
        fi

        # NOTE: does not handle PYTHONHOME/_OLD_VIRTUAL_PYTHONHOME
        unset _OLD_VIRTUAL_PYTHONHOME
        # NOTE: does not handle PS1/_OLD_VIRTUAL_PS1
        unset _OLD_VIRTUAL_PS1
        unset VIRTUAL_ENV
      fi
    fi

    if [[ -z "$VIRTUAL_ENV" ]]; then
      if (( $#venv )); then
        echo "Activating virtualenv: ${(D)venv}" >&2
        export VIRTUAL_ENV=$venv[1]
        autoenv_prepend_path $VIRTUAL_ENV/bin
        _ZSH_ACTIVATED_VIRTUALENV="$venv[1]"
      fi
    fi
  }
  add-zsh-hook chpwd _my_autoenv_venv_chpwd
  _my_autoenv_venv_chpwd
else
  add-zsh-hook -d chpwd _my_autoenv_venv_chpwd
fi
