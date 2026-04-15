# START ASGS "goto" completion
# This file is in the public domain

_goto_completion()
{
    local cur
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"

    COMPREPLY=( $(compgen -W "init load goto sd rd lsd tailf rebuild clone edit" -- "$cur") )

    return 0
}

complete -o default -F _goto_completion goto

# END goto completion
