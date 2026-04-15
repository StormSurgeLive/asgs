# START ASGS "move" completion
# This file is in the public domain

_move_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        statefile)
            # no further completion after subcommand
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "statefile" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _move_completion move

# END move completion
