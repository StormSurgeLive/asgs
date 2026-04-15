# START ASGS "define" completion
# This file is in the public domain

_define_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        config)
            COMPREPLY=( $(compgen -f -- "$cur") )
            compopt -o filenames
            ;;
        editor)
            COMPREPLY=( $(compgen -W "vim nano vi" -- "$cur") )
            compopt -o nospace
            ;;
        adcircdir|adcircbranch|adcircremote|scriptdir|scratchdir|workdir)
            # no completion (free-form input)
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcircdir adcircbranch adcircremote config editor scriptdir scratchdir workdir" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _define_completion define

# END define completion
