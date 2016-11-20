_vault() {
    local cur prev command commands
    COMPREPLY=()
    command="${COMP_WORDS[1]}"
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    commands="note list list-tags uri del search edit"

    if [ "$prev" = "-t" ] || [ "$command" = "list" -a "$prev" = "-T" ] ; then
        COMPREPLY=($(compgen -W "$(vault list-tags)" -- "$cur"))
        return 0
    elif [ "$prev" = "vault" ]; then
        COMPREPLY=($(compgen -W "$commands" -- "$cur"))
        return 0
    else
        return 1
    fi
}

complete -F _vault vault
