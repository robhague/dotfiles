# Utilities
function source_if_present {
    if [ -f $1 ]
    then
        source $1
    fi    
}

function pathadd {
    PATH=:$PATH
    PATH=$1${PATH//:$1/}
}

# Prompt
function last_exit_prompt {
    STATUS=$?
    if [[ $STATUS > 0 ]]
    then
        echo "!$STATUS "
    fi
}
function git_branch_prompt {
    git branch --no-color 2> /dev/null | \
	sed -e '/^[^*]/d' -e 's/* \(.*\)/git:\1 /'
}
function virtual_env_prompt {
    if [ $VIRTUAL_ENV ]
    then
        echo "Python:$(basename $VIRTUAL_ENV) "
    fi
}
PS1="\
\[\e[33m\]\t \
\[\e[31m\]\$(last_exit_prompt)\
\[\e[32m\]\$(virtual_env_prompt)\
\$(git_branch_prompt)\
\[\e[36m\]\w \n\$ \[\e[m\]\
"
export PS1
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Command Completion
source_if_present ~/.git-completion.bash

# Path
pathadd ~/scripts
pathadd ~/bin
export PATH

# Source local config
source_if_present ~/.bashrc.local
