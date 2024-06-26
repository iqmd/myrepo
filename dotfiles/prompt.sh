#!/bin/bash
# ~/.bashrc.d/prompt
#

_PROMPT() {
    local EXIT_STATUS="$?"

    # Other variables
    local _time12h="\T" _time12a="\@" _time24h="\t" _time24s="\A" _cwd_short="\W" \
          _cwd_full="\w" _new_line="\n" _jobcount="\j"

    # Set variables for PS1 string
    local _br1="\[${bold}${black}\][\[${reset}\]" _br2="\[${bold}${black}\]]\[${reset}\]" \
          _div1="\[${bold}${black}\] » \[${reset}\]" _div2="\[${bold}${black}\]╺─╸\[${reset}\]" \
          _line1="\[${red}\]┌─╼\[${reset}\]" _line2="\[${red}\]└────╼\[${reset}\]" \
          _sep="\[${reset}\]@" _title_sep="@" 
    local _cwd="${_br1}${_cwd_short}${_br2}" _title_cwd="[${_cwd_short}]" \
          _time="${_br1}${_time24h}${_br2}" 

    # Exit status string
    [ $EXIT_STATUS != 0 ] && local _errmsg="${_br1}\[${bold}${red}\]${EXIT_STATUS}${_br2}"

    # User
    if (( UID == 0 )); then
        local _user="\[${red}\]root\[${reset}\]" _title_user="root"
    else
        local _user="\[${green}\]\u\[${reset}\]" _title_user="\u"
    fi

    # Hostname
    if [[ $SSH_TTY ]]; then
        local _host="\[${blue}\]\h" _title_ssh="ssh://" _title_host="\h"
    else
        local _host="\[${yellow}\]\h" _title_ssh="" _title_host="\h"
    fi
    
    # Jobs
    if [[ $(echo -n `jobs | grep -Ec \[[:digit:]+\]`) != 0 ]]; then
        local _jobs="${_br1}\[${bold}${green}\]${_jobcount}${_br2}"
    else
        local _jobs=""
    fi

    # Get the Git shell 
    local _git_branch="$(__git_ps1 "%s")"

    if [[ ${_git_branch} != "" ]] && [[ ${PWD} =~ "/.git" ]]; then
        # CWD is inside a .git directory
        local _git="${_br1}\[${red}\]${_git_branch}${_br2}" _title_git="[${_git_branch}]"
    elif [[ ${_git_branch} != "" ]]; then
        local _title_git="[${_git_branch}]" 
        if [[ -z $(git status -s) ]] && ( [[ ${_git_branch} == "master" ]] || [[ ${_git_branch} == "master=" ]] ); then
            # CWD is inside an unmodified master branch
            local _git="${_br1}\[${green}\]${_git_branch}${_br2}" 
        elif [[ -z $(git status -s) ]]; then
            # CWD is inside an unmodified branch
            local _git="${_br1}\[${yellow}\]${_git_branch}${_br2}" 
        else
            # CWD is inside a modified branch
            local _git="${_br1}\[${red}\]${_git_branch}${_br2}"
        fi
    else
        # Not inside a Git branch
        local _git="" _title_git="" 
    fi

    # Set title in xterm/rxvt
    case $TERM in
        xterm*|rxvt*)
            local _title="\[\e]2;${_title_ssh}${_title_user}${_title_sep}${_title_host} ${_title_cwd} ${_title_git}\007\]" ;;
        *)
            local _title="" ;;
    esac
    
    # Set PS1
    PS1="${_title}${_new_line}${_line1}${_div1}${_user}${_sep}${_host} ${_div2} ${_errmsg}${_jobs}${_cwd}${_git}${_new_line}${_line2} "
    export PS2='continue >'
    export PS4='+$BASH_SOURCE[$LINENO]: '
}

# Prompt
PROMPT_COMMAND=_PROMPT
