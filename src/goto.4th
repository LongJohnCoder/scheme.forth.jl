\ Words implementing GOTO
\ These are required for tail call optimization.

: goto ( cfa -- )
    R> drop >body >R ;

: goto-deferred ( cfa -- )
    R> drop >body @ >body >R ;
