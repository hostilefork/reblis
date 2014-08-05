REBOL [
    Title: "Redis Asset Scraper to Generate Some Reblis Source Files"
    Description: {
        Makes a Rebol-style command table out scraped Redis resources, such as
        the "redis-command-table" in the redis.c file:

        https://github.com/antirez/redis/blob/a1b1c1ea3adb864c8c2e3feb4eaed890a9b505cf/src/redis.c#L69

        ...along with the information encoded in the JSON commands list:

        https://raw.github.com/antirez/redis-doc/master/commands.json

        This table assists in implementing the Rebol DO-dialect function
        prototypes, however it cannot write the entire functions themselves.
        Whether it is worth actually separating the "prototype" from the
        "implementation" such that the product of this script winds up being a
        Reblis source file that is never changed by hand remains to be seen.

        Another option would be to merely re-run this script from time to time
        and see if the output changes, and if so patch those changes into the
        main codebase by hand.
    }

    Author: "Hostile Fork"
    Home: http://hostilefork.com/
    License: 'apache-2.0

    Date: 3-Oct-2012
    Version: 1.2.0

    ; Header conventions: http://www.rebol.org/one-click-submission-help.r
    File: %redis-scraper.rebol
    Type: 'tool
    Level: 'intermediate

    Usage: { make-command-table }
]

; http://stackoverflow.com/questions/24412153/

problem: func [str [string!]] [
    do make error! str
]

make-command-table: does [

    ;----
    ; FIRST PHASE - Read the files into Rebol-processable structures
    ;----
    ; fields marked with a * are useless to us

    tableFields: [
        name            ; a string representing the command name.
        function*       ; pointer to the C function implementing the command.
        arity           ; number of arguments, it is possible to use -N to say >= N
        sflags          ; command flags as string. See below for a table of flags.
        flags*          ; flags as bitmask. Computed by Redis using the 'sflags' field.
        get_keys_proc*  ; an optional function to get key arguments from a command.
                        ; This is only used when the following three fields are not
                        ; enough to specify what arguments are keys.
        first_key_index*; first argument that is a key
        last_key_index* ; last argument that is a key
        key_step*       ; step to get all the keys from first to last argument. For instance
                        ; in MSET the step is two since arguments are key,val,key,val,...
        microseconds*   ; microseconds of total execution time for this command.
        calls*          ; total number of calls of this command.
    ]

    flag-meanings: [
        [
            #"w" write
            {write command (may modify the key space)}
        ] [
            #"r" read-only
            {read command  (will never modify the key space)}
        ] [
            #"m" may-allocate
            {may increase memory usage once called. Don't allow if out of memory.}
        ] [
            #"a" admin
            {admin command, like SAVE or SHUTDOWN.}
        ] [
            #"p" pub-sub
            {Pub/Sub related command.}
        ] [
            #"f" force-replicate
            {force replication of this command, regarless of server.dirty.}
        ] [
            #"s" not-scriptable
            {command not allowed in scripts.}
        ] [
            #"R" non-deterministic
            {random command. Command is not deterministic, that is, the same command
            with the same arguments, with the same key space, may have different
            results. For instance SPOP and RANDOMKEY are two random commands.}
        ] [
            #"S" sort-output
            {Sort command output array if called from script, so that the output
            is deterministic.}
        ] [
            #"l" loading-okay
            {Allow command while loading the database.}
        ] [
            #"t" stale-data-okay
            {Allow command while a slave has stale data but is not allowed to
            server this data. Normally no command is accepted in this condition
            but just a few.}
        ] [
            #"M" no-monitor-propagate
            {Do not automatically propagate the command on MONITOR.}
        ] [
            #"k"
            {Perform an implicit ASKING for this command, so the command will
            be accepted in cluster mode if the slot is marked as 'importing'.}
        ] [
            #"F"
            {Fast command: O(1) or O(log(N)) command that should never delay
            its execution as long as the kernel scheduler is giving us time.
            Note that commands that may trigger a DEL as a side effect (like
            SET) are not fast commands.}
        ]
    ]


    get-row-field: function [row [block!] field [word!]] [
        pos: find tableFields field
        unless pos [
            problem "unknown table field specified"
        ]
        return pick row index? pos
    ]

    unless parse load/type %../redis/src/redis.c 'text [
        thru "struct redisCommand redisCommandTable[] = "
        copy redis-command-table thru "};"
        to end
    ] [
        problem "Redis command description table not successfully found in redis.c"
    ]

    ; semicolon helped detect the ending brace of the table, but lose it

    take back tail redis-command-table

    commands-json: load/type %commands.json 'text

    ; in the JSON there are cases where strings are followed by colons, which
    ; are conceptually like Rebol's SET-WORD!  But some of them contain spaces
    ; in this case, so we'll handle that by turning them into hyphens (which
    ; JavaScript would interpret as subtraction, as it doesn't require spaces
    ; between tokens)

    parse commands-json [
        any [
            ; first part of the rule is required, a matched pair of quotes.  We
            ; capture the series positions before the first quote, at start of
            ; what will become the SET-WORD!'s name, and at end of that name

            to {"} before-pos: skip start-pos: to {"} end-pos: skip

            ; second part of the rule is optional, if we find a colon after that

            opt [
                {:}

                ; If we don't get to here, there was no colon and optional rule
                ; will stop trying to match.  But if we DID get here, the parser
                ; continues churning away; we escape into open code via PAREN!
                ; to do the insertion of the hyphen

                (
                    setWordString: copy/part start-pos end-pos
                    replace/all setWordString space "-"
                    change start-pos setWordString
                )

                ; We also want to remove the quotes in this case where it was
                ; supposed to be a SET-WORD!, but changing the length of the
                ; series inside open code without telling the parser is sketchy,
                ; right?  There is an "approved" REMOVE instruction in the
                ; parse dialect so we'll use that. We're still sitting at the
                ; point right after the colon, so jump the parser's position
                ; back to before the first quote

                :before-pos

                ; Do the removals through a match rule.  We know they are there
                ; and this will not cause our match rule to fail because we
                ; matched this case in the first pass through

                remove [thru {"}] to {"} remove [{"}]
            ]
        ]
    ]

    ; In both C and JSON, we want to wipe out the commas and change the brace
    ; characters into brackets

    foreach code-string [redis-command-table commands-json] [
        foreach [before after] [
            "{" "["
            "," " "
            "}" "]"
        ] [
            replace/all (get code-string) before after
        ]
    ]

    ; that should be all we need to LOAD these arrays into a Rebol structure
    ; for further (and easier) processing

    table-block: load redis-command-table
    commands-block: load commands-json



    ;----
    ; SECOND PHASE - As commands.json is not "executable" in the C Redis, it is
    ; not necessarily consistent.  We repair apparent anomalies in the Redis
    ; spec so it may be processed by a more consistent method when used to
    ; auto-generate Rebol
    ;----

    ; currently their command help is in alphabetical order, but no guarantee
    ; they might not be.  sort by the keys as we're going to visit them in
    ; alphabetical order to restructure and merge the subcommands, getting rid
    ; of those hyphenated bits

    map-stem-to-commands: map copy []

    pos: commands-block
    while [not empty? pos] [
        if (not set-word? pos/1) or (not block? pos/2) [
            probe pos
            problem {Source JSON did not have alternating "foo": { ... } style}
        ]

        command-string: to-string pos/1
        take back tail command-string ; it's a set-word, take off the colon
        command: to-word command-string

        if parse command-string [copy stem-string to {-} to end] [
            if not select map-stem-to-commands to-word stem-string [
                append map-stem-to-commands compose/only [(to-set-word stem-string) (copy [])]
            ]
            append (select map-stem-to-commands (to-word stem-string)) command
        ]

        ; The commands.json file has an odd concept of what an "enum" can be,
        ; including saying that a parameter can be a "single-element enum". 
        ; For instance, look at the definition for SHUTDOWN:

        ; "SHUTDOWN": {
        ;     "summary": "Synchronously save the dataset to disk and then shut down the server",
        ;     "arguments": [
        ;         {
        ;             "name": "NOSAVE",
        ;             "type": "enum",
        ;             "enum": ["NOSAVE"],
        ;             "optional": true
        ;         },
        ;         {
        ;             "name": "SAVE",
        ;             "type": "enum",
        ;             "enum": ["SAVE"],
        ;             "optional": true
        ;         }
        ;     ],
        ;     "since": "1.0.0",
        ;     "group": "server"
        ; },

        ; Er, what?  Optional arguments with only a single value they can have?
        ; In Rebol we'd just call that a "refinement" with no parameters.  And
        ; Redis seems to have the idea of a "command" parameter which I'm
        ; treating as a refinement, but maybe however they're doing it, a
        ; "command" needs at least one argument so it couldn't fit.  Anyway
        ; this hardly strikes me as an enum, so I transform any cases that
        ; look like this into Redis-style zero-argument commands...which we
        ; later turn into refinements.

        foreach arguments pos/2/arguments [
            if ("enum" = arguments/type) and (1 = length? arguments/enum) [

                if arguments/name != first arguments/enum [

                    ; Some exceptional cases.  For instance: sort has an optional
                    ; parameter called "sorting"...which might conceptually be
                    ; a refinement like
                    ;      SORT/SORTING (...) 'ALPHA
                    ;      SORT/SORTING (...) 'NUMERIC
                    ; or whatever.  But for the moment it's just alpha, so it makes
                    ; more sense to have no lit-word enum parameter and just make
                    ; it SORT/ALPHA

                    either all [
                        arguments/name = "sorting"
                        "ALPHA" = first arguments/enum
                    ] [
                        arguments/name: "ALPHA"
                    ] [
                        problem "Found single element enum whose name didn't match argument"
                    ]
                ]

                ; axe the enum block

                remove/part find arguments 'enum 2

                ; make the command name what used to be the name
                ; line break is intentional (it's for formatting)

                insert arguments compose [command: (arguments/name)
                ]

                ; wipe out the type and name to empty blocks

                arguments/type: copy []
                arguments/name: copy []
            ]

            ; some commands list as "multiple" and others as "variadic" (?)
            ; is there a difference?

            if arguments/command [
                if true? arguments/multiple [
                    insert (remove/part find arguments 'multiple 2) compose [
                    variadic: true]
                ]
            ]

            ; BITCOUNT claims it supports multiple START/ENDs but it doesn't
            ; actually work.  It's really just optional, so we turn that into
            ; a RANGE command, hence a /RANGE refinement

            if all [
                command = 'BITCOUNT
                block? arguments/name
            ] [
                insert arguments [
                    command: "RANGE"
                    optional: true
                ]
                remove/part find arguments 'multiple 2
            ]

            ; The enum for TYPE in CLIENT-KILL doesn't have a name field
            ; Oversight?

            if all [
                command = 'CLIENT-KILL
                arguments/command = "TYPE"
            ] [
                insert find arguments 'type [
                    name: "type-value"
                ] 
            ]

            ; special case as used by HMSET(NX)
            ; we just use a block! in this case always...
            ; http://redis.io/commands/hmset

            if find [HMSET HMSETNX] command [
                if all [
                    arguments/name = ["field" "value"]
                    arguments/type = ["string" "string"]
                ] [
                    arguments/name: copy "field-value-pairs"
                    arguments/type: copy "block"

                    unless true? arguments/multiple [
                        problem "Convention has changed... no multiple?"
                    ]

                    ; since we always demand a block, we get rid of the multiple
                    remove/part find arguments 'multiple 2
                ]
            ]

            ; special case as used by MSET(NX)
            ; once again, just use a block...

            if find [MSET MSETNX] command [
                if all [
                    arguments/name = ["key" "value"]
                    arguments/type = ["key" "string"]
                ] [
                    arguments/name: copy "key-value-pairs"
                    arguments/type: copy "block"

                    unless true? arguments/multiple [
                        print mold arguments
                        problem "Convention has changed... no multiple?"
                    ]

                    ; since we always demand a block, we get rid of the multiple
                    remove/part find arguments 'multiple 2
                ]
            ]

            if 'SUBSCRIBE = command [
                ; why are these names and types in blocks, anyway

                if all [
                    arguments/name = ["channel"]
                    arguments/type = ["string"]
                ] [
                    arguments/name: copy "channel"
                    arguments/type: copy "string"
                ]
            ]

            if 'PSUBSCRIBE = command [
                ; again...what's with the block?

                if all [
                    arguments/name = ["pattern"]
                    arguments/type = ["pattern"]
                ] [
                    arguments/name: "pattern"
                    arguments/type: "pattern"
                ]
            ]

            if 'SORT = command [
                ; didn't put it in a block.  I want it in a block

                if all [
                    arguments/name = "pattern"
                    arguments/type = "pattern"
                    arguments/command = "BY"
                ] [
                    arguments/name: copy ["pattern"]
                    arguments/type: copy ["pattern"]
                ]

                if all [
                    arguments/name = "pattern"
                    arguments/type = "string"
                    arguments/command = "GET"
                ] [
                    arguments/name: copy ["pattern"]
                    arguments/type: copy ["string"]
                ]

                if all [
                    arguments/name = "destination"
                    arguments/type = "key"
                    arguments/command = "STORE"
                ] [
                    arguments/name: copy ["destination"]
                    arguments/type: copy ["key"]
                ]
            ]

            if find [ZINTERSTORE ZUNIONSTORE] command [
                if all [
                    arguments/name = "weight"
                    arguments/type = "integer"
                    arguments/command = "WEIGHTS"
                ] [
                    arguments/name: copy ["weight"]
                    arguments/type: copy ["integer"]
                ]

                ; ZINTERSTORE/ZUNIONSTORE has a weird case of a command that
                ; takes an enum. There is no way in Redis specification to have
                ; more than one enum, so it describes a single argument.  If
                ; it's a single argument, I'm demoting it to just an optional
                ; parameter instead of a command
                ;
                ; {
                ;     "command": "AGGREGATE",
                ;     "name": "aggregate",
                ;     "type": "enum",
                ;     "enum": ["SUM", "MIN", "MAX"],
                ;     "optional": true
                ; }

                if arguments/command = "AGGREGATE" [
                    remove/part find arguments 'command 2
                ]
            ]

            ; ZADD takes a variable number of arguments, so in the Rebol world
            ; that has to be done with a block.

            if all [
                command = 'ZADD
                block? arguments/name
            ] [
                arguments/name: "scores-and-members"
                arguments/type: "block"
                remove/part find arguments 'multiple 2
            ]

            if arguments/command [
                if arguments/multiple [
                    ; These are really the same thing...I think, but let's check
                    probe arguments
                    problem "A command can be 'variadic' but not 'multiple'"
                ]
                if arguments/variadic [
                    unless 1 = length? arguments/type [
                        probe command
                        probe arguments

                        problem "Not sure what to do about variadic commands with more than one argument"
                    ]
                ]
            ]
        ]

        ; The SUBSTR command is still mentioned in the C function table,
        ; although it no longer exists in commands.json as it's now GETRANGE
        ;
        ;     http://redis.io/commands/getrange

        ; Meanwhile, the CLUSTER stuff is specified and in the codebase, but
        ; not as of yet in the commands.json
        ;
        ;     http://redis.io/topics/cluster-spec
        ;
        ; So the safest thing is just to have this script speak up when it
        ; notices that cluster appears.

        if find [CLUSTER ASKING] command [
            problem "Redis has brought cluster functionality online since script written"
        ]

        ; There is no zrevrangebylex in the commands.json file; if it gets
        ; added then take this bit out.  I raised a GitHub issue:
        ; https://github.com/antirez/redis-doc/issues/398

        if find [
            ZREVRANGEBYLEX
            REPLCONF
            PSYNC
            PFSELFTEST
            PFDEBUG
            LATENCY
            RESTORE-ASKING
            READONLY
            READWRITE
            WAIT
        ] [
            problem "Some manually added command duplicated in table"
        ]

        table-row-pos: table-block
        while [not empty? table-row-pos] [
            table-row: first table-row-pos
            stem: get-row-field table-row 'name
            either find ["substr" "cluster" "asking"] stem [
                take table-row-pos
            ] [
                table-row-pos: next table-row-pos
            ]
        ]

        pos: next next pos
    ]

    ; The REPLCONF command is "internal"...whatever that means.  It deals with
    ; replication, and was added in this commit:
    ;
    ;     https://github.com/antirez/redis/commit/b998ebe904b49b447ec04639f5d3e705d1d0ea7a
    ;
    ; I'm not versed enough in Redis to know what the effective difference
    ; between an internal command and a function is.  I'd guess it means that
    ; one Redis instance can call it on another through some kind of scripting
    ; protocol, but people aren't supposed to know it's there.  But if it can
    ; be executed then it needs to be in the interface, "internal" or not.

    append commands-block copy/deep [
        LATENCY: [
            summary: "Internal, undocumented."
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        PFDEBUG: [
            summary: "Internal, undocumented."
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        PFSELFTEST: [
            summary: "Internal, undocumented."
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        PSYNC: [
            summary: "Internal command used for replication"
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        REPLCONF: [
            summary: "Internal replication functionality, undocumented"
            complexity: "Undocumented, unknown."
            since: "2.2.0"
            group: "internal"
            arguments: [
                [
                    command: "LISTENING-PORT"
                    name: ["port"]
                    type: ["integer"]
                    optional: true
                ]
            ]
        ]
        RESTORE-ASKING: [
            summary: "Asking variant of create a key using the provided serialized value, previously obtained using DUMP."
            complexity: "O(1) to create the new key and additional O(N*M) to recostruct the serialized value, where N is the number of Redis objects composing the value and M their average size. For small string values the time complexity is thus O(1)+O(1*M) where M is small, so simply O(1). However for sorted set values the complexity is O(N*M*log(N)) because inserting values into sorted sets is O(log(N))."
            since: "2.6.0"
            group: "generic"
            arguments: [
                [
                    name: "key"
                    type: "key"
                ] [
                    name: "ttl"
                    type: "integer"
                ] [
                    name: "serialized-value"
                    type: "string"
                ]

            ]
        ]
        READONLY: [
            summary: "Internal, undocumented."
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        READWRITE: [
            summary: "Internal, undocumented."
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        WAIT: [
            summary: "Internal, undocumented."
            complexity: "Undocumented, unknown."
            since: "3.0"
            group: "internal"
        ]
        ZREVRANGEBYLEX: [
            summary: "Return a range of members in a sorted set, by lexicographical range, with the scores ordered from high to low."
            complexity: "O(log(N)+M) with N being the number of elements in the sorted set and M the number of elements being returned. If M is constant (e.g. always asking for the first 10 elements with LIMIT), you can consider it O(log(N))."
            since: "2.8.9"
            group: "sorted_set"
            arguments: [
                [
                    name: "key"
                    type: "key"
                ] [
                    name: "max"
                    type: "string"
                ] [
                    name: "min"
                    type: "string"
                ] [
                    command: "LIMIT"
                    name: ["offset" "count"]
                    type: ["integer" "integer"]
                    optional: true
                ]
            ]
        ]
    ]

    sort/skip commands-block 2



    ;----
    ; THIRD PHASE - Convert all arguments into Rebol style args and refinements
    ;----

    foreach [command info-block] commands-block [

        ; we enumerate and see command as a SET-WORD!, but we turn it into
        ; an ordinary WORD!

        command: to-word command
        print rejoin ["Now processing: " to-string command]

        ; Precedent seems to use LIT-WORD! to implement the concept of an
        ; enumerated type in a block which *might* sometime be evaluated

        info-block/group: to-lit-word info-block/group

        ; I don't actually know if turning the version number into a TUPLE!
        ; from a STRING! is a great idea.  Tuples kind of weird me out a bit,
        ; because you can say TO-TUPLE "1.2" and get back 1.2.0 while
        ; TO-TUPLE "1.2.3.4" is 1.2.3.4 - anyway, the Rebol script archive uses
        ; them for versioning so I guess it's Rebolish to do this, and it
        ; catches errors.

        info-block/since: to-tuple info-block/since

        ; we will be generating something called "parameters" which is the DO
        ; dialect style arguments to a function

        append info-block copy/deep [
            parameters: []]

        arg-list-block: info-block/arguments
        pos: arg-list-block
        while [not empty? pos] [
            arguments: pos/1
            new-args: copy []

            ; if it's a command, we'd consider it a refinement with arguments
            ; the blocks are just matched up pairs of arguments

            either arguments/command [
                is-arg-optional: false

                either arguments/optional [
                    append new-args to-refinement lowercase arguments/command
                ] [
                    problem "found non-optional command parameter, hence not a refinement"
                ]

                ; temporary, let's just do one and then enumerate...

                case [
                    all [
                        string? arguments/name
                        string? arguments/type
                    ] [
                        name-block: compose [(arguments/name)]
                        type-block: compose [(arguments/type)]
                    ]

                    all [
                        block? arguments/name
                        block? arguments/type
                    ] [
                        name-block: arguments/name
                        type-block: arguments/type
                    ]

                    true [
                        probe arguments/name
                        probe arguments/type

                        problem "command has something other than a block for name & type list"
                    ]
                ]

            ] [
                is-arg-optional: arguments/optional

                unless (string? arguments/name) and (string? arguments/type) [
                    probe arguments/name
                    probe arguments/type
                    problem "name and type were not strings and no special exemption made"
                ]

                ; now make them blocks for consistency in processing w/command

                name-block: compose [(arguments/name)]
                type-block: compose [(arguments/type)]
            ]

            unless (length? name-block) = (length? type-block) [
                probe name-block
                probe type-block
                problem "number of names and types in argument blocks aren't equal"
            ]

            while [not empty? name-block] [
                arg-name: name-block/1
                type-name: type-block/1

                ; take out any colons or spaces (such as server:port becomes
                ; server+port) these would be illegal WORD!-class names in Rebol

                foreach name [arg-name type-name] [
                    replace/all get name ":" "+"
                    replace/all get name " " "-"

                    ; there is a "yes/no" name (not legal it would be a path)
                    replace/all get name "/" "-"
                ]

                ; if optional use the "refinement" format...not legal if we're
                ; in a command-style that's already a refinement

                either is-arg-optional [
                    append new-args to-refinement arg-name

                    ; an optional argument is a refinement in Rebol, but the
                    ; refinement needs a name *and* the parameter needs a
                    ; unique name too.  Generate the parameter by sticking
                    ; "-value" onto it.

                    new-line (append new-args to-word rejoin [arg-name "-value"]) true
                ] [
                    new-line (append new-args to-word arg-name) true
                ]

                ; From: http://redis.io/topics/data-types-intro
                ; "Redis keys are binary safe, this means that you can use any
                ; binary sequence as a key, from a string like "foo" to the
                ; content of a JPEG file. The empty string is also a valid key.

                allowed-types: copy []
                arg-comment: none

                switch/default type-name [
                    "key" [
                        append allowed-types [string! binary!]
                        arg-comment: {key!}
                    ]

                    "enum" [
                        if 1 = length? arguments/enum [
                            problem "Uncaught 'single-element enum' that should be a command"
                        ]

                        ; We want a help string comment for the parameter
                        ; saying which words are legal

                        enum-options: copy {}
                        foreach enum-string arguments/enum [
                            append enum-options rejoin [space {'} enum-string]
                        ]

                        ; leading space from enum-string...

                        arg-comment: rejoin [{enum with choices:} enum-options]
                        append allowed-types [word!]
                    ]

                    "double" [ append allowed-types [decimal!] ]

                    "integer" [ append allowed-types [integer!] ]

                    "string" [ append allowed-types [string!] ]

                    "posix-time" [ append allowed-types [time!] ]

                    ; Not a Redis type, we inject it above...

                    "block" [ append allowed-types [block!] ]

                    ; I asked about this and whether a pattern is just a string
                    ; http://stackoverflow.com/questions/12984480/

                    "pattern" [
                        append allowed-types [string!]
                        arg-comment: {pattern}
                    ]
                ] [
                    problem rejoin ["Unknown type (" type-name ") specified by Redis command table"]
                ]

                if any [
                    true? arguments/multiple
                    true? arguments/variadic
                ] [
                    if type-name = "block" [
                        problem "Are there supposed to be multiple or variadics on a block type?"
                    ]

                    append allowed-types [block!]
                ]

                append/only new-args allowed-types
                if arg-comment [
                    append new-args arg-comment
                ]

                name-block: next name-block
                type-block: next type-block
            ]

            ; add the new-args to the parameters section
            
            append info-block/parameters new-args

            ; axe the JSON-style argument data from which the parameters
            ; were generated

            remove/part find info-block 'arguments 2

            pos: next pos
        ]
    ]

    probe map-stem-to-commands

    ;----
    ; FOURTH PHASE - Integrate data about runtime properties from the redis.c
    ; table and do a sanity check to make sure any redundant information
    ; matches the table above
    ;----

    foreach table-row table-block [
        flag-string: get-row-field table-row 'sflags
        flag-block: copy []
        foreach meaning flag-meanings [
            if find/case flag-string (first meaning) [
                append flag-block (second meaning)
            ]
        ]
        if empty? flag-block [
            problem "Didn't find at least one command to put the flags on."
        ]

        stem: get-row-field table-row 'name

        command-set: select map-stem-to-commands to-word stem
        if not command-set [
            command-set: compose [(to-word stem)]
        ]

        foreach command command-set [
            print rejoin ["Finalizing command: " to-string command]
            command-info: select commands-block command
            unless command-info [
                probe command-set
                problem "Could not find information block for command"
            ]

            if select command-info 'flags [
                problem "Trying to add flags to a command that already has flags set"
            ]

            append command-info compose/only [
                flags: (flag-block)]
        ]
    ]

    return mold/only commands-block
]

write %../redis-commands.ren make-command-table
