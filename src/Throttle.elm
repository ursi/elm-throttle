module Throttle exposing
    ( Throttle
    , create, update, try, ifNeeded
    )

{-|

@docs Throttle

@docs create, update, try, ifNeeded

-}


{-| Internally, a throttle keeps track of two things:

  - How many times [`update`](#update) needs to be applied before it will allow the next command to be executed (we'll call this the "counter")
  - A potential command to be executed when [`update`](#update) sets the counter to `0` (we'll call this the "stored command")

-}
type Throttle msg
    = Throttle Int Int (Maybe (Cmd msg))


{-| Create a throttle that will ensure that commands will be executed at most once per `n` applications of [`update`](#update). We'll call `n` the "counter maximum". The counter is initially set to `0` to ensure the execution of the first command is not delayed (see [`try`](#try)).

    throttle : Throttle msg
    throttle =
        Throttle.create n

-}
create : Int -> Throttle msg
create frames =
    Throttle frames 0 Nothing


{-|


## If the counter is `0` and there is a stored command:

Returns a `Throttle` whose counter is set to the counter maximum, and the stored command


## If the counter is `0` and there is not a stored command:

Returns an identical `Throttle` and `Cmd.none`

(**note:** As this case accomplishes nothing, if performance is a concern and you're using a subscription to update your throttle, use [`ifNeeded`](#ifNeeded) and this will never happen.)


## If the counter is not `0`:

Returns a throttle with a counter equal to the old counter minus one, and `Cmd.none`

-}
update : Throttle msg -> ( Throttle msg, Cmd msg )
update (Throttle maximum left maybeCmd) =
    if left == 0 then
        let
            ( newLeft, cmd ) =
                case maybeCmd of
                    Just cmd_ ->
                        ( maximum, cmd_ )

                    Nothing ->
                        ( 0, Cmd.none )
        in
        ( Throttle maximum newLeft Nothing, cmd )

    else
        ( Throttle maximum (left - 1) maybeCmd, Cmd.none )


{-| Try to execute a command.


## In the example below:

If `model.throttle`'s counter is `0`, `newThrottle` is a `Throttle` whose counter is set to the counter maximum, and `cmd` is `doSomething`.

If `model.throttle`'s counter is not `0`, `newThrottle` is a `Throttle` with `doSomething` set as the stored command, and `cmd` is `Cmd.none`.

    update msg model =
        case msg of
            MouseMoved ->
                let
                    ( newThrottle, cmd ) =
                        Throttle.try
                            doSomething
                            model.throttle
                in
                ( { model | throttle = newThrottle }, cmd )

-}
try : Cmd msg -> Throttle msg -> ( Throttle msg, Cmd msg )
try cmd (Throttle maximum left _) =
    if left == 0 then
        ( Throttle maximum maximum Nothing, cmd )

    else
        ( Throttle maximum left <| Just cmd, Cmd.none )


{-| If the throttle's counter is `0` and there is no stored command, return `Sub.none`, otherwise, return the subscription that was passed in.

This allows you to use high frequency subscriptions for throttling and not worry about it running when it's not needed.

-}
ifNeeded : Sub msg -> Throttle msg -> Sub msg
ifNeeded sub (Throttle _ left maybeCmd) =
    if left > 0 || maybeCmd /= Nothing then
        sub

    else
        Sub.none
