# elm-throttle

`elm-throttle` is a simple throttling API that uses a counter-based approach as apposed to a time-based one. This is greate because it allows you to throttle based on anything you want, including time! For example:

- `Time.every`
- `Browser.Events.onAnimationFrame`
- Waiting for an HTTP request to return a result before you send another one

Here is an example of throttling a command called `doSomething` that processes mouse movement. It is set to only be executed at most once every 100 ms.
```elm
type alias Model =
    { throttle : Throttle Msg }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { throttle = Throttle.create 1 }, Cmd.none )


type Msg
    = UpdateThrottle
    | MouseMoved MousePosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateThrottle ->
            let
                ( newThrottle, cmd ) =
                    Throttle.update model.throttle
            in
            ( { model | throttle = newThrottle }, cmd )

        MouseMoved mousePosition ->
            let
                ( newThrottle, cmd ) =
                    Throttle.try
                        (doSomething MousePosition)
                        model.throttle
            in
            ( { model | throttle = newThrottle }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Throttle.ifNeeded
            (Time.every 100 (\_ -> UpdateThrottle))
            model.throttle
        , Browser.Events.onMouseMove mousePositionDecoder
        ]
```

