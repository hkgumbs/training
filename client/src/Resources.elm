module Resources exposing (..)

import Postgrest as PG


type Client
    = Client


client =
    PG.resource Client
        "clients"
        { id = PG.string
        , name = PG.string
        , name = PG.string
        }
