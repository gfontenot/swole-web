module Lens exposing ( Lens )

type alias Lens whole part = {
    get : whole -> part,
    set : part -> whole -> whole
}
