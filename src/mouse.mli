type position = 
  { x: int;
    y: int;
  }

val position : position BsOakJson.Decode.decoder

val clicks : (position -> 'msg) -> 'msg BsOakCore.Sub.t

val moves : (position -> 'msg) -> 'msg BsOakCore.Sub.t

val downs : (position -> 'msg) -> 'msg BsOakCore.Sub.t

val ups : (position -> 'msg) -> 'msg BsOakCore.Sub.t