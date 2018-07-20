module Decode = BsOakJson.Decode
module Fx = BsOakCore.Fx
module Scheduler = BsOakCore.Fx_scheduler
module Dict = Belt.Map.String
module Result = Belt.Result

(* ffi *)

external add_event_listener: string -> (Decode.value -> unit) -> unit = "addEventListener" [@@bs.val][@@bs.scope "document"]
external remove_event_listener: string -> (Decode.value -> unit) -> unit = "removeEventListener" [@@bs.val][@@bs.scope "document"]

let on_document event_name decoder to_task =
  let perform_task event =
    match Decode.decode_value decoder event with
    | Result.Ok msg -> 
      let _ = Scheduler.raw_spawn (to_task msg) in
      ()
    | Result.Error _ -> ()
  in

  Scheduler.binding (fun _cb -> 
    let () = add_event_listener event_name perform_task in
    fun _pid -> remove_event_listener event_name perform_task
  )

(* utils *)

let (&>) t1 t2 =
  Scheduler.and_then (fun _ -> t2) t1

let (>>) f g x =
  g (f x)

let fold_left fn acc dict =
  Dict.reduce dict acc (fun acc' k v ->  fn k v acc')

let merge left_step both_step right_step left_dict right_dict initial_result =
  let rec step_state r_key r_value (list, result) =
    match list with
    | [] -> 
      (list, right_step r_key r_value result)

    | (l_key, l_value) :: rest ->
      if l_key < r_key then
        step_state r_key r_value (rest, left_step l_key l_value result)
      
      else if l_key > r_key then
        (list, right_step r_key r_value result)

      else 
        (rest, both_step l_key l_value r_value result)
  in
  let leftovers, intermediate_result =
    fold_left step_state (Dict.toList left_dict, initial_result) right_dict
  in
  List.fold_left (fun result (k,v) -> left_step k v result ) intermediate_result leftovers
  
(* types *)

type position = 
  { x: int;
    y: int;
  }

let position = 
  Decode.map2 (fun x y  -> {x; y})
    (Decode.field "pageX" Decode.int)
    (Decode.field "pageY" Decode.int)

(* effects *)

let ctx = Fx.ctx ()

(* subscriptions *)

type 'msg my_sub
  = MySub of string * (position -> 'msg)

let clicks tagger =
  MySub ("click", tagger)
  |> Fx.subscription ctx

let moves tagger =
  MySub ("mousemove", tagger)
  |> Fx.subscription ctx

let downs tagger =
  MySub ("mousedown", tagger)
  |> Fx.subscription ctx

let ups tagger =
  MySub ("mouseup", tagger)
  |> Fx.subscription ctx

(* map *)

let sub_map fn (MySub (category, tagger)) =
  MySub (category, (tagger >> fn))

(* fx state *)

type 'msg watcher =
  { taggers: (position -> 'msg) list; 
    pid: BsOakCore.Platform.process_id;
  }

type 'msg state =
  'msg watcher Dict.t 

(* categorize subscriptions *)

type 'msg sub_dict = (position -> 'msg) list Dict.t

let categorize_help_help value maybe_values =
  match maybe_values with
  | None -> Some [value]
  | Some values -> Some (value :: values)
  
let rec categorize_help subs sub_dict =
  match subs with
  | [] -> sub_dict
  | MySub (category, tagger) :: rest ->
    Dict.update sub_dict category (categorize_help_help tagger)
    |> categorize_help rest
    
let categorize subs : 'msg sub_dict =
  categorize_help subs Dict.empty

(* fx manager *)

let init =
  Scheduler.succeed Dict.empty

type msg =
  { category: string;
    position: position;
  }

let on_effects router new_subs old_state : ('msg state, unit) Scheduler.task =
  let left_step _category watcher task =
    Scheduler.kill watcher.pid &> task
  in
  let both_step category watcher taggers task =
    task
    |> Scheduler.and_then (fun state -> Scheduler.succeed (Dict.set state category {taggers; pid = watcher.pid}))
  in
  let right_step category taggers task =
    let tracker =
      on_document category position (fun position -> Fx.send_to_self router {category; position})
    in
      task
      |> Scheduler.and_then (fun state -> Scheduler.spawn tracker 
      |> Scheduler.and_then (fun pid -> Scheduler.succeed ( Dict.set state category {taggers; pid})))
  in
    merge left_step both_step right_step old_state (categorize new_subs) (Scheduler.succeed Dict.empty)

let on_self_msg router {category; position} state =
  match Dict.get state category with
  | None ->
    Scheduler.succeed state
  
  | Some {taggers; _} ->
    let send tagger =
      Fx.send_to_app router (tagger position)
    in
    Scheduler.sequence (List.map send taggers)
      &> Scheduler.succeed state
     
let () = 
  Fx.sub_manager
  ~ctx: ctx
  ~init: init
  ~on_effects: on_effects
  ~on_self_msg: on_self_msg
  ~sub_map: sub_map