type info = {
  apiversion : string;
  author : string;
  color : string;
  head  : string;
  tail  : string;
} [@@deriving yojson]

let info _ = 
  {
    apiversion = "1";
    author = "";
    color = "#c0ffee";
    head = "default";
    tail = "default";
  }
  |> info_to_yojson 
  |> Yojson.Safe.to_string
  |> Dream.json
;;
(* let on_start _ = None;;
let on_end _ = None;;
let on_move _ = None;; *)
