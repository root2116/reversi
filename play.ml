open Color
open Command

let standard_moves : string list = 
  let f = open_in "standard_moves.txt" in 
  let rec read moves_list index = 
    if index = 2013 then moves_list
    else
      let moves = input_line f in 
      read (moves::moves_list) (index+1)
  in read [] 0
    
let standard = ref standard_moves
let opposite_move = ref (0,0)

let set_opposite move = 
  match move with
  | Mv (i,j) -> opposite_move := (i,j) 
  | _ -> ()

type board = color array array
let board_value = 
  let value_table = Array.make_matrix 10 10 0 in
    let f = open_in "board_value.txt" in
    for i=0 to 9 do
      for j=0 to 9 do
        let value = input_line f in
        value_table.(i).(j) <- (int_of_string value)
      done
    done;
    value_table



let init_board () =
  let board = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then
      r
    else
      [] in
    f (di,dj) (i,j) []



let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with
      [] -> false
    | _  -> true

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j)


let doMove board com color =
  let copy = Array.map (fun z -> Array.copy z) board in
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> copy.(ii).(jj) <- color) ms in
	let _  = copy.(i).(j) <- color in
	  copy

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)


let valid_moves board color =
  let ls = [1;2;3;4;5;6;7;8] in
  List.filter (is_valid_move board color)
    (mix ls ls)

let eval_board_position board color = 
  let ocolor = opposite_color color in
  let rec eval i sum = 
    if i = 100 then 
     sum
    else 
      let cl = board.(i/10).(i mod 10) in
      if cl = color then 
        eval (i+1) (sum +. 3.0 *.(Random.float 1.0) *. float_of_int board_value.(i/10).(i mod 10))
      else if cl = ocolor then
        eval (i+1) (sum -. 3.0 *.(Random.float 1.0) *. float_of_int board_value.(i/10).(i mod 10))
      else eval (i+1) sum 
  in
    eval 0 0.0

let argmax xs = 
  let rec inner_argmax a max_index index max_value =
    match a with
    | [] -> max_index
    | x::xs -> if x > max_value then 
                 inner_argmax xs index (index+1) x 
               else 
                 inner_argmax xs max_index (index+1) max_value
    
  in 
     inner_argmax xs 0 0 (-10000.0)

let argsmax xs = 
  let rec inner_argsmax a max_idxs index max_value = 
    match a with
    | [] -> max_idxs
    | x::xs -> if x > max_value then 
                  inner_argsmax xs (index::[]) (index+1) x 
               else if x = max_value then 
                   inner_argsmax xs (index::max_idxs) (index+1) max_value 
               else
                  inner_argsmax xs max_idxs (index+1) max_value
  in
     inner_argsmax xs [] 0 (-1000000)


let rec print_moves ms =
  match ms with
  | [] -> print_newline ()
  | (i,j)::xs -> print_string "("; print_int i; print_string ","; print_int j; print_string ")"; print_newline (); print_moves xs  


let candidate_number board color = 
  let ms = valid_moves board color in
  List.length ms

(* 候補手の評価 *)
let eval_candidate_number board color = 
   let ocolor = opposite_color color in
   let value = (-1.0) *. (float_of_int (candidate_number board ocolor) +. (Random.float 1.0) *. 2.0 ) *. 10.0
     in value


(* 確定石の計算 *)
let fixed_stones line color =
   let rec none_exists xs =
     match xs with
     | [] -> false
     | y::ys -> if y = none then true else none_exists ys 
   in 
     if none_exists (Array.to_list line) then 
        let count xs color =
          let rec streak xs color sum = 
            match xs with
            | [] -> sum
            | y::ys -> if y = color then streak ys color (sum+1) else sum
          in 
            let streaks = streak (Array.to_list xs) color 0 
          in 
            if streaks = 8 then streaks else streaks + streak (List.rev (Array.to_list xs)) color 0
        in 
          count (Array.sub line 1 8) color
     else
       let rec count xs color sum = 
          match xs with
          | [] -> sum
          | y::ys -> if y = color then count ys color (sum+1) else count ys color sum
       in count (Array.to_list line) color 0

let get_column matrix n = 
  matrix.(n)

let get_row matrix n = 
  Array.map (fun row -> row.(n)) matrix 
let eval_fixed_stones board color = 
  let ocolor = opposite_color color in 
  let left = get_column board 1 in 
  let right = get_column board 8 in 
  let upper = get_row board 1 in 
  let bottom = get_row board 8 in
  let left_fs =  (fixed_stones left color) - (fixed_stones left ocolor)  in 
  let right_fs =  (fixed_stones right color) - (fixed_stones right ocolor)  in 
  let upper_fs =  (fixed_stones upper color) - (fixed_stones upper ocolor) in 
  let bottom_fs =  (fixed_stones bottom color) - (fixed_stones bottom ocolor) in 
  (float_of_int (left_fs + right_fs + upper_fs + bottom_fs) +. (Random.float 1.0) *. 3.0) *. 11.0





(* let eval_fixed_stones board color =  *)


let w_bp = 2.0
let w_cn = 1.0

let w_fs = 5.0

let eval_board board color = 
  let bp = w_bp *. eval_board_position board color in 
  let cn = w_cn *. eval_candidate_number board color in 
  let fs = w_fs *. eval_fixed_stones board color  in 
  bp +. cn +. fs 


let rec negamax board color depth passed = 
  let ocolor = opposite_color color in 
  if depth = 0 then eval_board board color 
  else
     let ms = valid_moves board color in 
     if ms = [] then 
        if passed then eval_board board color 
        else (-1.0) *. negamax board ocolor depth true
     else 
      let rec find_max xs max_score = 
        match xs with
        | [] -> max_score
        | (i,j)::ys -> 
           let next_board = doMove board (Mv (i,j)) color  
        in let score = (-1.0) *. negamax next_board ocolor (depth - 1) false
        in if max_score < score then 
             find_max ys score
            else find_max ys max_score
      in 
        find_max ms (-10000000000.0)


let rec alpha_beta board color alpha beta depth passed = 
  let ocolor = opposite_color color in 
  if depth = 0 then eval_board board color 
  else 
    let ms = valid_moves board color in 
    if ms = [] then 
      if passed then eval_board board color 
      else (-1.0) *. alpha_beta board ocolor ((-1.0)*.beta) ((-1.0)*.alpha) depth true
    else 
      let rec find_max xs alpha max_score = 
        match xs with
        | [] -> max_score
        | (i,j)::ys -> 
           let next_board = doMove board (Mv (i,j)) color 
        in let score = (-1.0) *. alpha_beta next_board ocolor ((-1.0)*.beta) ((-1.0)*.alpha) (depth - 1) false 
        in if score >= beta then score
        else if score > alpha then 
            if score > max_score then find_max ys score score
            else find_max ys score max_score
        else find_max ys alpha max_score
     in find_max ms alpha (-10000000.0)





let count board color =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s


let judge board color = 
  let ocolor = opposite_color color in 
  let color_n = count board color in 
  let ocolor_n = count board ocolor in 
  if color_n > ocolor_n then 1 
  else if color_n < ocolor_n then -1 
  else 0
     


let rec complete_analysis board my_color color passed = 
  let ocolor = opposite_color color in
  let ms = valid_moves board color in 
  if ms = [] then 
    if passed then judge board my_color 
    else complete_analysis board my_color ocolor true
  else 
    let rec my_turn_check xs lose = 
      match xs with
      | [] -> if lose then -1 else 0
      | (i,j)::ys -> 
        let new_board = doMove board (Mv (i,j)) color in 
        let res = complete_analysis new_board my_color ocolor false in 
        if res = 1 then 1 
        else if res = 0 then my_turn_check ys false
        else my_turn_check ys lose 
    in let rec op_turn_check xs win = 
      match xs with
      | [] -> if win then 1 else 0
      | (i,j)::ys -> 
         let new_board = doMove board (Mv (i,j)) color in 
         let res = complete_analysis new_board my_color ocolor false in 
         if res = 1 then op_turn_check ys win
         else if res = -1 then -1 
         else op_turn_check ys false
    in 
      if color = my_color then my_turn_check ms true 
      else op_turn_check ms true

  




let rest board = 
  let black_num = count board black in 
  let white_num = count board white in 
  64 - (black_num + white_num)

(* let play board color =
  let ocolor = opposite_color color in 
  let ms = valid_moves board color in
  (* print_moves ms; *)
    if ms = [] then
      Pass
    else
      let next_boards = List.map (fun (i,j) -> doMove board (Mv (i,j)) color) ms in 
      if rest board <= 12 then 
        
        let next_judge = List.map (fun board -> complete_analysis board color ocolor false) next_boards in 
        print_string "next_judge: ";
        List.iter (Printf.printf "%d ") next_judge;
        print_string "\n";
        let ks = argsmax next_judge in 
        print_string "ks: ";
        List.iter (Printf.printf "%d ") ks;
        print_string "\n";
        let rec find_max xs max_index max = 
          match xs with
          | [] -> max_index
          | y::ys -> let next_board = List.nth next_boards y in 
                     let score = (-1.0) *. negamax next_board ocolor 3 false in 
                     if score > max then find_max ys y score 
                     else find_max ys max_index max 
        in
        let max_index = find_max ks 0 (-10000000.0) in
        print_string "max_index: ";
        Printf.printf "%d" max_index;
        print_string "\n"; 
        let (i,j) = List.nth ms max_index in 
        Mv (i,j)
      else 
        let next_values = List.map (fun board -> (-1.0) *. negamax board ocolor 5 false) next_boards in
        let k = argmax next_values in 
        let (i,j) = List.nth ms k in 
	      Mv (i,j) *)

let f5 = 0 
let e6 = 1 
let c4 = 2 
let d3 = 3 

let mode = ref f5 
let convert (i,j) = 
  if !mode = f5 then (i,j)
  else if !mode = e6 then (j,i)
  else if !mode = c4 then (9-i,9-j) 
  else (9-j,9-i) 

let choose_standard (i,j) = 
  let (i',j') = convert (i,j) in
  let move_str = string_of_move (Mv (i',j')) in 
  let count_table = Array.make_matrix 10 10 0 in
  let rec filter xs res prefix = 
    match xs with
    | [] -> res
    | moves::rest -> 
      if String.equal prefix (String.sub moves 0 2) then
          let trimmed_moves = String.sub moves 2 (String.length moves - 2) in 
          let x_ch = trimmed_moves.[0] in 
          let y_ch = trimmed_moves.[1] in
          let x = int_of_char x_ch - int_of_char 'A' + 1 in 
          let y = int_of_char y_ch - int_of_char '1' + 1 in 
          let _ = count_table.(x).(y) <- count_table.(x).(y) + 1 in 
         filter rest ((String.sub moves 2 (String.length moves - 2)) :: res)  prefix
      else 
         filter rest res prefix
  in
    standard := filter !standard [] move_str;
    if !standard = [] then 
      None
    else
    let max = ref 0 in 
    let max_index = ref (0,0) in 
    for i=0 to 9 do 
      for j=0 to 9 do 
        if !max < count_table.(i).(j) then 
          let _ = max := count_table.(i).(j) in 
          max_index := (i,j) 
      done
    done;
    let (x,y) = !max_index in
    let (x',y') = convert (x,y) in
    standard := filter !standard [] (string_of_move (Mv (x,y)));
     Some (Mv (x',y'))

      
       
(* 現在のターン数 0-indexed *)
let turn board = 
  let black_n = count board black in 
  let white_n = count board white in 
  black_n + white_n - 4


let set_mode board = 
    if turn board = 1 then 
      if !opposite_move = (6,5) then mode := f5 
      else if !opposite_move = (5,6) then mode := e6 
      else if !opposite_move = (3,4) then mode := c4 
      else mode := d3
    else ()


    
let play board color =
  let ocolor = opposite_color color in 
  let rec find_max xs alpha index max_index = 
          match xs with
          | [] -> max_index
          | (i,j)::ys -> 
              let next_board = doMove board (Mv (i,j)) color in 
              let score = (-1.0) *. alpha_beta next_board ocolor (-100000000.0) ((-1.0)*.alpha) 6 false in 
              if score > alpha then find_max ys score (index+1) index
              else find_max ys alpha (index+1) max_index
  in
  let ms = valid_moves board color in
    if ms = [] then
      Pass
    else
      if turn board = 0 then 
        Mv (6,5) (* 自分が先手ならF5から始める *)
      else if turn board < 20 then 
        let _ = set_mode board in (* 1ターン目でモードを設定 *)
        (match choose_standard !opposite_move with 
         | None ->  let k = find_max ms (-1000000000.0) 0 0 in
                    let (i,j) = List.nth ms k in 
                    Mv (i,j)
         | Some v -> v)
      else if rest board <= 12 then 
        let next_boards = List.map (fun (i,j) -> doMove board (Mv (i,j)) color) ms in 
        let next_judge = List.map (fun board -> complete_analysis board color ocolor false) next_boards in 
        print_string "next_judge: ";
        List.iter (Printf.printf "%d ") next_judge;
        print_string "\n";
        let ks = argsmax next_judge in 
        print_string "ks: ";
        List.iter (Printf.printf "%d ") ks;
        print_string "\n";
        let rec find_max xs max_index max = 
          match xs with
          | [] -> max_index
          | y::ys -> let next_board = List.nth next_boards y in 
                     let score = (-1.0) *. negamax next_board ocolor 3 false in 
                     if score > max then find_max ys y score 
                     else find_max ys max_index max 
        in
        let max_index = find_max ks 0 (-10000000.0) in
        print_string "max_index: ";
        Printf.printf "%d" max_index;
        print_string "\n"; 
        let (i,j) = List.nth ms max_index in 
        Mv (i,j)
      else 
        let k = find_max ms (-1000000000.0) 0 0 in
        let (i,j) = List.nth ms k in 
	      Mv (i,j)

(* let play board color = 
  let _ = board in 
  let _ = color in 
  let input = read_line () in 
  let x_ch = input.[0] in 
  let y_ch = input.[1] in
  let x = int_of_char x_ch - int_of_char 'A' + 1 in 
  let y = int_of_char y_ch - int_of_char '1' + 1 in 
   Mv (x,y)

 *)




let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (board.(i).(j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board
