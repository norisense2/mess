open Curses

let get_max_y () = getmaxyx (stdscr ()) |> fst |> pred

let drop_last (target: string): string =
    String.sub target 0 ((String.length target)-1)

let prompt msg =
    move (get_max_y ()) 0 |> ignore;
    deleteln () |> ignore;
    Printf.sprintf "%s: " msg |> addstr |> ignore;
    echo () |> ignore;
    let rec inner text =
        match getch() |> keyname with
        | "^M" -> text
        | "^[" -> ""
        | "KEY_BACKSPACE" -> begin
            try
                delch () |> ignore;
                inner (drop_last text)
            with Invalid_argument _ ->
                addstr " " |> ignore;
                inner ""
            end
        | key -> inner (text^key)
    in
    let result = inner "" in
    noecho () |> ignore;
    result

let show (buf: string list) =
    move 0 0 |> ignore;
    let rec show_line buf offset max_y =
        if offset > (max_y-1) then
            ()
        else begin
            match buf with
            | [] -> ()
            | head :: tail -> begin
                addstr (head^"\n") |> ignore;
                show_line tail (offset+1) max_y end
        end
    in
    show_line buf 0 (get_max_y ())

let show_percentage percentage =
    move (get_max_y ()) 0 |> ignore;
    deleteln () |> ignore;
    percentage |> string_of_int |> addstr |> ignore;
    addstr "%" |> ignore

let get_q (): string =
    prompt "text"

let get_line (): int option =
    let got_input = prompt "line" in
    let line =
        try Some (got_input |> int_of_string)
        with Failure _ -> None
    in
    line
