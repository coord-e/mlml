let ksprintf k ff = ff (fun s -> k s)
let sprintf = ksprintf (fun x -> x)
let printf = ksprintf (fun x -> print_string x)
