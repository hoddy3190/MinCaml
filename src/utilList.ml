
let repeat e n =
    let rec loop xs acc =
        if acc <= 0 then
            xs
        else
            loop (e :: xs) (acc - 1)
    in
    loop [] n
