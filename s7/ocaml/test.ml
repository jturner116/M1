let rec forall p n =
        if n = 0 then true
        else p n && forall p (n - 1)
